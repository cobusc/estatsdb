-module(dbutils).
-export([
    transduce/2,

    equery/2, equery/3,
    squery/1, squery/2,
    
    get_connection/0, get_connection/1,
    return_connection/1, return_connection/2
]).
-include_lib("epgsql/include/pgsql.hrl").

-define(POOL, estatsdb).

%% 
%% @doc Return a list of proplists representing the Columns and Rows supplied
%%
-spec transduce(Colums::[#column{}], Rows::[tuple()]) -> [list({binary(), any()})].

transduce(_Colums, []) ->
    [];
transduce(Columns, Rows) ->
    ColumnNames = [ C#column.name || C <- Columns ],
    [ lists:zip(ColumnNames, erlang:tuple_to_list(Row)) || Row <- Rows].

%%
%% @doc Wrapper for pgsql:equery/3
%%
%% It also catches exotic errors and try to handle it in a sane way.
%%
-spec equery(Sql::iolist()|binary(), Params::[any()]) ->  {ok, Columns::[any()], Rows::[any()]} |
                                                 {ok, Count::non_neg_integer()} |
                                                 {ok, Count::non_neg_integer(), Columns::[any()], Rows::[any()]} |
                                                 {error, Error::any()}.

equery(Sql, Params) ->
    equery(Sql, Params, ?POOL).


-spec equery(Sql::iolist()|binary(), Params::[any()], Pool::atom()) ->  {ok, Columns::[any()], Rows::[any()]} |
                                                 {ok, Count::non_neg_integer()} |
                                                 {ok, Count::non_neg_integer(), Columns::[any()], Rows::[any()]} |
                                                 {error, Error::any()}.
equery(Sql, Params, Pool) ->
    case dbutils:get_connection(Pool) of
        {ok, Con} -> 
            case pgsql:equery(Con, Sql, Params) of
                {error, #error{message=M}} = E -> % Standard error. Log and pass on.
                    ok = error_logger:info_msg("PSQL ERROR: ~s", [M]),
                    ok = dbutils:return_connection(Con, Pool),
                    E;
                {error, closed} = E -> % What to do, what to do...can't hand it back to the pool, that's for sure.
                    ok = error_logger:info_msg("PSQL ERROR: closed", []),
                    E; % Pass back to caller and trash Con
                {error, sync_required} = E -> % According to docs "Error occured and pgsql:sync must be called"
                    ok = error_logger:info_msg("PSQL ERROR: sync_required", []),
                    ok = pgsql:sync(Con), % Hopefully this fixes things
                    ok = dbutils:return_connection(Con, Pool), % Return to pool
                    E; % Pass back to caller

                Result -> 
                    ok = dbutils:return_connection(Con, Pool), 
                    Result
            end;
        Other -> Other
    end.

%%
%% @doc Wrapper for pgsql:squery/2
%%
%% It also catches exotic errors and try to handle it in a sane way.
%% Important: squery does not map results to Erlang types, as equery does.
%%
-spec squery(Sql::iolist()|binary()) ->  {ok, Columns::[any()], Rows::[any()]} |
                                {ok, Count::non_neg_integer()} |
                                {ok, Count::non_neg_integer(), Columns::[any()], Rows::[any()]} |
                                {error, Error::any()}.

squery(Sql) ->
    squery(Sql, ?POOL).

-spec squery(Sql::iolist()|binary(), Pool::atom()) ->  {ok, Columns::[any()], Rows::[any()]} |
                                {ok, Count::non_neg_integer()} |
                                {ok, Count::non_neg_integer(), Columns::[any()], Rows::[any()]} |
                                {error, Error::any()}.
squery(Sql, Pool) ->
    case dbutils:get_connection(Pool) of
        {ok, Con} -> 
            case pgsql:squery(Con, Sql) of
                {error, #error{message=M}} = E -> % Standard error. Log and pass on.
                    ok = error_logger:info_msg("PSQL ERROR: ~s", [M]),
                    ok = dbutils:return_connection(Con, Pool),
                    E;
                {error, closed} = E -> % What to do, what to do...can't hand it back to the pool, that's for sure.
                    ok = error_logger:info_msg("PSQL ERROR: closed", []),
                    E; % Pass back to caller and trash Con
                {error, sync_required} = E -> % According to docs "Error occured and pgsql:sync must be called"
                    ok = error_logger:info_msg("PSQL ERROR: sync_required", []),
                    ok = pgsql:sync(Con), % Hopefully this fixes things
                    ok = dbutils:return_connection(Con, Pool), % Return to pool
                    E; % Pass back to caller

                Result -> 
                    ok = dbutils:return_connection(Con, Pool), 
                    Result
            end;
        Other -> Other
    end.

%%
%% @doc Get a database connection from the default pool.
%%
%% The default retry count is 5.
%% The default timeout is 1000ms.
%%
-spec get_connection() -> {ok, Pid::pid()} | {error, any()}.

get_connection() ->
    get_connection(?POOL).

-spec get_connection(Pool::atom()) -> {ok, Pid::pid()} | {error, any()}.

get_connection(Pool) ->
    DefaultRetryCount = 5,
    % DefaultTimeout specified in POOL configuration.
    DefaultTimeout = 1000,
    get_connection_helper(Pool, DefaultRetryCount, DefaultTimeout).


get_connection_helper(Pool, 0, Timeout) ->
    pgsql_pool:get_connection(Pool, Timeout);
get_connection_helper(Pool, RetryCount, Timeout)
when is_integer(Timeout),
     is_integer(RetryCount),
     RetryCount > 0 ->

    case pgsql_pool:get_connection(Pool, Timeout) of
        {error, timeout} -> get_connection_helper(Pool, RetryCount-1, Timeout);
        Other -> Other
    end.

%%
%% @doc Return a connection to the default pool.
%%
-spec return_connection(Con::pid()) -> ok.

return_connection(Con) ->
    return_connection(Con, ?POOL).

-spec return_connection(Con::pid(), Pool::atom()) -> ok.

return_connection(Con, Pool) ->
    pgsql_pool:return_connection(Pool, Con).


