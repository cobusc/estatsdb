-module(schema_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,

         lookup/1,
         refresh/0,
         describe_table/1
        ]).

-record(state, {table_info :: gb_tree()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec lookup(TableName::string()) -> none | {value, Val::any()}.

lookup(TableName) ->
    gen_server:call(?MODULE, {lookup, TableName}, infinity).


-spec refresh() -> ok.

refresh() ->
    TableInfo = load_schemas(),
    gen_server:call(?MODULE, {refresh, #state{table_info=TableInfo}}, infinity).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    TableInfo = load_schemas(),
    {ok, #state{table_info=TableInfo}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({lookup, TableName}, _From, State) ->
    Reply = gb_trees:lookup(TableName, State#state.table_info),
    {reply, Reply, State};

handle_call({refresh, NewState}, _From, _State) ->
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec load_schemas() -> gb_tree().

load_schemas() ->
    Schemas = get_schemas(),

    io:format("Schemas: ~p~n", [Schemas]),
    
    Tables = lists:foldl(fun (Schema, Acc) -> get_tables(Schema) ++ Acc end, [], Schemas),

    io:format("Tables: ~p~n", [Tables]),

    lists:foldl(fun (Table, GbTree) -> gb_trees:insert(Table, describe_table(Table), GbTree) end,
                gb_trees:empty(), Tables).

-spec get_schemas() -> list(string()).

get_schemas() ->
    case application:get_env(estatsdb, schemas) of
        undefined ->
            ["public"];
        {ok, Schemas} ->
            Schemas
    end.

-spec get_tables(Schema::string()) -> list(string()).

get_tables(Schema) ->
    % SQL used to select tables in a particular schema
    SelectSql = "SELECT $1::text||'.'||table_name AS table_name
                   FROM information_schema.tables 
                  WHERE table_schema=$1",

    case dbutils:equery(SelectSql, [Schema]) of
        {ok, _Cols, Rows} ->
            lists:map(fun ({X}) -> binary_to_list(X) end, Rows)
    end.

-spec describe_table(TableName::string()) -> tuple(). 

describe_table(FullTableName) ->
    io:format("Loading schema for ~s...~n", [FullTableName]),
    TableName = tl(string:tokens(FullTableName, ".")),
    % Tables must have a primary key for this to work...which is a good thing.
    % Returns column name, data type, is_pk
    Sql = "SELECT pg_attribute.attname AS column_name,
                  FORMAT_TYPE(pg_attribute.atttypid, pg_attribute.atttypmod) AS format_type,
                  pg_attribute.attnum = ANY(pg_index.indkey) AS is_pk
             FROM pg_attribute,
                  pg_index
            WHERE pg_attribute.attrelid = $1::regclass
              AND pg_index.indrelid = pg_attribute.attrelid
              AND pg_index.indisprimary
              AND FORMAT_TYPE(pg_attribute.atttypid, pg_attribute.atttypmod) NOT IN ('oid', 'cid', 'xid', 'tid')",

    case dbutils:equery(Sql, [TableName]) of
        {ok, Cols, Rows} ->
            dbutils:transduce(Cols, Rows)
    end.

