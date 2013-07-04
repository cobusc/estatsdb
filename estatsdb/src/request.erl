-module(request).
-export([build/1]).
-include("estatsdb.hrl").

-type proplist(A, B) :: list({A,B}).


-spec build(QueryArgs::proplist(string(), string())) -> 
    {ok, #request_info{}} |
    {error, Reason::binary()}.

build(QueryArgs) ->
    case proplists:lookup("tablename", QueryArgs) of
        none ->
            {error, "tablename argument not specified"};
        {"tablename", TableName} ->
            case schema_server:lookup(TableName) of
                none ->
                    {error, "Table '"++TableName++"' is not known to estatsdb. Maybe refresh the schema server?"};
                {value, #table_info{columns=Cols}=TableInfo} ->
                    SuppliedCols = ordsets:from_list([list_to_binary(N) || {N, _} <- QueryArgs, N =/= "tablename"]),
                    Pks = ordsets:from_list([ C || #schema_column{name=C, is_pk=true} <- Cols]), 
                    case ordsets:is_subset(Pks, SuppliedCols) of
                        false ->
                            {error, "Some primary key column is missing"};
                        true ->
                            % Check for unknown columns
                            AvailableCols = ordsets:from_list([N || #schema_column{name=N} <- Cols]),
                            case ordsets:is_subset(SuppliedCols, AvailableCols) of
                                false ->
                                    {error, "Unknown columns specified."};
                                true ->
                                    Map = fun({K,V}) ->
                                        Name = list_to_binary(K),
                                        Value = list_to_binary(V), % @todo Translate
                                        IsPk = lists:member(Name, Pks),
                                        #request_column{name=Name, value=Value, is_pk=IsPk}
                                    end,
                                    Columns = lists:map(Map, [ E || {N, _}=E <- QueryArgs, N =/= "tablename"]),
                                    ReqInfo = #request_info{
                                        table_name = TableName,
                                        table_info = TableInfo,
                                        columns = Columns
                                    },
                                    {ok, ReqInfo}
                            end
                    end
            end
    end.
