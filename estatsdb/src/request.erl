-module(request).
-export([build/1,
         sanitize_response_for_json/1
        ]).
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
            case schema_server:lookup(list_to_binary(TableName)) of
                none ->
                    {error, "Table '"++TableName++"' is not known to estatsdb. Maybe refresh the schema server?"};
                {value, #table_info{columns=Cols}=TableInfo} ->
                    SuppliedCols = ordsets:from_list([list_to_binary(N) || {N, _} <- QueryArgs, N =/= "tablename"]),
                    Pks = ordsets:from_list([ C || #column{name=C, is_pk=true} <- Cols]), 
                    case ordsets:is_subset(Pks, SuppliedCols) of
                        false ->
                            {error, "Some primary key column is missing"};
                        true ->
                            % Check for unknown columns
                            AvailableCols = ordsets:from_list([N || #column{name=N} <- Cols]),
                            case ordsets:is_subset(SuppliedCols, AvailableCols) of
                                false ->
                                    {error, "Unknown columns specified."};
                                true ->
                                    Map = fun({K,V}) ->
                                        [#column{}=C] = lists:filter(fun (#column{name=N}) -> N == list_to_binary(K) end, Cols),
                                        C#column{value = list_to_binary(V)}
                                    end,
                                    Columns = lists:map(Map, [ E || {N, _}=E <- QueryArgs, N =/= "tablename"]),
                                    ReqInfo = #request_info{
                                        table_info = TableInfo,
                                        columns = Columns
                                    },
                                    {ok, ReqInfo}
                            end
                    end
            end
    end.

sanitize_response_for_json(PropList) ->
    Sanitize = fun ({K,V}) ->
        {K, sanitize_value_for_json(V)}
    end,
    lists:map(Sanitize, PropList).

sanitize_value_for_json({Y,M,D}=Date)
when is_integer(Y), is_integer(M), is_integer(D) ->
    list_to_binary(pgdb_tools:date_to_string(Date));

sanitize_value_for_json({{Y,Mo,D},{H,Mi,S}}=Timestamp)
when is_integer(Y), is_integer(Mo), is_integer(D),
     is_integer(H), is_integer(Mi), is_float(S) ->
    list_to_binary(pgdb_tools:timestamp_to_string(Timestamp));

sanitize_value_for_json(Other) ->
    Other.
