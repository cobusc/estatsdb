-module(request).
-export([build/1]).
-include("estatsdb.hrl").

-type proplist(A, B) :: list({A,B}).


-spec build(QueryArgs::proplist(string(), string())) -> 
    {ok, #request_info{}} |
    {error, Reason::string()}.
                                                        
build(QueryArgs) ->
    case proplists:lookup("tablename", QueryArgs) of
        none ->
            {error, "tablename argument not specified"};
        {"tablename", TableName} ->
            QueryArgsSansTableName = [{K, V} || {K, V} <- QueryArgs, K =/= "tablename"],
            build_for_table(TableName, QueryArgsSansTableName)
    end.


-spec build_for_table(TableName::string(), QueryArgsSansTableName::proplist(string(), string())) ->
     {ok, #request_info{}} |
     {error, Reason::string()}.

build_for_table(TableName, QueryArgsSansTableName) ->
    case schema_server:lookup(list_to_binary(TableName)) of
        none ->
            {error, "Table '"++TableName++"' is not known to estatsdb. Maybe refresh the schema server?"};
        {value, #table_info{}=TableInfo} ->
            build_if_pks_supplied(TableInfo, QueryArgsSansTableName)
    end.

-spec build_if_pks_supplied(TableInfo::#table_info{}, QueryArgsSansTableName::proplist(string(), string())) ->
    {ok, #request_info{}} |
    {error, Reason::string()}.

build_if_pks_supplied(#table_info{columns=Cols}=TableInfo, QueryArgsSansTableName) ->
    SuppliedCols = ordsets:from_list(lists:map(fun ({K, _}) -> list_to_binary(K) end, QueryArgsSansTableName)),
    Pks = ordsets:from_list([ C || #column{name=C, is_pk=true} <- Cols]), 
    case ordsets:is_subset(Pks, SuppliedCols) of
        false ->
            {error, "Some primary key column is missing"};
        true ->
            build_if_no_unknown_columns(TableInfo, QueryArgsSansTableName)
    end.


-spec build_if_no_unknown_columns(TableInfo::#table_info{}, QueryArgsSansTableName::proplist(string(), string())) ->
        {ok, #request_info{}} |
        {error, Reason::string()}.

build_if_no_unknown_columns(#table_info{columns=Cols}=TableInfo, QueryArgsSansTableName) ->
    SuppliedCols = ordsets:from_list(lists:map(fun ({K, _}) -> list_to_binary(K) end, QueryArgsSansTableName)),
    AvailableCols = ordsets:from_list([N || #column{name=N} <- Cols]),
    case ordsets:is_subset(SuppliedCols, AvailableCols) of
        false ->
            {error, "Unknown columns specified."};
        true ->
            Map = fun({K,V}) ->
                [#column{}=C] = lists:filter(fun (#column{name=N}) -> N == list_to_binary(K) end, Cols),
                C#column{value = list_to_binary(V)}
            end,
            Columns = lists:map(Map, QueryArgsSansTableName),
            ReqInfo = #request_info{
                table_info = TableInfo,
                columns = Columns
            },
            {ok, ReqInfo}
    end.
