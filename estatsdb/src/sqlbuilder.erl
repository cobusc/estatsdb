-module(sqlbuilder).
-export([% Functions used by the web resources
         set_sql/1,
         update_sql/1,
         select_sql/1,

         % Functions used by the schema server
         build_returning_sql/1,
         build_as_sql/1
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("estatsdb.hrl").

-spec build_set_sql(ReqInfo::#request_info{}) -> iodata().

build_set_sql(#request_info{table_info=TableInfo, columns=Cols}) ->
    Table = TableInfo#table_info.table_name,
    ["INSERT INTO ", Table, " (", string:join([ binary_to_list(N) || #column{name=N} <- Cols], ", "), 
     ") VALUES (", string:join([ io_lib:format("'~s'::~s", [V, T]) || #column{value=V,type=T} <- Cols], ", "), ")"].


-spec build_overwrite_sql(ReqInfo::#request_info{}) -> iodata().

build_overwrite_sql(#request_info{table_info=TableInfo, columns=Cols}) ->
    Table = TableInfo#table_info.table_name,
    ["UPDATE ", Table,
     " SET ", string:join([io_lib:format("~s = '~s'::~s", [N, V, T]) || #column{name=N,value=V,type=T,is_pk=false} <- Cols], ", "),
     " WHERE ", string:join([io_lib:format("~s = '~s'::~s", [N, V, T]) || #column{name=N,value=V,type=T,is_pk=true} <- Cols], " AND ")].


-spec build_update_sql(ReqInfo::#request_info{}) -> iodata().

build_update_sql(#request_info{table_info=TableInfo, columns=Cols}) ->
    Table = TableInfo#table_info.table_name,
    ["UPDATE ", Table,
     " SET ", string:join([io_lib:format("~s = COALESCE(~s, 0) + '~s'::~s", [N, N, V, T]) || #column{name=N,value=V,type=T,is_pk=false} <- Cols], ", "),
    " WHERE ", string:join([io_lib:format("~s = '~s'::~s", [N, V, T]) || #column{name=N,value=V,type=T,is_pk=true} <- Cols], " AND ")].
     

-spec build_returning_sql(Cols::list(#column{})) -> iodata().

build_returning_sql(Cols) ->
    % "RETURNING a,b,c,d"
    "RETURNING "++string:join([ binary_to_list(N) || #column{name=N} <- Cols ], ", ").


-spec build_as_sql(Cols::list(#column{})) -> iodata().

build_as_sql(Cols) ->
    % "AS (hour TIMESTAMP, host TEXT, metric1 INTEGER, metric2 FLOAT, metric3 BIGINT)"
    "AS ("++string:join([ io_lib:format("~s ~s", [N, T]) || #column{name=N, type=T} <- Cols ], ", ")++")".


-spec set_sql(ReqInfo::#request_info{}) -> iodata().

set_sql(#request_info{table_info=TableInfo}=ReqInfo) ->
    #table_info{returning_sql=ReturningSql,as_sql=AsSql} = TableInfo,
    SetSql = build_set_sql(ReqInfo),
    OverwriteSql = build_overwrite_sql(ReqInfo),
    ["SELECT * FROM set_helper($$", SetSql, "$$, $$", OverwriteSql, "$$,$$", ReturningSql, "$$) ", AsSql].

-spec update_sql(ReqInfo::#request_info{}) -> iodata().

update_sql(#request_info{table_info=TableInfo}=ReqInfo) ->
    #table_info{returning_sql=ReturningSql,as_sql=AsSql} = TableInfo,
    UpdateSql = build_update_sql(ReqInfo),
    SetSql = build_set_sql(ReqInfo),
    ["SELECT * FROM update_helper($$", UpdateSql, "$$, $$", SetSql, "$$,$$", ReturningSql, "$$) ", AsSql].

-spec select_sql(ReqInfo::#request_info{}) -> iodata().

select_sql(#request_info{table_info=TableInfo, columns=Cols}) ->
    TableName = TableInfo#table_info.table_name,
    ["SELECT ", string:join([ binary_to_list(N) || #column{name=N} <- Cols], ", "), 
     " FROM ", TableName,
     " WHERE ", string:join([io_lib:format("~s = '~s'::~s", [N, V, T]) || #column{name=N,value=V,type=T,is_pk=true} <- Cols], " AND ")].

