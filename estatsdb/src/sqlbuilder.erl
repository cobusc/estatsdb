-module(sqlbuilder).
-export([set_sql/1,
         update_sql/2
        ]).
-include("estatsdb.hrl").

-spec build_set_sql(ReqInfo::#request_info{}) -> iodata().

build_set_sql(#request_info{table_name=T, columns=Cols}) ->
    ["INSERT INTO ", T, "(", string:join([ binary_to_list(Name) || #request_column{name=Name} <- Cols], ", "), 
     ") VALUES (", string:join([ binary_to_list(Value) || #request_column{value=Value} <- Cols], ", "), ")"].


-spec build_overwrite_sql(ReqInfo::#request_info{}) -> iodata().

build_overwrite_sql(#request_info{table_name=T, columns=Cols}) ->
    ["UPDATE ", T,
     " SET ", string:join([io_lib:format("~s = ~s", [Name, Value]) || #request_column{name=Name,value=Value,is_pk=false} <- Cols], ", "),
     " WHERE ", string:join([io_lib:format("~s = ~s", [Name, Value]) || #request_column{name=Name,value=Value,is_pk=true} <- Cols], " AND ")].


-spec build_update_sql(ReqInfo::#request_info{}) -> iodata().

build_update_sql(#request_info{table_name=T, columns=Cols}) ->
    ["UPDATE ", T,
     " SET ", string:join([io_lib:format("~s = COALESCE(~s, 0) + ~s", [Name, Name, Value]) || #request_column{name=Name,value=Value,is_pk=false} <- Cols], ", "),
    " WHERE ", string:join([io_lib:format("~s = ~s", [Name, Value]) || #request_column{name=Name,value=Value,is_pk=true} <- Cols], " AND ")].
     

-spec build_returning_sql(TableInfo::#table_info{}) -> iodata().

build_returning_sql(#table_info{columns=Cols}) ->
    % "RETURNING a,b,c,d"
    "RETURNING "++string:join([ binary_to_list(Name) || #schema_column{name=Name} <- Cols ], ", ").


-spec build_as_sql(TableInfo::#table_info{}) -> iodata().

build_as_sql(#table_info{columns=Cols}) ->
    % "AS (hour TIMESTAMP, host TEXT, metric1 INTEGER, metric2 FLOAT, metric3 BIGINT)"
    "AS ("++string:join([ io_lib:format("~s ~s", [Name, Type]) || #schema_column{name=Name, type=Type} <- Cols ], ", ")++")".

-spec set_sql(ReqInfo::#request_info{}) -> iodata().

set_sql(ReqInfo) ->
    ReturningSql = build_returning_sql(ReqInfo#request_info.table_info),
    AsSql = build_as_sql(ReqInfo#request_info.table_info),
    SetSql = build_set_sql(ReqInfo),
    OverwriteSql = build_overwrite_sql(ReqInfo),
    ["SELECT * FROM set_helper($$", SetSql, " ", ReturningSql, "$$, $$", OverwriteSql, " ", ReturningSql, "$$) AS ", AsSql].

-spec update_sql(ReqInfo::#request_info{}, TableInfo::#table_info{}) -> iodata().

update_sql(ReqInfo, TableInfo) ->
    ReturningSql = build_returning_sql(TableInfo),
    AsSql = build_as_sql(TableInfo),
    UpdateSql = build_update_sql(ReqInfo),
    SetSql = build_set_sql(ReqInfo),
    ["SELECT * FROM update_helper($$", UpdateSql, " ", ReturningSql, "$$, $$", SetSql, " ", ReturningSql, "$$) ", AsSql].

%% Example of setting a metric value:
%%
%% SELECT *
%%   FROM set_helper(
%%      'insert into hourly_example_stats(hour, host, metric1) values (''2013-01-01T00:'', ''test'', 1) RETURNING ...', 
%%      'update hourly_example_stats set metric1=1 where hour=''2013-01-01T00:'' and host=''test'' RETURNING ...') 
%%     AS (hour TIMESTAMP, host TEXT, metric1 INTEGER, metric2 FLOAT, metric3 BIGINT);
%%     

%% Example of updating a metric value:

%% SELECT *
%%   FROM update_helper('update hourly_example_stats set metric1=1 where hour=''2013-01-01T00:'' and host=''test'' RETURNING ...',
%%                      'insert into hourly_example_stats(hour, host, metric1) values (''2013-01-01T00:'', ''test'', 1) RETURNING ...') 
%%     AS (hour TIMESTAMP, host TEXT, metric1 INTEGER, metric2 FLOAT, metric3 BIGINT);
