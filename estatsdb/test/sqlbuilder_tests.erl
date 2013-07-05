-module(sqlbuilder_tests).
-include_lib("eunit/include/eunit.hrl").
-include("estatsdb.hrl").

get_test_table_columns() ->
    [#column{name = <<"id">>, type = <<"integer">>, is_pk = true},
     #column{name = <<"host">>, type = <<"text">>, is_pk = true},
     #column{name = <<"day">>, type = <<"date">>, is_pk = true},
     #column{name = <<"metric1">>, type = <<"bigint">>, is_pk = false},
     #column{name = <<"metric2">>, type = <<"double precision">>, is_pk = false}
    ].

get_test_request_columns() ->
    [#column{name = <<"id">>, type = <<"integer">>, is_pk = true, value = <<"1">>},
     #column{name = <<"host">>, type = <<"text">>, is_pk = true, value = <<"test_host">>},
     #column{name = <<"day">>, type = <<"date">>, is_pk = true, value = <<"2013-01-01">>},
     #column{name = <<"metric1">>, type = <<"bigint">>, is_pk = false, value = <<"123">>},
     #column{name = <<"metric2">>, type = <<"double precision">>, is_pk = false, value = <<"123.456">>}
    ].

get_test_table() ->
    #table_info{table_name = <<"unit.test">>, columns = get_test_table_columns()}.

get_test_request() ->
    #request_info{table_info = get_test_table(), columns = get_test_request_columns()}.


build_set_sql_test() ->
    ?assertEqual("INSERT INTO unit.test (id, host, day, metric1, metric2) VALUES ('1'::integer, 'test_host'::text, '2013-01-01'::date, '123'::bigint, '123.456'::double precision)",
                 lists:flatten(io_lib:format("~s", [sqlbuilder:build_set_sql(get_test_request())]))).


build_overwrite_sql_test() ->
    ?assertEqual("UPDATE unit.test SET metric1 = '123'::bigint, metric2 = '123.456'::double precision WHERE id = '1'::integer AND host = 'test_host'::text AND day = '2013-01-01'::date",
                 lists:flatten(io_lib:format("~s", [sqlbuilder:build_overwrite_sql(get_test_request())]))).


build_update_sql_test() ->
    ?assertEqual("UPDATE unit.test SET metric1 = COALESCE(metric1, 0) + '123'::bigint, metric2 = COALESCE(metric2, 0) + '123.456'::double precision WHERE id = '1'::integer AND host = 'test_host'::text AND day = '2013-01-01'::date",
                 lists:flatten(io_lib:format("~s", [sqlbuilder:build_update_sql(get_test_request())]))).


build_returning_sql_test() ->
    ?assertEqual("RETURNING id, host, day, metric1, metric2",
                 lists:flatten(io_lib:format("~s", [sqlbuilder:build_returning_sql(get_test_table_columns())]))).


build_as_sql_test() ->
    ?assertEqual("AS (id integer, host text, day date, metric1 bigint, metric2 double precision)",
                 lists:flatten(io_lib:format("~s", [sqlbuilder:build_as_sql(get_test_table_columns())]))).

