-module(functional_tests).
-include_lib("eunit/include/eunit.hrl").
-include("estatsdb.hrl").

ensure_loaded(App) ->
    case application:load(App) of
        ok ->
            ok;
        {error, {already_loaded, App}} ->
            ok
    end.

setup_test_() ->
    {timeout, 10, [fun () ->

     ?assertEqual(ok, ensure_loaded(kernel)),
     ?assertEqual(ok, application:set_env(kernel, error_logger, {file, "log/kernel.log"})),

     ?assertEqual(ok, ensure_loaded(sasl)),
     ?assertEqual(ok, application:set_env(sasl, sasl_error_logger, {file, "log/sasl-error.log"})),
     ?assertEqual(ok, application:set_env(sasl, errlog_type, error)),
     ?assertEqual(ok, application:set_env(sasl, error_logger_mf_dir, "log/sasl")),
     ?assertEqual(ok, application:set_env(sasl, error_logger_mf_maxbytes, 10485760)),
     ?assertEqual(ok, application:set_env(sasl, error_logger_mf_maxfiles, 5)),

     ?assertEqual(ok, ensure_loaded(epgsql_pool)),   
     ?assertEqual(ok, application:set_env(epgsql_pool, pools, [default])),
     ?assertEqual(ok, application:set_env(epgsql_pool, default, {10, [{database, "estatsdb"},
                                                                      {username, "estatsdb"},
                                                                      {port,5432},
                                                                      {password, "password"},
                                                                      {timeout, 60000}
                                                                ]})),

     ?assertEqual(ok, ensure_loaded(webmachine)),
     ?assertEqual(ok, application:set_env(webmachine, log_handlers, [
                                           {webmachine_log_handler, ["log"]},
                                           {webmachine_perf_log_handler, ["log"]}
                                          ])),

     ?assertEqual(ok, estatsdb:start()),

     ?assertEqual(ok, timer:sleep(5000)), % @todo

     ?assertEqual({ok,[],[]}, pgdb_tools:squery("DROP TABLE IF EXISTS hourly_example_stats")),
     ?assertEqual({ok,[],[]}, pgdb_tools:squery(
        "CREATE TABLE hourly_example_stats
         (
            hour TIMESTAMP(0),
            host TEXT,
            metric1 INTEGER,
            metric2 FLOAT,
            metric3 BIGINT,
            PRIMARY KEY(hour, host)
         )"))

    end]}.


refresh_test() ->
    Url = "http://localhost:8000/refresh",
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual("{\"success\":true}", ResponseData).

set_nonexistent_test() ->
    % Non-existent row. Partial metrics.
    Url = "http://localhost:8000/set?tablename=hourly_example_stats&host=test_host&hour=2013-01-01T00:&metric1=1&metric2=2",
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual("{\"host\":\"test_host\",\"hour\":\"2013-01-01T00:00:00.000\",\"metric1\":1,\"metric2\":2.0,\"metric3\":null}",
                 ResponseData).


set_existing_test() ->
    % Existing row. Different metrics.
    Url = "http://localhost:8000/set?tablename=hourly_example_stats&host=test_host&hour=2013-01-01T00:&metric1=1&metric3=3",
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual("{\"host\":\"test_host\",\"hour\":\"2013-01-01T00:00:00.000\",\"metric1\":1,\"metric2\":2.0,\"metric3\":3}",
                 ResponseData).


update_nonexistent_test() ->
    % Non-existent row. Partial metrics.
    Url = "http://localhost:8000/update?tablename=hourly_example_stats&host=test_host&hour=2013-01-01T01:&metric1=1&metric2=2",
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual("{\"host\":\"test_host\",\"hour\":\"2013-01-01T01:00:00.000\",\"metric1\":1,\"metric2\":2.0,\"metric3\":null}",
                 ResponseData).


update_existing_test() ->
    % Existing row. Different metrics.
    Url = "http://localhost:8000/update?tablename=hourly_example_stats&host=test_host&hour=2013-01-01T01:&metric1=1&metric3=3",
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual("{\"host\":\"test_host\",\"hour\":\"2013-01-01T01:00:00.000\",\"metric1\":2,\"metric2\":2.0,\"metric3\":3}",
                 ResponseData).


get_existing_test() ->
    Url = "http://localhost:8000/get?tablename=hourly_example_stats&host=test_host&hour=2013-01-01T01:&metric1&metric2",
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual("{\"host\":\"test_host\",\"hour\":\"2013-01-01T01:00:00.000\",\"metric1\":2,\"metric2\":2.0}",
                 ResponseData).


get_nonexistent_test() ->
    Url = "http://localhost:8000/get?tablename=hourly_example_stats&host=test_host&hour=2013-01-01T06:&metric1=1&metric3=3",
    ?assertMatch({ok, {{_Version, 404, _ReasonPhrase}, _Headers, _ResponseData}}, httpc:request(Url)).


no_tablename_specified_test() ->
    Url = "http://localhost:8000/update?host=test_host&hour=2013-01-01T01:&metric1=1&metric3=3",
    {ok, {{_Version, 400, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual(?ERROR_NO_TABLE, ResponseData).


unknown_table_test() ->
    Url = "http://localhost:8000/update?tablename=non_existent_table&host=test_host&hour=2013-01-01T01:&metric1=1&metric3=3",
    {ok, {{_Version, 400, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual(?ERROR_UNKNOWN_TABLE, ResponseData).


missing_primary_key_test() ->
    Url = "http://localhost:8000/update?tablename=hourly_example_stats&host=test_host&metric1=1&metric3=3",
    {ok, {{_Version, 400, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual(?ERROR_MISSING_PK, ResponseData).


unknown_column_test() ->
    Url = "http://localhost:8000/update?tablename=hourly_example_stats&host=test_host&hour=2013-01-01T01:&unknown=1&metric3=3",
    {ok, {{_Version, 400, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual(?ERROR_UNKNOWN_COLUMN, ResponseData).


missing_metric_test() ->
    Url = "http://localhost:8000/update?tablename=hourly_example_stats&host=test_host&hour=2013-01-01T01:",
    {ok, {{_Version, 400, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual(?ERROR_NO_METRIC, ResponseData).

wait_for_data(Url, ExpectedResponse) ->
    case httpc:request(Url) of 
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseData}} ->
            case ResponseData of
                ExpectedResponse -> 
                    ok;
                _ ->
                    io:format("~s~n", [ResponseData]),
                    ok = timer:sleep(100),
                    wait_for_data(Url, ExpectedResponse)
            end;
        {ok, {{_Version, 404, _ReasonPhrase}, _Headers, _ResponseData}} ->
            ok = timer:sleep(100),
            wait_for_data(Url, ExpectedResponse)
    end.


performance_test_() ->
    {timeout, 10, [fun () ->

     Url = "http://localhost:8000/update?tablename=hourly_example_stats&host=test_host&hour=2013-01-01T23:&metric1=1&metric3=3",
     Update = fun() ->
         httpc:request(Url)
     end,

     [ erlang:spawn(Update) || _ <- lists:seq(1, 200) ],

     CheckUrl = "http://localhost:8000/get?tablename=hourly_example_stats&host=test_host&hour=2013-01-01T23:&metric1&metric3",
     ExpectedResponse = "{\"host\":\"test_host\",\"hour\":\"2013-01-01T23:00:00.000\",\"metric1\":200,\"metric3\":600}",

     ?assertEqual(ok, wait_for_data(CheckUrl, ExpectedResponse))
    
    end]}.


teardown_test() ->
    ?assertEqual(ok, estatsdb:stop()).
