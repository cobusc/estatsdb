-module(functional_tests).
-include_lib("eunit/include/eunit.hrl").

ensure_loaded(App) ->
    case application:load(App) of
        ok ->
            ok;
        {error, {already_loaded, App}} ->
            ok
    end.

setup_test_() ->
    {timeout, 10, [fun () ->

     ?assertEqual(ok, ensure_loaded(epgsql_pool)),   
     ?assertEqual(ok, application:set_env(epgsql_pool, pools, [default])),
     ?assertEqual(ok, application:set_env(epgsql_pool, default, {10, [{database, "estatsdb"},
                                                                      {username, "estatsdb"},
                                                                      {port,5432},
                                                                      {password, "password"},
                                                                      {timeout, 60000}
                                                                ]})), 
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
    Url = "http://localhost:8000/set?tablename=public.hourly_example_stats&host=test_host&hour=2013-01-01T00:&metric1=1&metric2=2",
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual("{\"host\":\"test_host\",\"hour\":\"2013-01-01T00:00:00.000\",\"metric1\":1,\"metric2\":2.0,\"metric3\":null}",
                 ResponseData).


set_existing_test() ->
    % Existing row. Different metrics.
    Url = "http://localhost:8000/set?tablename=public.hourly_example_stats&host=test_host&hour=2013-01-01T00:&metric1=1&metric3=3",
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual("{\"host\":\"test_host\",\"hour\":\"2013-01-01T00:00:00.000\",\"metric1\":1,\"metric2\":2.0,\"metric3\":3}",
                 ResponseData).


update_nonexistent_test() ->
    % Non-existent row. Partial metrics.
    Url = "http://localhost:8000/update?tablename=public.hourly_example_stats&host=test_host&hour=2013-01-01T01:&metric1=1&metric2=2",
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual("{\"host\":\"test_host\",\"hour\":\"2013-01-01T01:00:00.000\",\"metric1\":1,\"metric2\":2.0,\"metric3\":null}",
                 ResponseData).


update_existing_test() ->
    % Existing row. Different metrics.
    Url = "http://localhost:8000/update?tablename=public.hourly_example_stats&host=test_host&hour=2013-01-01T01:&metric1=1&metric3=3",
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual("{\"host\":\"test_host\",\"hour\":\"2013-01-01T01:00:00.000\",\"metric1\":2,\"metric2\":2.0,\"metric3\":3}",
                 ResponseData).


teardown_test() ->
    ?assertEqual(ok, estatsdb:stop()).
