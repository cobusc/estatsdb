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
     ?assertEqual(ok, timer:sleep(5000))

    end]}.


refresh_test() ->
    Url = "http://localhost:8000/refresh",
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, ResponseData}} = httpc:request(Url),
    ?assertEqual("{\"success\":true}", ResponseData).


teardown_test() ->
    ?assertEqual(ok, estatsdb:stop()).
