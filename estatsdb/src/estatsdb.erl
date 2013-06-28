%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc estatsdb startup code

-module(estatsdb).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start_deps() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    ensure_started(public_key),
    ensure_started(ssl),
    ensure_started(epgsql),
    ensure_started(epgsql_pool),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine).


%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ok = start_deps(),
    estatsdb_sup:start_link().

%% @spec start() -> ok
%% @doc Start the estatsdb server.
start() ->
    ok = start_deps(),
    application:start(estatsdb).

%% @spec stop() -> ok
%% @doc Stop the estatsdb server.
stop() ->
    Res = application:stop(estatsdb),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    Res.
