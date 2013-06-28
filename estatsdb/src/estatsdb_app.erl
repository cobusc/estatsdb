%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the estatsdb application.

-module(estatsdb_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for estatsdb.
start(_Type, _StartArgs) ->
    estatsdb:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for estatsdb.
stop(_State) ->
    ok.
