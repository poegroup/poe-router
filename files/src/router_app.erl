-module(router_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  {ok, _} = poe_router:start(http, #{}),
  ok = poe_router_backend_env:start(),
  router_sup:start_link().

stop(_State) ->
  ok.
