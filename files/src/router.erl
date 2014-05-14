-module(router).

-export([start/0]).

start() ->
  poe_router:start(),
  ok = application:start(router).
