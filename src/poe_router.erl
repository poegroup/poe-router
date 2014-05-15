-module(poe_router).

-export([start/0]).
-export([start/2]).

start() ->
  ok = application:start(crypto),
  ok = application:start(asn1),
  ok = application:start(public_key),
  ok = application:start(ssl),
  ok = application:start(cowlib),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(gun),
  ok = application:start(poe_router).

start(Ref, _Config) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/:app/[...]", poe_router_handler, []},
      {"/", poe_router_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
    {env, [{dispatch, Dispatch}]}
  ]),
  {ok, Ref}.
