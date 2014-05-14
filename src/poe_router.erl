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

start(_Ref, _Config) ->
  %% cowboy:start_http(Ref, NbAcceptors, TransOpts, ProtoOpts).
  ok.
