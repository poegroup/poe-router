-module(poe_router_validation_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_, Req, Opts) ->
  {ok, Req, fast_key:get(value, Opts, <<"42">>)}.

handle(Req, Value) ->
  {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], Value, Req),
  {ok, Req2, Value}.

terminate(_Reason, _Req, _State) ->
  ok.
