-module(poe_router_favicon).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-compile({parse_transform,export_val}).
-export_val([favicon/0]).

-define(TRANSPARENT_GIF, <<71,73,70,56,57,97,1,0,1,0,128,0,0,0,0,0,255,255,255,33,249,4,1,0,0,0,0,44,0,0,0,0,1,0,1,0,0,2,1,68,0,59>>).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Headers, Body} = favicon(),
  {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

favicon() ->
  case simple_env:get("FAVICON_URL") of
    undefined ->
      {[{<<"content-type">>, <<"image/gif">>}], ?TRANSPARENT_GIF};
    URL ->
      ok = application:ensure_started(crypto),
      ok = application:ensure_started(asn1),
      ok = application:ensure_started(public_key),
      ok = application:ensure_started(ssl),
      ok = application:ensure_started(inets),
      {ok, {{_, 200, _}, Headers, Body}} = httpc:request(get, {URL, []}, [], [{body_format, binary}]),
      case fast_key:get("content-type", Headers) of
        undefined ->
          {[], Body};
        ContentType ->
          {[{<<"content-type">>, ContentType}], Body}
      end
  end.
