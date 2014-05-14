-module(poe_router_handler).

-export([init/3]).
-export([proxy_init/2]).
-export([backend/2]).
-export([forwarded_header_prefix/2]).
-export([request_id/2]).
-export([req_headers/3]).

init(_, _Req, _Opts) ->
  {upgrade, protocol, ranger}.

proxy_init(Req, Opts) ->
  {ok, Req, Opts}.

backend(Req, State) ->
  {Name, Req2} = app_name(Req, State),
  {Branch, Req3} = branch(Name, Req2, State),
  {User, Req4} = user(Req3, State),
  case poe_router_manager:get(Name, Branch, User) of
    {ok, Pid, Path, _Conf} ->
      {{Pid, Path}, Req4, State};
    Error ->
      %% TODO what should we do here so it fails with a decent message?
      Error
  end.

forwarded_header_prefix(Req, State) ->
  {{<<"x-forwarded-proto">>, <<"x-orig-host">>, <<"x-orig-port">>, <<"x-orig-path">>}, Req, State}.

request_id(Req, State) ->
  {ID, Req3} = case cowboy_req:header(<<"x-request-id">>, Req) of
    {undefined, Req2} ->
      {integer_to_binary(erlang:phash2(Req2)), Req2};
    {Val, Req2} ->
      {Val, Req2}
  end,
  {{<<"x-upstream-request-id">>, <<"x-request-id">>, ID}, Req3, State}.

req_headers(Headers, Req, State) ->
  Proto = fast_key:get(<<"x-orig-proto">>, Headers, fast_key:get(<<"x-forwarded-proto">>, Headers)),
  Headers2 = fast_key:set(<<"x-orig-proto">>, Proto, Headers),
  {Headers2, Req, State}.

app_name(Req, _State) ->
  case cowboy_req:binding(app, Req) of
    {undefined, Req2} ->
      %% TODO select from either the root or the api
      {<<"api">>, Req2};
    {App, Req2} ->
      {App, Req2}
  end.

branch(Name, Req, _State) ->
  cowboy_req:cookie(<<"poe-", Name/binary>>, Req, <<"master">>).

user(Req, _State) ->
  {<<"id">>, Req}.
