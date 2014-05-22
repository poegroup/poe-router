-module(poe_router_handler).

-export([init/3]).
-export([proxy_init/2]).
-export([backend/2]).
-export([forwarded_header_prefix/2]).
-export([request_id/2]).
-export([req_headers/3]).
-export([res_headers/3]).

-define(HEADERS, [
  {<<"x-ua-compatible">>, <<"IE=Edge,chrome=1">>},
  {<<"x-frame-options">>, <<"SAMEORIGIN">>},
  {<<"x-content-type-options">>, <<"nosniff">>},
  {<<"x-xss-protection">>, <<"1; mode=block">>},
  {<<"strict-transport-security">>, <<"max-age=31536000; includeSubDomains">>}
]).

init(_, _Req, _Opts) ->
  {upgrade, protocol, ranger}.

proxy_init(Req, Opts) ->
  {ok, Req, Opts}.

backend(Req, State) ->
  {Name, Req2} = app_name(Req, State),
  {Branch, Req3} = branch(Name, Req2, State),
  {User, Req4} = user(Req3, State),
  case poe_router_manager:get(Name, Branch, User) of
    {ok, Conf} ->
      {Conf, Req4, State};
    {error, {notfound, _Conf}} ->
      {error, 404, Req4};
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
  {Env, Req2} = cowboy_req:meta(x_env, Req, <<"production">>),
  Headers2 = [{<<"x-env">>, Env}|lists:keydelete(<<"accept-encoding">>, 1, Headers)],
  case fast_key:get(<<"x-orig-proto">>, Headers2) of
    undefined ->
      Proto = fast_key:get(<<"x-forwarded-proto">>, Headers2),
      Headers3 = [{<<"x-orig-proto">>, Proto}|Headers2],
      case Proto of
        <<"https">> ->
          case fast_key:get(<<"x-orig-port">>, Headers3) of
            <<"80">> ->
              {fast_key:set(<<"x-orig-port">>, <<"443">>, Headers3), Req2, State};
            _ ->
              {Headers3, Req2, State}
          end;
        _ ->
          {Headers3, Req2, State}
      end;
    _ ->
      {Headers2, Req2, State}
  end.

res_headers(Headers, Req, State) ->
  {?HEADERS ++ Headers, Req, State}.

app_name(Req, State) ->
  case cowboy_req:binding(app, Req) of
    {undefined, Req2} ->
      {fast_key:get(app, State, <<"root">>), Req2};
    {App, Req2} ->
      {App, Req2}
  end.

branch(Name, Req, _State) ->
  cowboy_req:cookie(<<"-", Name/binary>>, Req, <<"master">>).

user(Req, _State) ->
  {<<"id">>, Req}.
