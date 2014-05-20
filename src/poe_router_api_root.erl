-module(poe_router_api_root).

-export([init/3]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([to_json/2]).

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

is_authorized(Req, State) ->
  % TODO authorize the request
  {true, Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/hyper+json">>, to_json},
    {<<"application/json">>, to_json}
  ], Req, State}.

to_json(Req, State) ->
  Apps = poe_router_manager:apps(),
  Body = jsxn:encode([{apps, format_apps(maps:to_list(Apps), [])}]),
  {Body, Req, State}.

format_apps([], Acc) ->
  lists:reverse(Acc);
format_apps([{Name, Branches}|Apps], Acc) ->
  Branches2 = [[{<<"title">>, Branch}] || Branch <- maps:keys(Branches)],
  App = [{<<"title">>, Name}, {<<"branches">>, Branches2}],
  format_apps(Apps, [App|Acc]).
