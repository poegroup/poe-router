-module(poe_router_backend_env).

-export([start/0]).

start() ->
  Apps = get_csl("APPS"),
  [handle_app(App, format(App)) || App <- Apps],
  ok.

handle_app(App, APP) ->
  Branches = get_csl(APP ++ "_BRANCHES", ["master"]),
  [handle_branch(App, APP, Branch, format(Branch)) || Branch <- Branches].

handle_branch(App, APP, Branch, BRANCH) ->
  Backends = get_csl(APP ++ "_BRANCH_" ++ BRANCH),
  [handle_backend(App, APP, Branch, Backend) || Backend <- Backends].

handle_backend(App, APP, Branch, Backend) ->
  [ID, Weight] = string:tokens(Backend, ":"),
  case simple_env:get(APP ++ "_URL_" ++ ID) of
    undefined ->
      undefined;
    URI ->
      {ok, {Scheme, _UserInfo, Host, Port, Path, _Query}} = http_uri:parse(URI),
      poe_router_manager:add(
        list_to_binary(App),
        list_to_binary(Branch),
        list_to_integer(ID),
        {to_type(Scheme), Host, Port, list_to_binary(Path)},
        list_to_integer(Weight)
      )
  end.

get_csl(Name) ->
  get_csl(Name, []).
get_csl(Name, Default) ->
  case simple_env:get(Name) of
    undefined ->
      Default;
    List ->
      string:tokens(List, ",")
  end.

%% TODO handle characters like '/' and '-'
format(Value) ->
  string:to_upper(Value).

to_type(http) ->
  tcp;
to_type(_) ->
  ssl.
