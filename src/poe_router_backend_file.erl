-module(poe_router_backend_file).

-export([start/1]).

start(File) ->
  {ok, Apps} = file:consult(File),
  [handle_app(App) || App <- Apps],
  ok.

handle_app({App, Conf}) ->
  Branches = fast_key:get(branches, Conf),
  Backends = fast_key:get(backends, Conf),
  [handle_branch(App, Branch, Backends) || Branch <- Branches].

handle_branch(App, {Branch, BranchBackends}, Backends) ->
  [handle_backend(App, Branch, ID, Weight, fast_key:get(ID, Backends)) || {ID, Weight} <- BranchBackends].

handle_backend(_App, _Branch, _ID, _Weight, undefined) ->
  undefined;
handle_backend(App, Branch, ID, Weight, URI) ->
  {ok, {Scheme, _UserInfo, Host, Port, Path, _Query}} = http_uri:parse(URI),
  poe_router_manager:add(
    list_to_binary(App),
    list_to_binary(Branch),
    ID,
    {to_type(Scheme), Host, Port, list_to_binary(Path)},
    Weight
  ).

to_type(http) ->
  tcp;
to_type(_) ->
  ssl.
