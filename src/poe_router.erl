-module(poe_router).

-export([start/0]).
-export([start/2]).

start() ->
  ok = application:ensure_started(crypto),
  ok = application:ensure_started(asn1),
  ok = application:ensure_started(public_key),
  ok = application:ensure_started(ssl),
  ok = application:ensure_started(cowlib),
  ok = application:ensure_started(ranch),
  ok = application:ensure_started(cowboy),
  ok = application:ensure_started(gun),
  ok = application:ensure_started(poe_router).

start(Ref, Config) ->
  Routes = fast_key:get(routes, Config, []),
  Internal = fast_key:get(internal_path, Config, "/_"),

  %% mount a validation endpoint for things like google webmaster tools
  Routes2 = case simple_env:get("VALIDATION_PATH") of
    undefined ->
      Routes;
    Path ->
      Val = simple_env:get_binary("VALIDATION_VALUE"),
      [{Path, poe_router_validation_handler, [{value, Val}]}] ++ Routes
  end,

  Dispatch = cowboy_router:compile([
    {'_', Routes2 ++ [
      {"/favicon.ico", poe_router_favicon, []},
      {Internal ++ "/api", poe_router_api_root, []},
      {Internal ++ "/[...]", poe_router_handler, [{app, <<"routerui">>}]},
      {"/:app/[...]", poe_router_handler, []},
      {"/", fast_key:get(root, Config, poe_router_handler), Config}
    ]}
  ]),

  ERL_ENV = simple_env:get("ERL_ENV", "production"),

  Env = [
    {dispatch, Dispatch},
    {force_https, fast_key:get(force_https, Config, ERL_ENV =/= "development")},
    {trust_proxy, fast_key:get(trust_proxy, Config, ERL_ENV =/= "development")}
  ] ++ fast_key:get(env, Config, []),

  Middlewares = [
    poe_router_force_https
  ] ++ fast_key:get(middlewares, Config, []) ++ [
    cowboy_router,
    cowboy_handler
  ],

  start(Ref, 100, [
    {port, fast_key:get(port, Config, simple_env:get_integer("PORT", 8080))}
  ], [
    {env, Env},
    {middlewares, Middlewares},
    {compress, true},
    {onrequest, fast_key:get(onrequest, Config)},
    {onresponse, fast_key:get(onresponse, Config)}
  ]).

start(https, Listeners, Tcp, Proto) ->
  cowboy:start_https(https, Listeners, Tcp, Proto);
start(spdy, Listeners, Tcp, Proto) ->
  cowboy:start_spdy(spdy, Listeners, Tcp, Proto);
start(Ref, Listeners, Tcp, Proto) ->
  cowboy:start_http(Ref, Listeners, Tcp, Proto).

