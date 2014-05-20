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

start(Ref, Config) ->
  Routes = fast_key:get(routes, Config, []),

  Internal = fast_key:get(internal_path, Config, "/_"),

  Dispatch = cowboy_router:compile([
    {'_', Routes ++ [
      %% {"/favicon.ico", poe_router_favicon, [fast_key:get(favicon, Config)]},
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

