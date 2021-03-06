-module(poe_router_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
  ok = poe_router_manager:init(),
  Procs = [{poe_router_pinger, {poe_router_pinger, start_link, []},
          permanent, 5000, worker, [poe_router_pinger]}],
  {ok, {{one_for_one, 10, 10}, Procs}}.
