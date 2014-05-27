-module(poe_router_pinger).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([stop/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
  tref :: reference(),
  interval :: integer()
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> stopped.
stop() ->
  gen_server:call(?MODULE, stop).

%% gen_server.

-spec init([]) -> {ok, #state{}}.
init([]) ->
  Inteval = simple_env:get_integer("PING_INTERVAL", 10000),
  TRef = erlang:send_after(Inteval, self(), ping),
  {ok, #state{tref = TRef, interval = Inteval}}.

-spec handle_call(any(), _, State) -> {reply, ignored, State} | {stop, normal, stopped, State} when State::#state{}.
handle_call(stop, _From, State = #state{tref = TRef}) ->
  {ok, cancel} = erlang:cancel_timer(TRef),
  {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

-spec handle_cast(_, State) -> {noreply, State} when State::#state{}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(any(), State) -> {noreply, State} when State::#state{}.
handle_info(ping, State = #state{interval = Interval}) ->
  ping(),
  TRef = erlang:send_after(Interval, self(), ping),
  {noreply, State#state{tref = TRef}};
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(_, State, _) -> {ok, State} when State::#state{}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal.

ping() ->
  Confs = poe_router_manager:backend_confs(),
  Refs = [begin
    {ok, Pid} = gun:open(Host, Port, [{type, Proto}]),
    Ref = gun:get(Pid, Path, [{<<"user-agent">>, <<"poe-router">>}, {<<"accept">>, <<"*/*">>}]),
    {Pid, Ref, Host}
  end || {Proto, Host, Port, Path} <- Confs],
  [begin
    case gun:await(Pid, Ref) of
      {response, _, Status, _} when Status < 500 ->
        ok;
      {response, _, Status, _Headers} when Status >= 500 ->
        io:format("source=health-check count#health-check.~p=1 count#health-check.~s=1~n", [Status, Host])
    end,
    gun:await_body(Pid, Ref)
  end || {Pid, Ref, Host} <- Refs],
  ok.
