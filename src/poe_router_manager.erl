-module(poe_router_manager).

-export([init/0]).
-export([get/3]).
-export([add/5]).
-export([update/4]).
-export([remove/2]).
-export([remove/3]).

%% private.
-export([hash/2]).

-define(CONN_TAB, poe_router_connections).
-define(APP_TAB, poe_router_apps).
-define(HASH_RANGE, 16777216).

init() ->
  ?APP_TAB = ets:new(?APP_TAB, [bag, public, named_table, {read_concurrency, true}]),
  ok.

%% return a list of hosts for a given branch
get(Name, Branch, User) ->
  Key = {Name, Branch},
  case ets:lookup(?APP_TAB, Key) of
    [{_, _, Conf, _}] ->
      {ok, Conf};
    [] when Branch =:= <<"master">> ->
      {error, {notfound, Key}};
    [] ->
      %% try the prod branch - the other was most likely deleted
      get(Name, <<"master">>, User);
    Confs ->
      hash({User, Name}, Confs)
  end.

add(Name, Branch, ID, Conf, Weight) when is_integer(ID) ->
  true = ets:insert(?APP_TAB, {{Name, Branch}, ID, Conf, Weight}),
  ok.

update(_Name, _Branch, _Conf, _Weight) ->
  %% TODO
  %% ets:update_element(?APP_TAB, {Name, Branch}, {}),
  ok.

remove(Name, Branch) ->
  true = ets:delete(?APP_TAB, {Name, Branch}),
  ok.

remove(Name, Branch, Conf) ->
  true = ets:match_delete(?APP_TAB, {{Name, Branch}, '_', Conf, '_'}),
  ok.

%% private.

hash(Key, Confs) ->
  %% TODO possibly memoize this if it becomes an issue
  {Total, Range} = format_confs(Confs),
  Pos = erlang:phash2(Key, ?HASH_RANGE) / ?HASH_RANGE * Total,
  select(Pos, Range).

format_confs(Confs) ->
  Sorted = lists:sort(fun sort/2, Confs),
  range(Sorted, 0, []).

sort({_, A, _, _}, {_, B, _, _}) ->
  A =< B.

range([], Total, Range) ->
  {Total, Range};
range([{_, _, _, 0}|Confs], Total, Range) ->
  range(Confs, Total, Range);
range([{_, _, Conf, Weight}|Confs], Total, Range) ->
  range(Confs, Total + Weight, [{Total, Conf}|Range]).

select(Pos, [{Index, Conf}|_]) when Index =< Pos ->
  {ok, Conf};
select(Pos, [_|Rest]) ->
  select(Pos, Rest).
