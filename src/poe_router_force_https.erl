-module(poe_router_force_https).

-export([execute/2]).

execute(Req, Env) ->
  case fast_key:get(force_https, Env) of
    true ->
      case cowboy_req:header(<<"x-forwarded-proto">>, Req) of
        {<<"http">>, Req2} ->
          {<<"http://", Rest/binary>>, Req3} = cowboy_req:url(Req2),
          {ok, Req4} = cowboy_req:reply(302, [{<<"location">>, <<"https://", Rest/binary>>}], Req3),
          {halt, Req4};
        {_, Req2} ->
          {ok, Req2, Env}
      end;
    _ ->
      {ok, Req, Env}
  end.
