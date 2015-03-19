-module(q_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case q_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

init_0_test() ->
    {Ret, _} = start(whateva, idowhatiwant),
    ?assertEqual(
        Ret,
        ok).

-endif.
