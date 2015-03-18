-module(q_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    q_store:init(),
    case q_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").

%% TODO add some nice quickchecks here
proper_module_test() ->
  ?assertEqual(
    [],
    proper:module(?MODULE, [long_result])).

-include_lib("eunit/include/eunit.hrl").

init_0_test() ->
  {Ret, _} = start(whateva, idowhatiwant),
  ?assertEqual(
     Ret,
     ok).

-endif.
