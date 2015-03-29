-module(q_app).
-behaviour(application).
-export([start/2, stop/1]).

-include_lib("nodes.hrl").

start(normal, _Args) ->
    Nodes = case init:get_argument(seeds) of
        {ok, [Seeds]} ->
          % toss Seeds into the slop list, to be connected with later
          SeedAtoms = lists:flatmap(fun(Seed) ->
              SeedAtom = list_to_atom(Seed),
              case SeedAtom == node() of
                true -> [];
                false -> [SeedAtom]
              end
          end, Seeds),
          #nodes{slop=SeedAtoms};
        error ->
            error_logger:warning_msg("no seeds configured."),
            case net_kernel:get_net_ticktime() of
                ignored ->
                    error_logger:error_msg("local node not alive!  exiting!"),
                    init:stop();
                _A ->
                    error_logger:info_msg("listening for others to connect.")
            end,
            #nodes{}
    end,
    case q_sup:start_link() of
        {ok, _Pid} ->
            error_logger:info_msg("started supervisor~n");
        Other ->
            error_logger:error_msg("failed to start supervisor: ~p", [Other]),
            init:stop()
    end,
    q_sup:start_child(Nodes).

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
