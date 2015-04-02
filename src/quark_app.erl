-module(quark_app).
-behaviour(application).
-export([start/2, stop/1]).

-include_lib("nodes.hrl").

start(normal, _Args) ->
    Seeds = case init:get_argument(seeds) of
        {ok, [SeedsIn]} ->
          % toss Seeds into the slop list, to be connected with later
          SeedAtoms = lists:flatmap(fun(Seed) ->
              SeedAtom = list_to_atom(Seed),
              case SeedAtom == node() of
                true -> [];
                false -> [SeedAtom]
              end
          end, SeedsIn),
          #nodes{slop=SeedAtoms};
        error ->
            error_logger:warning_msg("[~w] no seeds configured.", [node()]),
            case net_kernel:get_net_ticktime() of
                ignored ->
                    error_logger:error_msg("[~w] local node not alive!  exiting!", [node()]),
                    init:stop();
                _A ->
                    error_logger:info_msg("[~w] listening for others to connect.", [node()])
            end,
            #nodes{}
    end,
    LowQuorum = case init:get_argument(quorum) of
        {ok, [[Quorum]]} ->
            error_logger:info_msg("trying to parse \"~w\"", [Quorum]),
            case string:to_integer(Quorum) of
                {error, Reason} ->
                    error_logger:error_msg("[~w] passed a non-integer as arg to -quorum: "
                        "~w", [node(), Reason]),
                    init:stop();
                {Q, []} ->
                    Q
            end;
        error ->
            error_logger:warning_msg("[~w] No quorum low-water mark (-quorum <number>) enabled! "
                "You must pass this or you will split brain!  Recommended (N/2) + 1", [node()]),
            init:stop()
    end,
    case gossip_sup:start_link() of
        {ok, _Pid} ->
            error_logger:info_msg("started gossip supervisor~n");
        Other ->
            error_logger:error_msg("failed to start gossip supervisor: ~p", [Other]),
            init:stop()
    end,
    gossip_sup:start_child(Seeds),
    case election_sup:start_link() of
        {ok, _Pid2} ->
            error_logger:info_msg("started election supervisor~n");
        Other2 ->
            error_logger:error_msg("failed to start election supervisor: ~p", [Other2]),
            init:stop()
    end,
    election_sup:start_child(LowQuorum),
    case consensus_sup:start_link() of
        {ok, _Pid3} ->
            error_logger:info_msg("started consensus supervisor~n");
        Other3 ->
            error_logger:error_msg("failed to start consensus supervisor: ~p", [Other3]),
            init:stop()
    end,
    consensus_sup:start_child(LowQuorum).

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
