-module(q_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % first, connect to any specified seeds
    case init:get_argument(seeds) of
        {ok, [Seeds]} ->
            lists:foreach(fun(Seed) ->
                  case net_kernel:connect_node(list_to_atom(Seed)) of
                      true -> error_logger:info_msg("Successfully connected to ~p", [Seed]);
                      false -> error_logger:error_msg("Failed to connect to ~p", [Seed]);
                      ignored ->
                          error_logger:error_msg(" ignored - local node not alive!  exiting!"),
                          init:stop()
                  end
            end, Seeds);
        error ->
            error_logger:warning_msg("no seeds configured."),
            case net_kernel:get_net_ticktime() of
                ignored ->
                    error_logger:error_msg("local node not alive!  exiting!"),
                    init:stop();
                _A ->
                    error_logger:info_msg("listening for others to connect.")
            end
    end,
    case q_sup:start_link() of
        {ok, _Pid} ->
            error_logger:info_msg("started supervisor~n");
        Other ->
            error_logger:error_msg("failed to start supervisor: ~p", [Other]),
            init:stop()
    end,
    q_sup:start_child(nodes()).

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
