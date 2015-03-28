-module(q_app).
-behaviour(application).
-export([start/2, stop/1, attempt_slop_connect/1]).

-include_lib("nodes.hrl").

-spec attempt_slop_connect(Nodes :: nodes) -> nodes.
attempt_slop_connect(Nodes) ->
    lists:foldl(fun(Node, NewNodes) ->
        case net_kernel:connect_node(list_to_atom(Node)) of
            true ->
              error_logger:info_msg("Successfully connected to ~p", [Node]),
              NewNodes#nodes{connected=[Node | NewNodes#nodes.connected]};
            false ->
              error_logger:error_msg("Failed to connect to ~p", [Node]),
              NewNodes#nodes{slop=[Node | NewNodes#nodes.slop]};
            ignored ->
                error_logger:error_msg(" ignored - local node not alive!  exiting!"),
                init:stop(),
                #nodes{}
        end
    end, #nodes{}, Nodes).

start(normal, _Args) ->
    % first, connect to any specified seeds
    Nodes = case init:get_argument(seeds) of
        {ok, [Seeds]} ->
            attempt_slop_connect(Seeds);
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
