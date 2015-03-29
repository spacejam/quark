-module(q_detect).
-behaviour(gen_server).
-export([
         start_link/1,
         create/1
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("nodes.hrl").

-define(SERVER, ?MODULE).
-define(ASK_INTERVAL_SECONDS, 3).
-define(BASE_WAIT, 0).
-define(SPLAY, 1).

-spec detect_dead_nodes(Nodes :: nodes) -> nodes.
detect_dead_nodes(Nodes) ->
    lists:foldl(fun(Node, NewNodes) ->
        case net_kernel:node_info(Node) of
          {ok, _} ->
              NewNodes#nodes{connected=[Node | NewNodes#nodes.connected]};
          {error, Msg} ->
              error_logger:error_msg("[~w] Lost connection to ~p: ~w", [node(), Node, Msg]),
              NewNodes#nodes{slop=[Node | NewNodes#nodes.slop]}
          end
    end, Nodes#nodes{connected=[]}, Nodes#nodes.connected).

-spec reset_connected_nodes(Nodes :: nodes) -> nodes.
reset_connected_nodes(Nodes) ->
    Nodes#nodes{connected=nodes()}.

-spec potentially_enslop(Nodes :: nodes, NeighborNodes :: [term()]) -> nodes.
potentially_enslop(Nodes, NeighborNodes) ->
    % TODO perform novelty determination here
    Desired = lists:filter(fun(Node) ->
        case (lists:member(Node, Nodes#nodes.connected) or (Node == node())) of
          true -> false;
          false -> true
        end
    end, NeighborNodes),
    Nodes#nodes{slop=lists:merge(Nodes#nodes.slop, Desired)}.

-spec attempt_slop_connect(Nodes :: nodes) -> nodes.
attempt_slop_connect(Nodes) ->
    lists:foldl(fun(Node, NewNodes) ->
        case net_kernel:connect_node(Node) of
            true ->
              error_logger:info_msg("[~w] Successfully connected to ~p", [node(), Node]),
              NewNodes#nodes{connected=[Node | NewNodes#nodes.connected]};
            false ->
              error_logger:error_msg("[~w] Failed to connect to ~p", [node(), Node]),
              NewNodes#nodes{slop=[Node | NewNodes#nodes.slop]};
            ignored ->
                error_logger:error_msg(" ignored - local node not alive!  exiting!"),
                init:stop(),
                NewNodes
        end
    end, Nodes#nodes{slop=[]}, Nodes#nodes.slop).

start_link(Nodes) ->
    gen_server:start_link(?MODULE, Nodes, []).

create(Value) ->
    q_sup:start_child(Value).

splay() ->
    (?BASE_WAIT + random:uniform(?SPLAY)) * 1000.

init(State) ->
    register(detect, self()),
    erlang:send_after(splay(), self(), chat),
    {ok, State}.

handle_call(fetch, _From, State) ->
    {reply, {ok, State}, State}.

handle_cast({replace, Value}, _State) ->
    {noreply, Value};
handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(chat, State) ->
    error_logger:info_msg("[~w] Checking connection states.  State: ~w~n", [node(), State]),
    DetectedLiveNodes = detect_dead_nodes(State),
    ResetConnectedNodes = reset_connected_nodes(DetectedLiveNodes),
    {ResL, _BadNodes} = rpc:multicall(ResetConnectedNodes#nodes.connected, erlang, nodes, []),
    MoreSloppyNodes = potentially_enslop(ResetConnectedNodes, lists:flatten(ResL)),
    FreshState = attempt_slop_connect(MoreSloppyNodes),
    erlang:send_after(splay(), self(), chat),
    {noreply, FreshState}.

terminate(_Reason, _State) ->
    error_logger:info_msg("terminating detector"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    error_logger:info_msg("code change in detector"),
    {ok, State}.
