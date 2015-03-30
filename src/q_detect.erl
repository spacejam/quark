-module(q_detect).
-behaviour(gen_server).
-export([
         start_link/1,
         create/1
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("nodes.hrl").

-define(BASE_WAIT, 5).
-define(SPLAY, 5).

-spec query_neighbors(Nodes :: nodes) -> [term()].
query_neighbors(Nodes) ->
    {ResL, _BadNodes} = gen_server:multi_call(Nodes#nodes.connected, detect, fetch, 1000),
    % error_logger:info_msg("[~w] ResL is ~w~n", [node(), ResL]),
    lists:flatmap(fun(Res) ->
        case Res of
            {_Node, {ok, {nodes, Connected, _Slop}}} -> Connected;
            _ ->
              error_logger:info_msg("[~w] Res is actually ~w~n", [nodes(), Res]),
              []
        end
    end, ResL).

start_link(Nodes) ->
    gen_server:start_link(?MODULE, Nodes, []).

create(Value) ->
    q_sup:start_child(Value).

splay() ->
    (?BASE_WAIT + random:uniform(?SPLAY)) * 1000.

init(State) ->
    [H | _Rest] = os:cmd("dd if=/dev/urandom count=1"),
    random:seed(H, H, H),
    register(detect, self()),
    self() ! gossip,
    {ok, State}.

handle_call(fetch, _From, State) ->
    % error_logger:info_msg("[~w] in handle_call(fetch...)~n", [node()]),
    {reply, {ok, State}, State}.

handle_cast({replace, Value}, _State) ->
    {noreply, Value};
handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(detect_dead, State) ->
    {noreply, lists:foldl(fun(Node, NewNodes) ->
        case net_kernel:node_info(Node) of
          {ok, _} ->
              NewNodes#nodes{connected=[Node | NewNodes#nodes.connected]};
          {error, Msg} ->
              error_logger:error_msg("[~w] Lost connection to ~p: ~w", [node(), Node, Msg]),
              NewNodes#nodes{slop=[Node | NewNodes#nodes.slop]}
          end
      end, State#nodes{connected=[]}, State#nodes.connected)};
handle_info(reset_connected, State) ->
    {noreply, State#nodes{connected=nodes()}};
handle_info(connect_slop, State) ->
    {noreply, lists:foldl(fun(Node, NewNodes) ->
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
    end, State#nodes{slop=[]}, State#nodes.slop)};
handle_info(gossip, State) ->
    error_logger:info_msg("[~w] Checking connection states.  State: ~w~n", [node(), State]),
    NeighborNodes = query_neighbors(State),
    % TODO perform novelty determination here
    Desired = lists:filter(fun(Node) ->
        not (lists:member(Node, State#nodes.connected) or (Node == node()))
    end, NeighborNodes),
    self() ! detect_dead,
    self() ! reset_connected,
    self() ! connect_slop,
    erlang:send_after(splay(), self(), gossip),
    {noreply, State#nodes{slop=lists:merge(State#nodes.slop, Desired)}}.

terminate(_Reason, _State) ->
    error_logger:info_msg("terminating detector"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    error_logger:info_msg("code change in detector"),
    {ok, State}.
