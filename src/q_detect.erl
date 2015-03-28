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
-define(SPLAY, 5).

-spec filter_dead_nodes(Nodes :: nodes) -> nodes.
filter_dead_nodes(Nodes) ->
    lists:foldl(fun(Node, NewNodes) ->
        case net_kernel:node_info(Node) of
          {ok, _} ->
              NewNodes#nodes{connected=[Node | NewNodes#nodes.connected]};
          {error, _} ->
              error_logger:error_msg("Lost connection to ~p", [Node]),
              NewNodes#nodes{slop=[Node | NewNodes#nodes.slop]}
          end
    end, Nodes#nodes{connected=[]}, Nodes#nodes.connected).

-spec attempt_slop_connect(Nodes :: nodes) -> nodes.
attempt_slop_connect(Nodes) ->
    lists:foldl(fun(Node, NewNodes) ->
        case net_kernel:connect_node(Node) of
            true ->
              error_logger:info_msg("Successfully connected to ~p", [Node]),
              NewNodes#nodes{connected=[Node | NewNodes#nodes.connected]};
            false ->
              error_logger:error_msg("Failed to connect to ~p", [Node]),
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
    FilteredNodes = filter_dead_nodes(State),
    error_logger:info_msg("[~w] pinging connected nodes for new friends.  State: ~w~n", [node(), State]),
    FreshState = attempt_slop_connect(FilteredNodes),
    erlang:send_after(splay(), self(), chat),
    {noreply, FreshState}.

terminate(_Reason, _State) ->
    error_logger:info_msg("terminating detector"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    error_logger:info_msg("code change in detector"),
    {ok, State}.
