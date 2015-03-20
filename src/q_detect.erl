-module(q_detect).
-behaviour(gen_server).
-export([
         start_link/1,
         create/1
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ASK_INTERVAL_SECONDS, 3).
-define(BASE_WAIT, 10).
-define(SPLAY, 5).

start_link(Nodes) ->
    gen_server:start_link(?MODULE, [Nodes], []).

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
    error_logger:info_msg("pinging connected nodes for new friends"),
    erlang:send_after(splay(), self(), chat),
    {noreply, State}.

terminate(_Reason, _State) ->
    error_logger:info_msg("terminating detector"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    error_logger:info_msg("code change in detector"),
    {ok, State}.
