-module(q_detect).
-behaviour(gen_server).
-export([
         start_link/2,
         create/2,
         create/1
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).
-define(ASK_INTERVAL_SECONDS, 3).
-define(BASE_WAIT, 10).
-define(SPLAY, 5).

-record(state, {doods}).

start_link(Value, LeaseTime) ->
    gen_server:start_link(?MODULE, [Value, LeaseTime], []).

create(Value, LeaseTime) ->
    q_sup:start_child(Value, LeaseTime).

create(Value) ->
    create(Value, ?DEFAULT_LEASE_TIME).

splay() ->
    %% ?BASE_WAIT + random:uniform(?SPLAY).
    random:uniform(2).

init([]) ->
    erlang:send_after(splay(), self(), chat),
    State = #state{doods = []},
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
    erlang:send_after(splay(), self(), chat),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
