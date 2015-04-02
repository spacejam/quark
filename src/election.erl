-module(election).
-behaviour(gen_server).
-export([
         start_link/1
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link([]) ->
    error_logger:info_msg("[~w] in election start_link/1", [node()]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(State) ->
    error_logger:info_msg("[~w] in election init/1", [node()]),
    {ok, State}.

handle_call(fetch, _From, State) ->
    % error_logger:info_msg("[~w] in handle_call(fetch...)~n", [node()]),
    {reply, {ok, State}, State}.

handle_cast({replace, Value}, _State) ->
    {noreply, Value};
handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(timeout, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    error_logger:info_msg("terminating election"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    error_logger:info_msg("code change in election"),
    {ok, State}.
