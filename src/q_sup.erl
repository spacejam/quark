-module(q_sup).
-behaviour(supervisor).
-export([start_link/0,
         start_child/1
        ]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Nodes) ->
    supervisor:start_child(?SERVER, [Nodes]).

init([]) ->
    Element = {q_detect, {q_detect, start_link, []},
               temporary, brutal_kill, worker, [q_detect]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
