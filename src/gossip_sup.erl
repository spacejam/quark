-module(gossip_sup).
-behaviour(supervisor).
-export([start_link/0,
         start_child/1
        ]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Nodes) ->
    supervisor:start_child(?MODULE, [Nodes]).

init([]) ->
    Children = [
      {gossip, {gossip, start_link, []}, temporary, brutal_kill, worker, [gossip]}
    ],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
