-module(election_sup).
-behaviour(supervisor).
-export([start_link/0,
         start_child/0
        ]).
-export([init/1]).

-define(CHILD(I, Type, Timeout, Args), {I, {I, start_link, Args}, permanent, Timeout, Type, [I]}).
-define(CHILD(I, Type, Timeout), ?CHILD(I, Type, Timeout, [[]])).
-define(CHILD(I, Type), ?CHILD(I, Type, 5000)).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?MODULE, []).

init([]) ->
    Children = lists:flatten([
      ?CHILD(election, worker)
    ]),
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.
