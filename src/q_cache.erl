-module(q_cache).
-export([insert/2, lookup/1, delete/1]).
insert(Key, Value) ->
    case q_store:lookup(Key) of
        {ok, Pid} ->
            q_detect:replace(Pid, Value);
        {error, _} ->
            {ok, Pid} = q_detect:create(Value),
            q_store:insert(Key, Pid)
    end.
lookup(Key) ->
    try
        {ok, Pid} = q_store:lookup(Key),
        {ok, Value} = q_detect:fetch(Pid),
        {ok, Value}
    catch
        _Class:_Exception ->
            {error, not_found}
    end.
delete(Key) ->
    case q_store:lookup(Key) of
        {ok, Pid} ->
            q_detect:delete(Pid);
        {error, _Reason} ->
            ok
    end.

