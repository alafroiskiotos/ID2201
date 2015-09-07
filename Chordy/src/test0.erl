-module(test0).
-export([start/2, stop/1, status/1]).

start(Module, Number) ->
    Pid = apply(Module, start, [0]),
    %%Pid = node1:start(0),
    Name = 'node0',
    register(Name, Pid),
    timer:sleep(200),
    init(Module, Number, 1, [Pid]).

init(Module, Number, Counter, Spawned) ->
    case Number of
        Counter ->
            Spawned;
        _ ->
            [H | _] = Spawned,
            Pid = apply(Module, start, [Counter, H]),
           % Pid = node1:start(Counter, H),
        
            Name = "node" ++ integer_to_list(Counter),
            register(list_to_atom(Name), Pid),
            NSpawned = lists:append(Spawned, [Pid]),
            init(Module, Number, Counter + 1, NSpawned)
    end.

status(Nodes) ->
    lists:foreach(fun(Node) -> Node ! status, timer:sleep(10) end, Nodes).

stop(Nodes) ->
    lists:foreach(fun(Node) -> Node ! stop end, Nodes).
