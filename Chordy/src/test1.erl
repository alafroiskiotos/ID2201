-module(test1).
-export([start/2, stop/1, status/1, add/2, lookup/2]).

start(Module, Number) ->
    Pid = apply(Module, start, [key:generate()]),
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

add(Node, Number) ->
    Now = erlang:now(),
    [After | Elements] = add_i(Node, Number, []),
    Diff = timer:now_diff(After, Now),
    io:format("Total time to add ~w data in microseconds: ~w~n", [Number, Diff]),
    Elements.
    
add_i(Node, Number, Elements) ->
    case Number of
        0 ->
            [erlang:now() | Elements];
        _ ->
            Rnd = key:generate(),
            Ref = make_ref(),
            Node ! {add, Rnd, key:generate(), Ref, self()},
            receive
                {Ref, ok} ->
                    add_i(Node, Number - 1, [Rnd | Elements])
            end
    end.

lookup(Node, Elements) ->
    Now = erlang:now(),
    After = lookup_i(Node, Elements),
    Diff = timer:now_diff(After, Now),
    N = lists:flatlength(Elements),
    io:format("Total time to search for ~w elements in microseconds: ~w~n", [N,
        Diff]).

lookup_i(Node, Elements) ->
    case Elements of
        [] ->
            erlang:now();
        [Head | Tail] ->
            Ref = make_ref(),
            Node ! {lookup, Head, Ref, self()},
            receive
                {Ref, _} ->
                    %io:format("Result: ~w~n", [Result]),
                    lookup_i(Node, Tail)
            end
    end.

status(Nodes) ->
    lists:foreach(fun(Node) -> Node ! status, timer:sleep(10) end, Nodes).

stop(Nodes) ->
    lists:foreach(fun(Node) -> Node ! stop end, Nodes).
