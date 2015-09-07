-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() -> [].

add(Name, Ref, Pid, Intf) ->
    lists:append(Intf, [{Name, Ref, Pid}]).

remove(Name, Intf) ->
    case lists:keysearch(Name, 1, Intf) of
        {_, Tuple} -> lists:delete(Tuple, Intf);
        false -> io:format("Interface was not found")
    end.

lookup(Name, Intf) ->
    case lists:keysearch(Name, 1, Intf) of
        {_, {_, _, Pid}} -> {ok, Pid};
        false -> notfound
    end.

ref(Name, Intf) ->
    case lists:keysearch(Name, 1, Intf) of
        {_, {_, Ref, _}} -> {ok, Ref};
        false -> notfound
    end.

name(Ref, Intf) ->
    case lists:keysearch(Ref, 2, Intf) of
        {_, {Name, _, _}} -> {ok, Name};
        false -> notfound
    end.

list(Intf) ->
    lists:foldl(fun({Name, _, _}, Acc) -> lists:append(Acc, [Name]) end, [], Intf).

broadcast(Message, Intf) ->
    lists:foreach(fun({_, _, Pid}) -> Pid ! Message end, Intf).
