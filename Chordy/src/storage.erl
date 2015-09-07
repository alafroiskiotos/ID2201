-module(storage).
-compile(export_all).

create() ->
    [].

add(Key, Value, Storage) ->
    TmpS = lists:append(Storage, [{Key, Value}]),
    lists:ukeysort(1, TmpS).

lookup(Key, Storage) ->
    {_, Result} = lists:keysearch(Key, 1, Storage),
    Result.

split(Key, Storage) ->
    lists:splitwith(fun({SKey, _}) -> SKey =< Key end, Storage).

merge(Storage0, Storage) ->
    lists:umerge(Storage0, Storage).

size(Storage) ->
    lists:flatlength(Storage).
