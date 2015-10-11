-module(storage).

-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() ->
    [].

add(Key, Value, Store) ->
    [{Key, Value} | Store].

lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
    lists:partition(fun({Key, _Value}) -> Key > From andalso Key =< To end, Store).

merge(Entries, Store) ->
    lists:merge(Entries, Store).
	
