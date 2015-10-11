-module(key).
-export([generate/0, between/3]).

-define(MAX, 1000000000).

generate() ->
    random:uniform(?MAX).

between(Key, From, To) when From < To ->
    From < Key andalso Key =< To;
between(Key, From, To) when From > To ->
    Key =< To orelse From < Key;
between(_Key, Id, Id) ->
    true.

