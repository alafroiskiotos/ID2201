-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
    0.

inc(_Name, T) ->
    T + 1.

merge(Ti, Tj) when Ti > Tj ->
    Ti;
merge(Ti, Tj) when Ti =< Tj->
    Tj.

leq(Ti, Tj) when Ti =< Tj ->
    true;
leq(Ti, Tj) when Ti > Tj ->
    false.

clock(Nodes) ->
    lists:foldl(fun(Node, Acc) -> [{Node, zero()} | Acc] end, [], Nodes).

update(Node, Time, Clock) ->
    TmpList = lists:keyreplace(Node, 1, Clock, {Node, Time}),
    lists:keysort(2, TmpList).

%% Clock is sorted
safe(Time, [{_Node, Low} | _Rest]) ->
    leq(Time, Low).
