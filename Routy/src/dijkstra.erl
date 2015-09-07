-module(dijkstra).
-export([table/2, route/2]).

sort(List) -> lists:keysort(2, List).

entry(Node, Slist) ->
    TmpList0 = lists:filter(fun({LstNode, _, _}) ->
        (LstNode =:= Node)
        end, Slist),
    case lists:keysearch(Node, 1, sort(TmpList0)) of
        {_, {_, N, _}} -> N;
        false -> 0
    end.

replace(Node, N, Gateway, Slist) ->
    TmpList0 = lists:filter(fun({LstNode, _, _}) ->
        (LstNode =:= Node)
        end, Slist),
    {_, Tuple} = lists:keysearch(Node, 1, TmpList0),
    sort(lists:append(lists:delete(Tuple, Slist), [{Node, N, Gateway}])).

update(Node, N, Gateway, Slist) ->
    CurHop = entry(Node, Slist),
    if
        N < CurHop ->
            replace(Node, N, Gateway, Slist);
        true -> sort(Slist)
    end.

update_all(Reachable, N, Gateway, List) ->
    case Reachable of
        [] -> List;
        [H | T] ->
            TmpL = update(H, N, Gateway, List),
            update_all(T, N, Gateway, TmpL)
    end.
    
iterate(Slist, Map, Table) ->
    case Slist of
        [] -> Table;
        [Head | Tail] ->
            case [Head] of
                [{}] -> Table;
                [{_, inf, _}] -> Table;
                [{LstNode, LstN, LstGW}] ->
                    Reachable = map:reachable(LstNode, Map),
                    NewL = update_all(Reachable, LstN + 1, LstGW, Tail),
                    Table0 = lists:append(Table, [{LstNode, LstGW}]),
                    iterate(sort(NewL), Map, Table0)
            end
    end.

table(GW, Map) ->
    GWlist = lists:foldl(fun(Gw, Acc) -> lists:append(Acc, [{Gw, 0, Gw}]) end,
        [], GW),
    Maplist = lists:foldl(fun(Node, Acc) -> lists:append(Acc, [{Node, inf,
        unknown}]) end, [], map:all_nodes(Map)),
    List = lists:append(GWlist, Maplist),
    iterate(sort(List), Map, []).

route(Node, Table) ->
    case lists:keysearch(Node, 1, Table) of
        {_, {_, GW}} -> {ok, GW};
        false -> notfound
    end.
