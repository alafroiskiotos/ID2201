update_all(Reachable, N, Gateway, List) ->
    case Reachable of
        [] -> List;
        [H | T] ->
            TmpList = update(H, N, Gateway, List),
            update_all(T, N, Gateway, TmpList)
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
                    Slist0 = update_all(Reachable, LstN + 1, LstGW, Tail),
                    Table0 = lists:append(Table, [{LstNode, LstGW}]),
                    iterate(sort(Slist0), Map, Table0)
            end
    end.
