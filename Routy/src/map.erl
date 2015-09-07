-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1, map/0]).

%%map() -> [{paris, [madrid, rome]}, {madrid, [berlin]}].
map() -> [{paris, [berlin, athens]}, {berlin, [stockholm]}, {athens, [rome]}].

new() -> [].

update(Node, Link, Map) ->
    TmpMap = lists:foldl(fun({LstNode, LstLink}, List) ->
            if Node == LstNode -> List;
                true -> lists:append(List, [{LstNode, LstLink}])
            end
        end, [], Map),
        lists:append(TmpMap, [{Node, Link}]).

reachable(Node, Map) ->
    case lists:keysearch(Node, 1, Map) of
        {_, {_, ReLink}} -> ReLink;
        false -> []
    end.

all_nodes(Map) ->
    lists:usort(lists:foldl(fun({LstNode, LstLink}, Acc) -> Flat = lists:append([LstNode], LstLink),
        lists:append(Acc, Flat) end, [], Map)).
