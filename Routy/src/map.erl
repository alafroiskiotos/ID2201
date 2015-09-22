-module(map).

-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    [].

update(Node, Links, Map) ->
    case lists:keyfind(Node, 1, Map) of
	{_, _} ->
	    lists:keyreplace(Node, 1, Map, {Node, Links});
	false ->
	    [{Node, Links} | Map]
    end.


reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
	{_Name, Links} ->
	    Links;
	false ->
	    []
    end.

%% all_nodes(Map) ->
%%     lists:umerge(lists:flatten(Map)).

all_nodes(Map) ->
    all_nodes_helper(Map, []).

all_nodes_helper([], Nodes) ->
    lists:usort(Nodes);
all_nodes_helper([{Node, Links} | Rest], Nodes) ->
    TmpNodes = [Node | Nodes],
    TmpNodes2 = lists:foldl(fun(X, Acc) -> [X | Acc] end, TmpNodes, Links),
    all_nodes_helper(Rest, TmpNodes2).

