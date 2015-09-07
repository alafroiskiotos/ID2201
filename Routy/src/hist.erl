-module(hist).
-export([new/1, update/3]).

new(Name) -> [{Name, 0}].

update(Node, N, History) ->
    case lists:keysearch(Node, 1, History) of
        {_, {HisNode, HisN}} ->
            if
                N < HisN ->
                    {new, lists:keyreplace(HisNode, 1, History, {Node, N})};
                true -> old
            end;
        false ->
            NewHistory = lists:append(History, new(Node)),
            {new, NewHistory}
    end.
