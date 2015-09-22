-module(hist).

-export([new/1, update/3]).

new(Name) ->
    [{Name, 0}].

update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
	{Node, Nh} ->
	    case N =< Nh of
		true ->
		    old;
		false ->
		    {new, lists:keyreplace(Node, 1, History, {Node, N})}
	    end;
	false ->
	    {new, [{Node, 0} | History]}
    end.
	    
