-module(intf).

-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->
    [].

add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid} | Intf].

remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
	{Name, _Pref, Pid} ->
	    {ok, Pid};
	false ->
	    notfound
    end.
    
name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) of
	{Name, Ref, _Pid} ->
	    {ok, Name};
	false ->
	    notfound
    end.

ref(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
	{Name, Ref, _Pid} ->
	    {ok, Ref};
	false ->
	    notfound
    end.

list(Intf) ->
    lists:foldl(fun({Name, _Ref, _Pid}, Acc) -> [Name | Acc] end, [], Intf).

broadcast(Message, Intf) ->
    Names = list(Intf),
    lists:map(fun(X) -> send(Message, lookup(X, Intf)) end, Names),
    ok.

%% private functions
send(Msg, {ok, Destination}) ->
    Destination ! Msg.
