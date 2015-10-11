-module(node1).

-export([start/1, start/2]).

-define(STABILIZE, 100).
-define(TIMEOUT, 1000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(_Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
	{Qref, Skey} ->
	    {ok, {Skey, Peer}}
    after ?TIMEOUT ->
	io:format("Timeout: no reponse!~n")
    end.

schedule_stabilize() ->
    timer:send_interval(?STABILIZE, self(), stabilize).

node(Id, Predecessor, Successor) ->
    receive
	%% A peer needs to know our key
	{key, Qref, Peer} ->
	    Peer ! {Qref, Id},
	    node(Id, Predecessor, Successor);
	%% A new node informs us of its existence
	{notify, New} ->
	    Pred = notify(New, Id, Predecessor),
	    node(Id, Pred, Successor);
	%% A predecessor needs to know our predecessor
	{request, Peer} ->
	    request(Peer, Predecessor),
	    node(Id, Predecessor, Successor);
	%% Our successor informs us about its predecessor
	{status, Pred} ->
	    Succ = stabilize(Pred, Id, Successor),
	    node(Id, Predecessor, Succ);
	stabilize ->
	    stabilize(Successor),
	    node(Id, Predecessor, Successor);
	probe ->
	    create_probe(Id, Successor),
	    node(Id, Predecessor, Successor);
	{probe, Id, Nodes, T} ->
	    remove_probe(T, Nodes),
	    node(Id, Predecessor, Successor);
	{probe, Ref, Nodes, T} ->
	    forward_probe(Ref, T, Nodes, Id, Successor),
	    node(Id, Predecessor, Successor);
	state ->
	    io:format("ID: ~w~n", [Id]),
	    io:format("Predecessor: ~p, Successor: ~p~n", [Predecessor, Successor]),
	    node(Id, Predecessor, Successor);
	stop ->
	    ok
    end.

create_probe(Id, {_Skey, Spid}) ->
    Spid ! {probe, Id, [Id], erlang:now()}.

forward_probe(Ref, T, Nodes, Id, {_Skey, Spid}) ->
    Spid ! {probe, Ref, [Id | Nodes], T}.

remove_probe(T, Nodes) ->
    Diff = timer:now_diff(erlang:now(), T),
    io:format("Route: ~p~n", [lists:reverse(Nodes)]),
    io:format("Trip time: ~w micro~n", [Diff]).

request(Peer, Predecessor) ->
    case Predecessor of
	nil ->
	    Peer ! {status, nil};
	{Pkey, Ppid} ->
	    Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
	nil ->
	    {Nkey, Npid};
	{Pkey, _Ppid} ->
	    case key:between(Nkey, Pkey, Id) of
		true ->
		    {Nkey, Npid};
		false ->
		    Predecessor
	    end
    end.

stabilize({_Skey, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
	nil ->
	    %% Inform the node of our existence
	    Spid ! {notify, {Id, self()}},
	    Successor;
	{Id, _} ->
	    %% It is pointing back to us, do nothing
	    Successor;
	{Skey, Spid} ->
	    %% It is pointing to itself, notify the node of our existence
	    Spid ! {notify, {Id, self()}},
	    Successor;
	{Xkey, Xpid} ->
	    case key:between(Xkey, Id, Skey) of
		true ->
		    %% Adopt that node as our successor and stabilize again
		    Xpid ! {request, self()},
		    Pred;
		false ->
		    Spid ! {notify, {Id, self()}},
		    Successor
	    end
    end.
    
