-module(node1).
-export([start/1, start/2]).

-define(stabilize, 100).
-define(timeout, 10000).

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
connect(_, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, SKey} ->
            {ok, {SKey, Peer}}
    after ?timeout ->
        io:format("Time out: no response~n")
    end.

node(Id, Predecessor, Successor) ->
    receive
        %% A Peer wants to know our key
        {key, Qref, Peer} ->
            %io:format("Key request! Qref: ~w - Peer: ~w~n", [Qref, Peer]),
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        %% A New node informs us of its existence
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            %io:format("Notify request! New: ~w - Pred: ~w~n", [New, Pred]),
            node(Id, Pred, Successor);
        %% A Predecessor wants to know our predecessor
        {request, Peer} ->
            %io:format("Request request! Peer ~w~n", [Peer]),
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        %% Our successor informs us of its Predecessor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            %io:format("Status request! Pred: ~w - Succ: ~w~n", [Pred, Succ]),
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
        status ->
            io:format("Node ID: ~w~n", [Id]),
            io:format("Predecessor: ~w~n", [Predecessor]),
            io:format("Successor: ~w~n", [Successor]),
            node(Id, Predecessor, Successor);
        stop ->
            io:format("Receivied Stop command~n"),
            ok
    end.

create_probe(Id, {_, SPid}) ->
    SPid ! {probe, Id, [Id], erlang:now()}.

forward_probe(Ref, T, Nodes, Id, {_, SPid}) ->
    SPid ! {probe, Ref, lists:append(Nodes, [Id]), T}.

remove_probe(T, Nodes) ->
    Diff = timer:now_diff(erlang:now(), T),
    %lists:foreach(fun(Node) -> io:format("Probe: ~w~n", [Node]) end, Nodes),
    io:format("Total hops: ~w~n", lists:flatlength(Nodes)),
    io:format("Total trip time: ~w microseconds~n", [Diff]).

stabilize({_, SPid}) ->
    SPid ! {request, self()}.

%% Pred: Our successor's current predecessor
stabilize(Pred, Id, Successor) ->
    {SKey, SPid} = Successor,
    case Pred of
        nil ->
            SPid ! {notify, {Id, self()}},
            Successor;
        {Id, _} ->
            Successor;
        {SKey, _} ->
            SPid ! {notify, {Id, self()}},
            Successor;
        {XKey, XPid} ->
            %%io:format("Is ~w between ~w and ~w? ", [XKey, Id, SKey]),
            case key:between(XKey, Id, SKey) of
                true ->
                    %%io:format("Yes it is!~n"),
                    %% Request being predecessor (aka call stabilize)
                    XPid ! {request, self()},
                    Pred;
                false ->
                    %%io:format("No it is not!~n"),
                    SPid ! {notify, {Id, self()}},
                    Successor
            end
    end.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {PKey, PPid} ->
            Peer ! {status, {PKey, PPid}}
    end.

notify({NKey, NPid}, Id, Predecessor) ->
    case Predecessor of
        nil ->
            {NKey, NPid};
        {PKey, _} ->
            %%io:format("Is ~w between ~w and ~w? ", [NKey, PKey, Id]),
            case key:between(NKey, PKey, Id) of
                true ->
                    %%io:format("Yes it is!~n"),
                    {NKey, NPid};
                false ->
                    %%io:format("No it is not!~n"),
                    Predecessor
            end
    end.

schedule_stabilize() ->
    timer:send_interval(?stabilize, self(), stabilize).
