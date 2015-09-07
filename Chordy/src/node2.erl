-module(node2).
-export([start/1, start/2]).

-define(stabilize, 100).
-define(timeout, 10000).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(key:generate(), Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, storage:create()).

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

node(Id, Predecessor, Successor, Storage) ->
    receive
        %% A Peer wants to know our key
        {key, Qref, Peer} ->
            %io:format("Key request! Qref: ~w - Peer: ~w~n", [Qref, Peer]),
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Storage);
        %% A New node informs us of its existence
        {notify, New} ->
            {Pred, NStorage} = notify(New, Id, Predecessor, Storage),
            %io:format("Notify request! New: ~w - Pred: ~w~n", [New, Pred]),
            node(Id, Pred, Successor, NStorage);
        %% A Predecessor wants to know our predecessor
        {request, Peer} ->
            %io:format("Request request! Peer ~w~n", [Peer]),
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Storage);
        %% Our successor informs us of its Predecessor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            %io:format("Status request! Pred: ~w - Succ: ~w~n", [Pred, Succ]),
            node(Id, Predecessor, Succ, Storage);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Storage);
        probe ->
            create_probe(Id, Successor, Storage),
            node(Id, Predecessor, Successor, Storage);
        {probe, Id, Size, Nodes, T} ->
            remove_probe(T, Nodes, Size),
            node(Id, Predecessor, Successor, Storage);
        {probe, Ref, Size, Nodes, T} ->
            forward_probe(Ref, T, Size, Nodes, Id, Successor, Storage),
            node(Id, Predecessor, Successor, Storage);
        {add, Key, Value, Qref, Client} ->
            %%io:format("Node: ~w Received add request~n", [Id]),
            NewStorage = add(Key, Value, Qref, Client, Id, Predecessor,
                Successor, Storage),
            node(Id, Predecessor, Successor, NewStorage);
        {lookup, Key, Qref, Client} ->
            %io:format("Node: ~w Received lookup request for key ~w~n", [Id, Key]),
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Storage),
            node(Id, Predecessor, Successor, Storage);
        {handover, Elements} ->
            MergedStorage = storage:merge(Storage, Elements),
            node(Id, Predecessor, Successor, MergedStorage);
        status ->
            io:format("Node ID: ~w~n", [Id]),
            io:format("Predecessor: ~w~n", [Predecessor]),
            io:format("Successor: ~w~n", [Successor]),
            io:format("Storage: "),
            lists:foreach(fun(Element) -> io:format("~w", [Element]) end, Storage),
            io:format("~n"),
            node(Id, Predecessor, Successor, Storage);
        stop ->
            io:format("Receivied Stop command~n"),
            ok
    end.

add(Key, Value, Qref, Client, Id, {PKey, _}, {SKey, SPid}, Storage) ->
    case key:between(Key, PKey, Id) of
        true ->
            Client ! {Qref, ok},
            %%io:format("Node: ~w I am responsible for that element~n", [Id]),
            storage:add(Key, Value, Storage);
        false ->
            %%io:format("Node: ~w I am NOT responsible for that element,
            %%    forwarding request to ~w~n", [Id, SKey]),
            SPid ! {add, Key, Value, Qref, Client},
            Storage
    end.

lookup(Key, Qref, Client, Id, {PKey, _}, {SKey, SPid}, Storage) ->
    case key:between(Key, PKey, Id) of
        true ->
            %io:format("Node: ~w I have value for key ~w~n", [Id, Key]),
            Result = storage:lookup(Key, Storage),
            Client ! {Qref, Result};
        false ->
            %io:format("Node: ~w I do NOT have value for key ~w, forwarding
            %    request to ~w~n", [Id, Key, SKey]),
            SPid ! {lookup, Key, Qref, Client}
    end.

create_probe(Id, {_, SPid}, Storage) ->
    Size = storage:size(Storage),
    SPid ! {probe, Id, Size, [Id], erlang:now()}.

forward_probe(Ref, T, Size, Nodes, Id, {_, SPid}, Storage) ->
    MySize = storage:size(Storage),
    NewSize = MySize + Size,
    SPid ! {probe, Ref, NewSize, lists:append(Nodes, [Id]), T}.

remove_probe(T, Nodes, Size) ->
    Diff = timer:now_diff(erlang:now(), T),
    io:format("Total size: ~w~n", [Size]),
    %lists:foreach(fun(Node) -> io:format("Probe: ~w~n", [Node]) end, Nodes),
    io:format("Hops: ~w~n", [lists:flatlength(Nodes)]),
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

handover(Storage, NKey, NPid) ->
    {LeaveStorage, KeepStorage} = storage:split(NKey, Storage),
    NPid ! {handover, LeaveStorage},
    KeepStorage.

notify({NKey, NPid}, Id, Predecessor, Storage) ->
    case Predecessor of
        nil ->
            KeepStorage = handover(Storage, NKey, NPid),
            {{NKey, NPid}, KeepStorage};
        {PKey, _} ->
            %%io:format("Is ~w between ~w and ~w? ", [NKey, PKey, Id]),
            case key:between(NKey, PKey, Id) of
                true ->
                    %%io:format("Yes it is!~n"),
                    KeepStorage = handover(Storage, NKey, NPid),
                    {{NKey, NPid}, KeepStorage};
                false ->
                    %%io:format("No it is not!~n"),
                    {Predecessor, Storage}
            end
    end.

schedule_stabilize() ->
    timer:send_interval(?stabilize, self(), stabilize).
