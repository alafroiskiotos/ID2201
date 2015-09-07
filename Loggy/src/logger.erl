-module(logger).
-export([start/2, stop/1, stop/2]).

start(Name, Nodes) -> register(Name, spawn_link(fun() -> init(Name, Nodes) end)).

stop(Name) -> Name ! stop.

stop(Name, After) ->
    timer:sleep(After),
    Name ! stop.

init(Name, Nodes) -> io:format("Logger: ~w is running!~n", [Name]),
    %% A list for every worker
    %% [{worker0, []}, {worker1, []}, ...]
    NodesList = lists:foldl(fun(X, Acc) -> [{X, []} | Acc] end, [], Nodes),
    loop(Nodes, NodesList, [], 0).

loop(Nodes, NodesList, QueueLengthList, NMsg) ->
    receive
        {log, From, Time, Msg} ->
            %% Returns the list of the respective worker with the new message
            NodesList1 = add(NodesList, From, Time, Msg),
            QueueLengthList0 = queueLength(Nodes, NodesList, QueueLengthList),
            %% Check if all nodes have at least one message
            ActiveList = lists:filter(fun({_, NList}) -> length(NList) > 0 end,
                NodesList1),
            if length(ActiveList) =:= length(NodesList) ->
                NodesList2 = log(Nodes, NodesList1),
                loop(Nodes, NodesList2, QueueLengthList0, NMsg + 1);
            true ->
                loop(Nodes, NodesList1, QueueLengthList0, NMsg + 1)
            end;
        {logw, From, Time, Msg} ->
            io:format("Logger: From: ~w Lamport Time: ~w Message: ~w~n",
                [From, Time,Msg]),
            loop(Nodes, NodesList, QueueLengthList, 0);
        stop -> printStats(QueueLengthList, NMsg),
            io:format("Logger stopped!~n"),
            ok;
        {stop, After} ->
            timer:sleep(After),
            printStats(QueueLengthList, NMsg),
            io:format("Logger stopped!~n"),
            ok
    end.

printStats(QueueLengthList, NMsg) ->
    SortedQueueL = lists:usort(QueueLengthList),
    [Min | _] = SortedQueueL,
    Max = lists:last(SortedQueueL),
    io:format("Number of messages received: ~w~n", [NMsg]),
    io:format("Min Queue Length: ~w~n", [Min]),
    io:format("Max Queue Length: ~w~n", [Max]).

%% Length of the message queue for all workers
queueLength(Nodes, NodesList, QueueLengthList) ->
    case Nodes of
        [] -> QueueLengthList;
        [Node | Rest] ->
            {_, {_, NodeList}} = lists:keysearch(Node, 1, NodesList),
            TmpList = lists:append(QueueLengthList, [length(NodeList)]),
            queueLength(Rest, NodesList, TmpList)
    end.

%% Remove printed elements from the node's message queue
removeElem(ToPrintElem, NodeList) ->
    case ToPrintElem of
        [] -> NodeList;
        [Head | Tail] ->
            NewNodeList = lists:delete(Head, NodeList),
            removeElem(Tail, NewNodeList)
    end.

getMinLamport(Nodes, NodesList, TmpList) ->
    case Nodes of
        [] -> [Min | _] = lists:usort(TmpList),
                Min;
        [Node | Rest] ->
            {_, {_, NodeList}} = lists:keysearch(Node, 1, NodesList),
            TmpList0 = lists:foldl(fun({_, Lamp, _}, Acc) -> [Lamp | Acc] end,
                [], NodeList),
            TmpList1 = lists:append(TmpList, TmpList0),
            getMinLamport(Rest, NodesList, TmpList1)
    end.

printWorkerLog(Nodes, NodesList, Time) ->
    case Nodes of
        [] -> NodesList;
        [Node | Rest] ->
            {_, {_, NodeList}} = lists:keysearch(Node, 1, NodesList),
            %% Print only those message whose Lamport Time is less than or
            %%equal to the current one
            ToPrintElem = lists:filtermap(fun({_, LTime, _}) -> LTime =< Time end,
                NodeList),
            lists:foreach(fun({FromP, TimeP, MsgP}) ->
                io:format("Logger: From: ~w Lamport Time: ~w Message: ~w~n", [FromP,
                TimeP, MsgP]) end,
            ToPrintElem),
            %% Remove ToPrintElem from Queue
            NewNList = removeElem(ToPrintElem, NodeList),
            %% Update NodesList
            NewNodesList = lists:keyreplace(Node, 1, NodesList, {Node, NewNList}),
            printWorkerLog(Rest, NewNodesList, Time)
    end.

%% NodeList = [{worker0, queue}, {worker1, queue}, ...]
%% Queue = {From, Time, Msg}
%%Msg = {received/sending, Rname, Msg}
log(Nodes, NodesList) ->
    %% Find the min of all worker list
    MinLamp = getMinLamport(Nodes, NodesList, []),
    NewNodesList = printWorkerLog(Nodes, NodesList, MinLamp),
    NewNodesList.

% Add a message to the respective message queue
add(NodesList, From, Time, Msg) ->
    {_, {_, NodeList}} = lists:keysearch(From, 1, NodesList),
    NewNodeList = lists:append([{From, Time, Msg}], NodeList),
    NodeList1 = lists:keyreplace(From, 1, NodesList, {From, NewNodeList}),
    NodeList1.
