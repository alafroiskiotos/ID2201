-module(gms2).
-export([start/1, start/2]).

-define(timeout, 300).
-define(failure, 200).

start(Id) ->
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Self) end)}.

init(Id, Master) ->
    {A0, A1, A2} = now(),
    random:seed(A0, A1, A2),
    leader(Id, Master, [], [Master]).

start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader | Slaves], Group} ->
            Master ! {view, Group},
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Slaves, Group)
    after ?timeout ->
        Master ! {error, "NO reply from the Leader"}
    end.

slave(Id, Master, Leader, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            %%io:format("gms ~w: received {mcast, ~w} ~n", [Id, Msg]),
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} ->
            %%io:format("gms ~w: forward join from ~w to leader~n", [Id, Peer]),
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} ->
            %%io:format("gms ~w: deliver msg ~w ~n", [Id, Msg]),
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader | Slaves2], Group2} ->
            %%io:format("gms ~w: received view ~w ~w~n", [Id, N, View]),
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, Reason} ->
            election(Id, Master, Slaves, Group),
            io:format("Leader is DEAD with ~w~n", [Reason]);
        stop ->
            ok;
        Error ->
            io:format("gms ~w: slave, Error message ~w~n", [Id, Error])
    end.

%%[_ | Group] -> Group
election(Id, Master, Slaves, [_ | Group]) ->
    Self = self(),
    case Slaves of
        [Self | Rest] ->
            bcast(Id, {view, Slaves, Group}, Rest),
            Master ! {view, Group},
            io:format("Your new Leader is: ~w~n", [Self]),
            leader(Id, Master, Rest, Group);
        [Leader | Rest] ->
            erlang:monitor(process, Leader),
            io:format("Your new Leader is ~w~n", [Leader]),
            slave(Id, Master, Leader, Rest, Group)
    end.

leader(Id, Master, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            %%io:format("gms ~w: received {mcast, ~w} ~n", [Id, Msg]),
            bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} ->
            %%io:format("gms ~w: forward join from ~w to master~n", [Id, Peer]),
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self() | Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        stop ->
            ok;
        Error ->
            io:format("gms ~w: leader: error message: ~w~n", [Id, Error])
    end.

bcast(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
    case random:uniform(?failure) of
        ?failure ->
            io:format("leader ~w: Crashed! ~n", [Id]),
            exit(i_see_dead_leaders);
        _ ->
            ok
    end.
