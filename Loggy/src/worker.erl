-module(worker).
-export([start/4, stop/1]).

start(Name, Logger, Sleep, Jitter) ->
    register(Name, spawn_link(fun() -> init(Name, Logger, 0, Sleep, Jitter) end)).

stop(Worker) ->
    Worker ! stop.

%peers(Worker, Peers) ->
%    Worker ! {peers, Peers}.

init(Name, Logger, Time, Sleep, Jitter) ->
    io:format("Worker: ~w is running!~n", [Name]),
    {A0, A1, A2} = now(),
    random:seed(A0, A1, A2),
    receive
        {peers, Peers} ->
            loop(Name, Logger, Time, Peers, Sleep, Jitter);
        stop -> ok
    end.

loop(Name, Logger, Time, Peers, Sleep, Jitter) ->
    Wait = random:uniform(Sleep),
    receive
        {msg, RName, RTime, Msg} ->
            NewTime = max(Time, RTime) + 1,
            %io:format("Name: ~w, Time: ~w, RTime: ~w, NewTime: ~w~n",
            %   [Name, Time, RTime, NewTime]),
            % Use 'logw' for messages without ordering
            Logger ! {log, Name, NewTime, {received, RName, Msg}},
            loop(Name, Logger, NewTime + 1, Peers, Sleep, Jitter);
        status ->
            io:format("Name: ~w~n", [Name]),
            io:format("Logger: ~w~n", [Logger]),
            io:format("Peers: ~w~n", [Peers]),
            io:format("Wait: ~w~n", [Wait]);
        stop -> io:format("Worker: ~w is stopped!~n", [Name]),
            ok;
        Error ->
            logger ! {log, Name, Time, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        Message = {hello, random:uniform(100)},
        Selected ! {msg, Name, Time + 1, Message},
        jitter(Jitter),
        % Use 'logw' for messages without ordering
        Logger ! {log, Name, Time + 1, {sending, Message}},
        loop(Name, Logger, Time + 1, Peers, Sleep, Jitter)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(Jitter) ->
    timer:sleep(random:uniform(Jitter)).
