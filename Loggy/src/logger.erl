-module(logger).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    loop(time:clock(Nodes), []).

loop(Clock, HoldBackQ) ->
    receive
	{log, From, Time, Msg} ->
	    NewClock = time:update(From, Time, Clock),
	    UpdatedHoldBackQ = [{From, Time, Msg} | HoldBackQ],
	    NewHoldBackQ = checkQ(UpdatedHoldBackQ, NewClock, []),
	    loop(NewClock, NewHoldBackQ);	 
	stop ->
	    io:format("HoldBackQ length: ~w~n", [length(HoldBackQ)]),
	    io:format("Flushing hold-back queue~n"),
	    lists:foreach(fun({From, Time, Msg}) -> log(From, Time, Msg) end, HoldBackQ),
	    ok
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

checkQ([], _Clock, NewHoldBackQ) ->
    NewHoldBackQ;
checkQ([{From, Time, Msg} | Rest], Clock, NewHoldBackQ) ->
    case time:safe(Time, Clock) of
	true ->
	    log(From, Time, Msg),
	    checkQ(Rest, Clock, NewHoldBackQ);
	false ->
	    checkQ(Rest, Clock, [{From, Time, Msg} | NewHoldBackQ])
    end.
