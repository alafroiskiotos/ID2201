-module(worker).

-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
	{peers, Peers} ->
	    loop(Name, Log, Peers, Sleep, Jitter, time:zero());
	stop ->
	    ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, LampTime) ->
    Wait = random:uniform(Sleep),
    receive
	{msg, Time, Msg} ->
	    NewLampTime = time:inc(Name, time:merge(LampTime, Time)),
	    Log ! {log, Name, NewLampTime, {received, Msg}},
	    loop(Name, Log, Peers, Sleep, Jitter, NewLampTime);
	stop ->
	    ok;
	Error ->
	    Log ! {log, Name, time, {error, Error}}
    after Wait ->
	    Selected = select(Peers),
	    NewLampTime = time:inc(Name, LampTime),
	    Message = {hello, random:uniform(500)},
	    Selected ! {msg, NewLampTime, Message},
	    jitter(Jitter),
	    Log ! {log, Name, NewLampTime, {sending, Message}},
	    loop(Name, Log, Peers, Sleep, Jitter, NewLampTime)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) ->
    ok;
jitter(Jitter) ->
    timer:sleep(random:uniform(Jitter)).
