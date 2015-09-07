-module(test0).
-export([run/2]).

run(Sleep, Jitter) ->
    logger:start(loggy, [worker0, worker1, worker2, worker3]),
    worker:start(worker0, loggy, Sleep, Jitter),
    worker:start(worker1, loggy, Sleep, Jitter),
    worker:start(worker2, loggy, Sleep, Jitter),
    worker:start(worker3, loggy, Sleep, Jitter),

    worker0 ! {peers, [worker1, worker2, worker3]},
    worker1 ! {peers, [worker0, worker2, worker3]},
    worker2 ! {peers, [worker1, worker0, worker3]},
    worker3 ! {peers, [worker1, worker2, worker0]},

    timer:sleep(10000),

    loggy ! {stop, 6000},
    worker0 ! stop,
    worker1 ! stop,
    worker2 ! stop,
    worker3 ! stop.
