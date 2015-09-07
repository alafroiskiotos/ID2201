-module(test).
-export([start/0, stop/0]).

ip() -> 'sweden@127.0.0.1'.

start() ->
    routy:start(stockholm),
    io:format("[Test] Started router 'stockholm'~n", []),
    routy:start(boras),
    io:format("[Test] Started router 'boras'~n", []),
    routy:start(lund),
    io:format("[Test] Started router 'lund'~n", []),

    stockholm ! {add, boras, {boras, ip()}},
    io:format("[Test] Added 'boras' to 'stockholm'~n", []),
    boras ! {add, stockholm, {stockholm, ip()}},
    io:format("[Test] Added 'stockholm' to 'boras'~n", []),
    boras ! {add, lund, {lund, ip()}},
    io:format("[Test] Added 'lund' to 'boras'~n", []),
    lund ! {add, boras, {boras, ip()}},
    io:format("[Test] Added 'boras' to 'lund'~n", []),
    stockholm ! {add, lund, {lund, ip()}},
    io:format("[Test] Added 'lund' to 'stockholm'~n", []),
    lund ! {add, stockholm, {stockholm, ip()}},
    io:format("[Test] Added 'stockholm' to 'lund'~n", []),

  %stockholm ! {add, kumla, {kumla, andraip()}},
  %io:format("[Test] Added 'stockholm' to 'kumla'~n", []),

    stockholm ! broadcast,
    timer:sleep(100),
    boras ! broadcast,
    timer:sleep(100),
    lund ! broadcast,
    timer:sleep(100),

    stockholm ! update,
    timer:sleep(100),
    boras ! update,
    timer:sleep(100),
    lund ! update,

    

    stockholm ! {status, self()},
    timer:sleep(100),
    boras ! {status, self()},
    timer:sleep(100),
    lund ! {status, self()},
    timer:sleep(100).

stop() ->
    routy:stop(stockholm),
    routy:stop(boras),
    routy:stop(lund).
