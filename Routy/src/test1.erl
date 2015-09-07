-module(test1).
-export([start/0, stop/0]).

ip() -> 'sweden@127.0.0.1'.

start() ->
    routy:start(stockholm),
    io:format("[Test] Started router 'stockholm'~n", []),
    routy:start(boras),
    io:format("[Test] Started router 'boras'~n", []),
    routy:start(lulea),
    io:format("[Test] Started router 'lulea'~n", []),
    routy:start(umea),
    io:format("[Test] Started router 'umea'~n", []),
    routy:start(lund),
    io:format("[Test] Started router 'lund'~n", []),

    stockholm ! {add, boras, {boras, ip()}},
    io:format("[Test] Added 'boras' to 'stockholm'~n", []),
    stockholm ! {add, lulea, {lulea, ip()}},
    io:format("[Test] Added 'lulea' to 'stockholm'~n", []),
    boras ! {add, stockholm, {stockholm, ip()}},
    io:format("[Test] Added 'stockholm' to 'boras'~n", []),
    boras ! {add, lund, {lund, ip()}},
    io:format("[Test] Added 'lund' to 'boras'~n", []),
    lulea ! {add, stockholm, {stockholm, ip()}},
    io:format("[Test] Added 'stockholm' to 'lulea'~n", []),
    lulea ! {add, umea, {umea, ip()}},
    io:format("[Test] Added 'umea' to 'lulea'~n", []),
    lund ! {add, boras, {boras, ip()}},
    io:format("[Test] Added 'boras' to 'lund'~n", []),
    lund ! {add, umea, {umea, ip()}},
    io:format("[Test] Added 'umea' to 'lund'~n", []),

    stockholm ! broadcast,
    timer:sleep(100),
    boras ! broadcast,
    timer:sleep(100),
    lulea ! broadcast,
    timer:sleep(100),
    lund ! broadcast,
    timer:sleep(100),
    umea ! broadcast,
    timer:sleep(100),

    stockholm ! update,
    timer:sleep(100),
    boras ! update,
    timer:sleep(100),
    lulea ! update,
    timer:sleep(100),
    lund ! update,
    timer:sleep(100),
    umea ! update,
    timer:sleep(100),

    stockholm ! {status, self()},
    timer:sleep(100),
    boras ! {status, self()},
    timer:sleep(100),
    lund ! {status, self()},
    timer:sleep(100),
    umea ! {status, self()},
    timer:sleep(100),
    lulea ! {status, self()},
    timer:sleep(100).

stop() ->
    routy:stop(stockholm),
    routy:stop(boras),
    routy:stop(lund),
    routy:stop(umea),
    routy:stop(lulea).
