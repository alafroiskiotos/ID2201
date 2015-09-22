-module(setup).

-export([start_greece/0, stop_greece/0, start_sweden/0, stop_sweden/0]).

start_greece() ->
    io:format("Starting athens~n"),
    routy:start(athens),
    io:format("Starting patra~n"),
    routy:start(patra),
    io:format("Starting herakleio~n"),
    routy:start(herakleio),
    athens ! {add, patra, {patra, node()}},
    athens ! {add, herakleio, {herakleio, node()}},
    herakleio ! {add, athens, {athens, node()}},
    patra ! {add, athens, {athens, node()}},
    athens ! broadcast,
    timer:sleep(1000),
    herakleio ! broadcast,
    timer:sleep(1000),
    patra ! broadcast,
    timer:sleep(1000),
    athens ! update,
    timer:sleep(1000),
    patra ! update,
    timer:sleep(1000),
    herakleio ! update.

stop_greece() ->
    athens ! stop,
    patra ! stop,
    herakleio ! stop.

start_sweden() ->
    io:format("Starting stockholm~n"),
    routy:start(stockholm),
    io:format("Starting umea~n"),
    routy:start(umea),
    io:format("Starting lulea~n"),
    routy:start(lulea),
    stockholm ! {add, umea, {umea, node()}},
    umea ! {add, stockholm, {stockholm, node()}},
    umea ! {add, lulea, {lulea, node()}},
    lulea ! {add, umea, {umea, node()}},
    stockholm ! broadcast,
    timer:sleep(1000),
    umea ! broadcast,
    timer:sleep(1000),
    lulea ! broadcast,
    timer:sleep(1000),
    stockholm ! update,
    timer:sleep(1000),
    umea ! update,
    timer:sleep(1000),
    lulea ! update.

stop_sweden() ->
    stockholm ! stop,
    umea ! stop,
    lulea ! stop.
