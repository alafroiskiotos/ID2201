-module(test0).
-export([start/1, stop/1]).

start(Module) ->
    worker:start(0, l, Module, 1000),
    timer:sleep(100),
    worker:start(1, w0, Module, l, 1000),
    timer:sleep(100),
    worker:start(2, w1, Module, w0, 1000),
    timer:sleep(100),
    worker:start(3, w2, Module, w1, 1000),
    timer:sleep(100),
    worker:start(4, w3, Module, w2, 1000),
    timer:sleep(100),
    worker:start(5, w4, Module, w3, 1000),
    timer:sleep(100),
    worker:start(6, w5, Module, w4, 1000),
    timer:sleep(100),
    worker:start(7, w6, Module, w5, 1000).
stop(Node) ->
    worker:stop(Node).
