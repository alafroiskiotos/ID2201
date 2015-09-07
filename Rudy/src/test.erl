-module(test).
-export([bench/3]).

bench({A, B, C, D}, Port, No) ->
    Start = now(),
    run(No, {A, B, C, D}, Port),
    Finish = now(),
    timer:now_diff(Finish, Start).

%bench2({A, B, C, D}, Port) ->
%    Start = now(),
%    run(100, Host, Port),
%    Finish = now(),
%    timer:now_diff(Finish, Start).

run(N, {A, B, C, D}, Port) ->
    if
        N == 0 ->
            ok;
        true ->
            request({A, B, C, D}, Port),
            %%spawn(fun() -> request(Host, Port) end),
            run(N-1, {A, B, C, D}, Port)
    end.

request({A, B , C, D}, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Server} = gen_tcp:connect({A, B, C, D}, Port, Opt),
    gen_tcp:send(Server, http:get("/www/index.htm")),
    Recv = gen_tcp:recv(Server, 0),
    case Recv of
        {ok, _} ->
            ok;
        {error, Error} ->
            io:format("test: error: ~w~n", [Error])
    end.
