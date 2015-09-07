-module(rudy).
-export([start/1, stop/0]).

start(Port) -> 
    register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
    exit(whereis(rudy), "Die bitch!").

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen),
            gen_tcp:close(Listen);
        {error, Reason} ->
            io:format("rudy: Error ~w\n", [Reason])
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            spawn(fun() -> request(Client) end),
            handler(Listen);
        {error, Reason} ->
            io:format("rudy: Error ~w\n", [Reason])
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Req = http:parse_request(Str),
            Response = reply(Req),
            gen_tcp:send(Client, Response);
        {error, Reason} ->
            io:format("rudy: Receive ~w\n", [Reason])
    end,
    gen_tcp:close(Client).
       
%reply1({{get, _, _}, _, _, _}) ->
%    http:ok(" ").

reply({{get, _, _}, File, _, _}) ->
            case file:read_file(File) of
                {ok, ContentBin} ->
                    Content = unicode:characters_to_list(ContentBin, unicode),
                    http:ok(Content);
                {error, Reason} -> io:format("Rudy: Read File: ~w\n", [Reason])
            end.
