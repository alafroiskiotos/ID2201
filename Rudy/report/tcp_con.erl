init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen),
            gen_tcp:close(Listen);
        {error, Reason} ->
            io:format("rudy: Listen Error ~w\n", [Reason])
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} -> request(Client),
            handler(Listen);
        {error, Reason} ->
            io:format("rudy: Accept Error ~w\n", [Reason])
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Req = http:parse_request(Str),
            Response = reply(Req),
            gen_tcp:send(Client, Response);
        {error, Reason} ->
            io:format("rudy: Receive Error ~w\n", [Reason])
    end,
    gen_tcp:close(Client).
