-module(http).
-export([parse_request/1, ok/1, get/1]).

parse_request(Req0) ->
    {Request, File, Req1} = request_line(Req0),
    {Headers, Req2} = request_headers(Req1),
    {Body, _} = request_body(Req2),
    {Request, File, Headers, Body}.

request_line([$G, $E, $T, 32 | Req0]) ->
    {URI, Req1} = request_uri(Req0),
    File = parse_uri(URI),
    {Ver, Req2} = request_version(Req1),
    [13, 10 | Req3] = Req2,
    {{get, URI, Ver}, File, Req3}.

% URI Parser
request_uri([32 | R0]) -> {[], R0};
request_uri([C | R0]) ->
    {Head, R1} = request_uri(R0),
    {[C | Head], R1}.
parse_uri([ 47 | Rest]) -> Rest.

% Version Parser
request_version([$H, $T, $T, $P, $/, $1, $., $0 | Req]) ->
    {v10, Req};
request_version([$H, $T, $T, $P, $/, $1, $., $1 | Req]) ->
    {v11, Req}.

% Headers parser
request_headers([13, 10 | Req0]) ->
    {[], Req0};
request_headers(Req0) ->
    {Header, Req1} = header(Req0),
    {Rest, Req2} = request_headers(Req1),
    {[Header | Rest], Req2}.

header([13, 10 | Req0]) ->
    {[], Req0};
header([C | Req0]) ->
    {Rest, Req1} = header(Req0),
    {[C | Rest], Req1}.

% Body parser
request_body(Req0) ->
    {Req0, []}.

ok(Body) -> "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.
get(URI) -> "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".
