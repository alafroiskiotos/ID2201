reply({{get, _, _}, File, _, _}) ->
            case file:read_file(File) of
                {ok, ContentBin} ->
                    Content =
                        unicode:characters_to_list(ContentBin, unicode),
                    http:ok(Content);
                {error, Reason} ->
                    io:format("Rudy: Read File ~w\n", [Reason])
            end.
