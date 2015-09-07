-module(key).
-export([generate/0, between/3]).

generate() ->
    {A0, A1, A2} = now(),
    random:seed(A0, A1, A2),
    random:uniform(10000000000).

between(Key, From, To) ->
    case From =< To of
        true ->
            if
                ((Key > From) and (Key =< To)) ->
                    true;
                (From == To) ->
                    true;
                true ->
                    false
            end;
        false ->
            if
                ((Key > From) or (Key =< To)) ->
                    true;
                true ->
                    false
            end
    end.
