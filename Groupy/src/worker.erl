-module(worker).
-export([start/4, start/5, stop/1]).

-define(change, 20).
-define(colour, {0,0,0}).

start(Id, Name, Module, Sleep) ->
    register(Name, spawn(fun() -> init(Id, Module, Sleep) end)).

init(Id, Module, Sleep) ->
    {ok, Cast} = apply(Module, start, [Id]),
    Colour = ?colour,
    init_cont(Id, Cast, Colour, Sleep).

start(Id, Name, Module, Peer, Sleep) ->
    register(Name, spawn(fun() -> init(Id, Module, Peer, Sleep) end)).

init(Id, Module, Peer, Sleep) ->
    {ok, Cast} = apply(Module, start, [Id, Peer]),
    {ok, Colour} = join(Id, Cast),
    init_cont(Id, Cast, Colour, Sleep).

stop(Node) ->
    Node ! stop,
    unregister(Node).

join(Id, Cast) ->
    receive
        {view, _} ->
            Ref = make_ref(),
            Cast ! {mcast, {state_request, Ref}},
            state(Id, Ref);
        {error, Reason} ->
            {error, Reason}
    end.

state(Id, Ref) ->
    receive
        {state_request, Ref} ->
            receive
                {state, Ref, Colour} ->
                    {ok, Colour}
            end;
        _Ignore ->
            state(Id, Ref)
    end.

init_cont(Id, Cast, Colour, Sleep) ->
    {A0, A1, A2} = now(),
    random:seed(A0, A1, A2),
    Title = "Worker: " ++ integer_to_list(Id),
    Gui = gui:start(Title, self()),
    Gui ! {colour, Colour},
    worker(Id, Cast, Colour, Gui, Sleep),
    Cast ! stop,
    Gui ! stop.

worker(Id, Cast, Colour, Gui, Sleep) ->
    Wait = if Sleep == 0 -> 0; true -> random:uniform(Sleep) end,
    receive
        {change, N} ->
            Colour2 = change_colour(N, Colour),
            Gui ! {colour, Colour2},
            worker(Id, Cast, Colour2, Gui, Sleep);
        {state_request, Ref} ->
            Cast ! {mcast, {state, Ref, Colour}},
            worker(Id, Cast, Colour, Gui, Sleep);
        {state, _, _} ->
            worker(Id, Cast, Colour, Gui, Sleep);
        {join, Peer, Gms} ->
            Cast ! {join, Peer, Gms},
            worker(Id, Cast, Colour, Gui, Sleep);
        {view, _} ->
            worker(Id, Cast, Colour, Gui, Sleep);
        stop ->
            ok;
        Error ->
            io:format("Worker: Error message: ~w~n", [Error]),
            worker(Id, Cast, Colour, Gui, Sleep)
    after Wait ->
        Cast ! {mcast, {change, random:uniform(?change)}},
        worker(Id, Cast, Colour, Gui, Sleep)
    end.

%% Rotate RGB
change_colour(N, {R, G, B}) ->
    {G, B, ((R + N) rem 256)}.
