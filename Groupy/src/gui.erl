-module(gui).
-export([start/2]).
-include_lib("wx/include/wx.hrl").

-define(width, 200).
-define(height, 200).

start(Title, Master) ->
    spawn_link(fun() -> init(Title, Master) end).

init(Title, Master) ->
    Window = make_window(Title),
    loop(Window, Master).

make_window(Title) ->
    %% Server will be the parent of the Frame
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, Title, [{size, {?width, ?height}}]),
    wxFrame:setBackgroundColour(Frame, ?wxBLACK),
    Window = wxWindow:new(Frame, ?wxID_ANY),
    wxFrame:show(Frame),
    wxWindow:setBackgroundColour(Window, ?wxBLACK),
    wxWindow:show(Window),
    %% Monitor closing window
    wxFrame:connect(Frame, close_window),
    Window.

loop(Window, Master) ->
    receive
        %% Check if window is closed by the user
        #wx{event=#wxClose{}} ->
            wxWindow:destroy(Window),
            Master ! stop,
            ok;
        {colour, Colour} ->
            color(Window, Colour),
            loop(Window, Master);
        stop ->
            ok;
        Error ->
            io:format("gui: error message ~w~n", [Error]),
            loop(Window, Master)
    end.

color(Window, Colour) ->
    wxWindow:setBackgroundColour(Window, Colour),
    wxWindow:refresh(Window).
