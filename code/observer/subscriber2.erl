-module(subscriber2).
-export([on_fire/1]).

on_fire(Event) ->
    io:format("S2: ~p~n", [Event]). 
