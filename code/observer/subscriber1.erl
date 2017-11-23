-module(subscriber1).
-export([on_fire/1]).

on_fire(Event) ->
    io:format("S1: ~p~n", [Event]). 