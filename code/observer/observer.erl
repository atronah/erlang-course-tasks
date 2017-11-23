-module(observer).
-behaviour(gen_server).

-export([
    start/0,
    add_handler/2,
    fire/2
]).

-export([
    init/1,
    code_change/3,
    terminate/2
]).

-export([
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).


start() ->
    gen_server:start_link(?MODULE, [], []).
    
 
add_handler(Server, Module) ->
    gen_server:call(Server, {add_handler, Module}).


fire(Server, Event) ->
    gen_server:call(Server, {fire, Event}).
    

init([])
    {ok, []}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, State) ->
    ok.


handle_call({add_handler, Module}, _From, State) ->
    {reply, ok, [Module | State]};
    
    
handle_call({fire, Event}, _From, State) ->
    {reply, on_fire(Event, State), State}.
   

on_fire(Event, []) ->
    ok;
    
on_fire(Event, [Module | Tail]) ->
    Module:on_fire(Event),
    on_fire(Event, Tail).



handle_cast(_, State) ->
    {noreply, State}.


handle_info(Msg, State) ->
    {noreply, State}.
