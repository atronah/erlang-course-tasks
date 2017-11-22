-module(echo).
-behaviour(gen_server).

-export([
    start_link/0,
    echo/2
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

start_link() ->
    gen_server:start_link(?MODULE, [], []).

echo(Server, Text) ->
    gen_server:call(Server, {echo, Text}).


init([]) ->
    PyPort = open_port(
        {spawn, "python3 echo.py"},
        [
            use_stdio,
            stream,
            binary
        ]
    ),
    {ok, {PyPort, []}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    {PyPort, _Waiters} = State,
    port_close(PyPort),
    ok.


handle_call({echo, Text}, From, State) when is_list(Text) ->
    handle_call({echo, list_to_binary(Text)}, From, State);

handle_call({echo, Text}, From, State) ->
    {PyPort, Waiters} = State,
    port_command(PyPort, erlang:iolist_to_binary([Text, "\n"])),
    {noreply, {PyPort, [From | Waiters]}}.
   

handle_cast(_, State) ->
    {noreply, State}.


handle_info({PyPort, {data, Data}}, {PyPort, [LastWaiter | OtherWaiters]}) ->
    gen_server:reply(LastWaiter, erlang:binary_to_list(Data)),
    {noreply, {PyPort, OtherWaiters}};
handle_info(Msg, State) ->
    io:format("Message: ~p~n", [Msg]),
    {noreply, State}.
