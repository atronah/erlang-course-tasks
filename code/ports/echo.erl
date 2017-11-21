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

echo(Server, String) ->
    gen_server:call(Server, {echo, String}).


init([])
    Port = open_port(
        {spawn, "python echo.py"},
        [
            use_stdio,
            stream,
            binary
        ]
    ),
    {ok, {Port, []}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    {Pid, _Queue} = State,
    port_close(Pid),
    ok.


handle_call({Command, Data}, _From, State) ->
    {Pid, Queue} = State,
    {noreply, {Pid, [{Command, Data} | Queue]}}
   

handle_cast(_, State) ->
    {noreply, State}.


handle_info(Msg, State) ->
    {Pid, [Head | Tail]} = State,
    case Head of
        {echo, Str} -> 
            port_command(State, Str),
            Result = receive
                          {Pid, {data, Data}} -> Data
                      end,
            {reply, Result, {Pid, Tail}};
        {Command, _Data} ->
            io:format("Usupported command: ~p", [Command]),
            {noreply, {Pid, Tail}}
     end.
