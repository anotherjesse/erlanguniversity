-module(echoes_tcp).

-export([start/1]).

start(Port) ->
    spawn(fun() -> start_server(Port) end).

start_server(Port) ->
    {ok, Sock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, false}]),
    Me = self(),
    Pid = spawn(fun () -> accept(Me, Sock) end),
    erlang:link(Pid),
    receive_clients().

receive_clients() ->
    receive
        {new_client, Client} ->
            inet:setopts(Client, [{active, once}]),
            receive_clients();
        {tcp, Socket, <<"close\r\n">>} ->
            gen_tcp:close(Socket),
            receive_clients();
        {tcp, Socket, Data} ->
            gen_tcp:send(Socket, [<<"You said: ">>, Data]),
            inet:setopts(Socket, [{active, once}]),
            receive_clients();
        {tcp_closed, Socket} ->
            gen_tcp:close(Socket),
            receive_clients();
        {tcp_error, Socket} ->
            gen_tcp:close(Socket),
            receive_clients()
    end.

accept(Receive, Sock) ->
    case gen_tcp:accept(Sock) of
        {ok, Client} ->
            gen_tcp:controlling_process(Client, Receive),
            Receive ! {new_client, Client},
            accept(Receive, Sock);
        _Error ->
            ok
    end.
