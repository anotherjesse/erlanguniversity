-module(math_server).

-export([start/1]).

start(Port) ->
    spawn(fun() -> start_server(Port) end).

start_server(Port) ->
    {ok, Sock} = gen_tcp:listen(Port, [{packet, line}, {active, false}]),
    Me = self(),
    Pid = spawn(fun () -> accept(Me, Sock) end),
    erlang:link(Pid),
    receive_clients().

receive_clients() ->
    receive
        stop ->
            ok;
        {new_client, Client} ->
            process(Client),
            receive_clients()
    end.

process(Client) ->
    %% Read all available bytes
    case gen_tcp:recv(Client, 0) of
        {ok, <<"close\r\n">>} ->
            gen_tcp:close(Client);
        {ok, Data0} ->
            Data = lists:filter(fun(C) -> C /= $\r andalso C /= $\n end,
                                Data0),
            io:format("Data: ~p~n", [Data]),
            Answer = lists:sum([list_to_integer(N) ||
                                   N <- string:tokens(Data, [$,])]),
            gen_tcp:send(Client, [integer_to_list(Answer), "\n"]),
            process(Client);
        _Error ->
            gen_tcp:close(Client)
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
