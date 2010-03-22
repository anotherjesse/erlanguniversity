-module(cache_tcp_server).

-export([start/1]).

start(Port) ->
    spawn(fun() -> start_server(Port) end).

start_server(Port) ->
    cache_server:start_link(),
    {ok, Sock} = gen_tcp:listen(Port, [{packet, line}, {active, false}, {reuseaddr, true}]),
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
        {ok, Data0} ->
            [Line|_] = string:tokens(Data0, [$\n,$\r]),
            case string:tokens(Line, [$\ ]) of
                ["close"] ->
                    gen_tcp:close(Client);
                ["delete", K] ->
                    cache_server:delete(K),
                    gen_tcp:send(Client, [<<"ok\n">>]),
                    process(Client);
                ["store", K, V] ->
                    cache_server:store(K, V),
                    gen_tcp:send(Client, [<<"ok\n">>]),
                    process(Client);
                ["fetch", K] ->
                    case cache_server:fetch(K) of
                        not_found ->
                            gen_tcp:send(Client, [<<"none\n">>]);
                        V ->
                            gen_tcp:send(Client, [V, <<"\n">>])
                    end,
                    process(Client);
                _ ->
                    gen_tcp:send(Client, [<<"error, unsupported command\n">>]),
                    process(Client)
            end;
        _Error ->
            gen_tcp:close(Client)
    end.

accept(Worker, Sock) ->
    case gen_tcp:accept(Sock) of
        {ok, Client} ->
            gen_tcp:controlling_process(Client, Worker),
            Worker ! {new_client, Client},
            accept(Worker, Sock);
        _Error ->
            ok
    end.
