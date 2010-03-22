-module(echo_client).

-export([say_it/3]).

say_it(Host, Port, Message) ->
    case gen_tcp:connect(Host, Port, [{active, false}, {packet, 0}]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, Message),
            {ok, Reply} = gen_tcp:recv(Socket, 0),
            io:format("~s", [Reply]),
            gen_tcp:close(Socket);
        Error ->
            io:format("Error: ~p~n", [Error])
    end.
