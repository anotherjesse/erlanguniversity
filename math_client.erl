-module(math_client).

-export([add/4, start/0]).

-define(HOST, "localhost").
-define(PORT, 9000).

add(Host, Port, X, Y) ->
    case gen_tcp:connect(Host, Port, [{active, false}, {packet, line}]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, io_lib:format("~p,~p~n", [X,Y])),
            {ok, Reply} = gen_tcp:recv(Socket, 0),
            gen_tcp:close(Socket),
            [Result|_] = string:tokens(Reply, [$\n]),
            list_to_integer(Result);
        Error ->
            {error, io_lib:format("~p", [Error])}
    end.

start() ->
    5 = add(?HOST, ?PORT, 2, 3),
    1 = add(?HOST, ?PORT, 3, -2),
    init:stop().
