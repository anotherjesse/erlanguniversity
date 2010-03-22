-module(port_scanner).
-export([scan/3]).

scan(Addr, Start, End) ->
    Services = parse_services(),
    scan(Addr, Start, End, Services).

scan(Host, Start, End, Services) ->
    [Data || Data <-
                 [ping(Host, Port, Services) || Port <- lists:seq(Start, End)],
             Data /= none].

%% Internal functions
parse_services() ->
    {ok, F} = file:read_file("/etc/services"),
    F1 = string:tokens(binary_to_list(F), "\n"),
    Lines = [string:tokens(Line, " ") || Line <- F1,
                                         hd(Line) /= $#],
    [{Port, Name} || [Name, Port|_] <- Lines,
                     length(Lines) > 2].

ping(Host, Port, Services) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0},
                                      {active, false}], 500) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            {proplists:get_value(integer_to_list(Port) ++ "/tcp",
                                Services,
                                unkown), Port};
        _Error ->
            none
    end.
