-module(basic_webserver).

-export([start/1, stop/0, handle_request/1]).

start(Port) ->
  Options = [{port, Port},
             {ip, "127.0.0.1"},
             {loop, fun handle_request/1}],
  mochiweb_http:start(Options).

stop() ->
  mochiweb_http:stop().

handle_request(Req) ->
  {ok, F} = file:open("output.txt", [raw]),
  Req:respond({200, [{"Content-Type", "text/plain"},
                     {"Connection", "close"}], {file, F}}).
