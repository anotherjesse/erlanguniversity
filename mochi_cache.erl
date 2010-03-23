-module(mochi_cache).

-export([start/0, start/1, stop/0, handle_request/1]).

start() ->
    start(1234).
    
start(Port) ->
  cache_server:start_link(),
  Options = [{port, Port},
             {ip, "0.0.0.0"},
             {loop, fun handle_request/1}],
  mochiweb_http:start(Options).

stop() ->
  mochiweb_http:stop().

handle_request(Req) ->
    Path = Req:get(path),
    Method = Req:get(method),
    Resp = dispatch(Method, Path, Req),
    Req:respond(Resp).

dispatch('DELETE', Path, _Req) ->
    cache_server:delete(Path),
    send_text(<<"ok">>);

dispatch('PUT', Path, Req) ->
    cache_server:store(Path, Req:recv_body()),
    send_text(<<"ok">>);

dispatch('GET', Path, Req) ->
    case cache_server:fetch(Path) of
        not_found ->
            send_404();
        V ->
            send_text(V)
    end;
    
dispatch(_, _, _Req) ->
    {404, [{"Content-Type", "text/plain"}], "not implemented"}.
    

send_text(Data) ->
    {200, [{"Content-Type", "text/plain"}], Data}.

send_404() ->
    {404, [{"Content-Type","text/plain"}], "not found"}.