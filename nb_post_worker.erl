-module(nb_post_worker).

-behaviour(gen_server).

-define(FORM, <<"<html><body><form method=\"POST\" action=\"/login\">
                 Username: <input type=\"text\" size=\"10\" name=\"userid\" /><br />
                 Password: <input type=\"password\" size=\"10\" name=\"password\" /><br />
                 <input type=\"submit\" value=\"Login\" /></form></body></html>">>).

%% API
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {sock, method, path, headers=[]}).

start(Sock) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Sock], []),
    gen_tcp:controlling_process(Sock, Pid),
    setup_socket(Pid),
    {ok, Pid}.

init([Sock]) ->
    {ok, #state{sock=Sock}}.

handle_call(setup_socket, _From, #state{sock=Sock}=State) ->
    inet:setopts(Sock, [{active, once}]),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({http, Sock, {http_request, Method, {_, Path}, _}}, State) ->
    inet:setopts(Sock, [{active, once}]),
    {noreply, State#state{method=Method, path=Path}};

handle_info({http, Sock, {http_header, _, Name, _, Value}}, #state{headers=Headers}=State) ->
    io:format("~p = ~p~n", [Name, Value]),
    inet:setopts(Sock, [{active, once}]),
    {noreply, State#state{headers=[{Name, Value}|Headers]}};

handle_info({http, Sock, http_eoh}, #state{method='GET', path=Path}=State) ->
    case Path of
        <<"/favicon.ico">> ->
            send_404_response(Sock);
        _ ->
            send_html(Sock, ?FORM)
    end,
    {stop, normal, State};

handle_info({http, Sock, http_eoh}, #state{method='POST', headers=Headers}=State) ->
    inet:setopts(Sock, [{packet, raw}]),
    BodySize = binary_to_integer(proplists:get_value('Content-Length', Headers)),
    case gen_tcp:recv(Sock, BodySize, 60000) of
        {error, timeout} ->
            {stop, normal, State};
        {ok, Body} ->
            send_greeting(Sock),
            {stop, normal, State}
    end;

handle_info(_Info, State) ->
    io:format("Hmm: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("(~p) worker stopped~n", [self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
binary_to_integer(Bin) ->
    list_to_integer(binary_to_list(Bin)).

setup_socket(Pid) ->
    gen_server:call(Pid, setup_socket).

send_404_response(Sock) ->
    Response = [<<"HTTP/1.1 404 Not Found\r\n">>,
                <<"Connection: close\r\n\r\n">>],
    gen_tcp:send(Sock, Response).

send_html(Sock, HTML) ->
    Response = [<<"HTTP/1.1 200 OK\r\n">>,
                <<"Content-Type: text/html\r\n">>,
                <<"Connection: close\r\n">>,
                <<"Content-Length: ">>, integer_to_list(size(HTML)), <<"\r\n\r\n">>,
                HTML, <<"\r\n\r\n">>],
    gen_tcp:send(Sock, Response).

send_greeting(Sock) ->
    HTML = <<"You are now logged in!">>,
    Response = [<<"HTTP/1.1 200OK\r\n">>,
                <<"Content-Type: text/plain\r\n">>,
                <<"Connection: close\r\n">>,
                <<"Content-Length:">>, integer_to_list(size(HTML)), <<"\r\n\r\n">>,
                HTML, <<"\r\n\r\n">>],
    gen_tcp:send(Sock, Response).
