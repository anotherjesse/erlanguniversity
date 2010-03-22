-module(cache_nb_worker).

-behaviour(gen_server).

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
    io:format("implement ~p ~p ~n", [_Request, _From]),
    {reply, ignore, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Client, Data0}, State) ->
    io:format("Hmm: ~p~n", [Data0]),
    [Line|_] = string:tokens(Data0, [$\n,$\r]),
    case string:tokens(Line, [$\ ]) of
        ["close"] ->
            gen_tcp:close(Client);
        ["delete", K] ->
            cache_server:delete(K),
            gen_tcp:send(Client, [<<"ok\n">>]);
        ["set", K, V] ->
            cache_server:store(K, V),
            gen_tcp:send(Client, [<<"ok\n">>]);
        ["get", K] ->
            case cache_server:fetch(K) of
                not_found ->
                    gen_tcp:send(Client, [<<"none\n">>]);
                V ->
                    gen_tcp:send(Client, [V, <<"\n">>])
            end;
        _ ->
            gen_tcp:send(Client, [<<"error, unsupported command\n">>])
    end,
    inet:setopts(Client, [{active, once}]),
    {noreply, State};

handle_info(_Info, State) ->
    io:format("Hmm: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("(~p) worker stopped~n", [self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
setup_socket(Pid) ->
    gen_server:call(Pid, setup_socket).
