-module(basic_nb_worker).

-behaviour(gen_server).

%% API
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {sock}).

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

handle_info({tcp, Sock, Data}, State) ->
    gen_tcp:send(Sock, [<<"You said: ">>, Data]),
    inet:setopts(Sock, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, _Sock}, State) ->
    {stop, shutdown, State};
handle_info({tcp_error, _Sock, _Reason}, State) ->
    {stop, shutdown, State};
handle_info(_Info, State) ->
    io:format("Hmm: ~p~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
setup_socket(Pid) ->
    gen_server:call(Pid, setup_socket).
