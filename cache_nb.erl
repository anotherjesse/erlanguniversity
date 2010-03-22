-module(cache_nb).

-behaviour(gen_nb_server).

%% API
-export([start_link/1, start_link/2]).

%% gen_nb_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, start/0,
         terminate/2, sock_opts/0, new_connection/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link(Port) ->
    start_link("0.0.0.0", Port).

start_link(IpAddr, Port) ->
    gen_nb_server:start_link(?MODULE, IpAddr, Port, []).

init([]) ->
    {ok, #state{}}.

sock_opts() ->
    [list, {packet, line}, {active, false}, {reuseaddr, true}].

new_connection(Sock, State) ->
    cache_nb_worker:start(Sock),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start() ->
    cache_server:start_link(),
    cache_nb:start_link(4321).
