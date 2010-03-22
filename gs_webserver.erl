-module(gs_webserver).

-behaviour(gen_server).

%% API
-export([start_link/2, die/0, handle_request/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

handle_request(Req) ->
    Req:respond({200, [{"Content-Type", "text/plain"},
                       {"Connection", "close"}], "Running"}).

die() ->
    exit(whereis(?SERVER), kill).

start_link(IpAddr, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [IpAddr, Port], []).

init([IpAddr, Port]) ->
    Options = [{port, Port}, {ip, IpAddr},
               {loop, fun handle_request/1}],
    {ok, Pid} = mochiweb_http:start(Options),
    erlang:link(Pid),
    io:format("Listening on ~s:~p~n", [IpAddr, Port]),
    {ok, []}.

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
