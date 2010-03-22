-module(cache_server).

-behaviour(gen_server).

%% RUN TESTS VIA:

% erlc cache_server.erl  && erl -s cache_server

%% API
-export([start_link/0,
         start/0,
         store/2,
         fetch/1,
         delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

store(K, V) ->
    gen_server:call(?SERVER, {store, K, V}).

delete(K) ->
    gen_server:call(?SERVER, {delete, K}).

fetch(K) ->
    gen_server:call(?SERVER, {fetch, K}).

init([]) ->
    {ok, dict:new()}.

handle_call({store, K, V}, _From, State) ->
    {reply, ok, dict:store(K, V, State)};

handle_call({fetch, K}, _From, State) ->
    case dict:find(K, State) of
        error ->
            {reply, not_found, State};
        {ok, V} ->
            {reply, V, State}
    end;

handle_call({delete, K}, _From, State) ->
    {reply, ok, dict:erase(K, State)};

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(clear, _State) ->
    {noreply, dict:new()};

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
    cache_server:store(one, 1),
    cache_server:store(two, 2),
    1 = cache_server:fetch(one),
    2 = cache_server:fetch(two),
    cache_server:delete(one),
    not_found = cache_server:fetch(one),
    2 = cache_server:fetch(two),
    io:format("tests pass~n"),
    init:stop().