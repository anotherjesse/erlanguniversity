-module(math_server1).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add/2,
         clear/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(X, Y) ->
    gen_server:call(?SERVER, {add, X, Y}).

clear() ->
    gen_server:cast(?SERVER, clear).

init([]) ->
    {ok, dict:new()}.

handle_call({add, X, Y}=Message, _From, State) ->
    case dict:find({add, X, Y}, State) of
        error ->
            Answer = X + Y,
            {reply, Answer, dict:store(Message, Answer, State)};
        {ok, Answer} ->
            {reply, Answer, State}
    end;

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
