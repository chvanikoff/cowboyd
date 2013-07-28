-module(cowboyd_srv).
-author('chvanikoff <chvanikoff@gmail.com>').

%% API
-export([
	start_link/0,
	set/2,
	get/1,
	get/2
]).

%% Gen_server behaviour
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {config = []}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set(Key, Value) -> gen_server:cast(?MODULE, {set, Key, Value}).

get(Key) -> ?MODULE:get(Key, undefined).

get(Key, Default) -> gen_server:call(?MODULE, {get, Key, Default}).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

init(_Args) -> {ok, #state{}}.


handle_call({get, Key, Default}, _From, State) ->
	Reply = case lists:keyfind(Key, 1, State#state.config) of
		{Key, Value} -> Value;
		_ -> Default
	end,
	{reply, Reply, State};

handle_call(_Request, _From, State) -> {reply, ignored, State}.


handle_cast({set, Key, Value}, #state{config = Config} = State) ->
	{noreply, State#state{config = lists:keystore(Key, 1, Config, {Key, Value})}};

handle_cast(_Msg, State) -> {noreply, State}.


handle_info(_Info, State) -> {noreply, State}.


code_change(_OldVsn, State, _Extra) -> {ok, State}.


terminate(_Reason, _State) -> ok.
