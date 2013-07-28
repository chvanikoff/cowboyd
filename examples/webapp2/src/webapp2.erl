-module(webapp2).
-author('chvanikoff <chvanikoff@gmail.com>').

%% API
-export([
	start/0,
	stop/0
]).

%% Application behaviour
-behaviour(application).
-export([start/2, stop/1]).

%% Supervisor behaviour
-behaviour(supervisor).
-export([init/1]).
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start() -> ensure_started([?MODULE]).

stop() -> application:stop(?MODULE).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) -> ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Restart_strategy = {one_for_one, 5, 10},
	Children = [],
	{ok, {Restart_strategy, Children}}.

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps] = All) ->
	case application:start(App) of
		ok -> ensure_started(Apps);
		{error, {already_started, App}} -> ensure_started(Apps);
		{error, {not_started, Dependency}} -> ensure_started([Dependency | All])
	end.
