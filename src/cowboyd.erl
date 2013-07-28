-module(cowboyd).
-author('chvanikoff <chvanikoff@gmail.com>').

%% API
-export([
	start/0,
	stop/0,
	update_routes/0
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

start() ->
	ensure_started([?MODULE]),
	{ok, Path} = application:get_env(?MODULE, path),
	cowboyd_srv:set(path, Path),
	{ok, App} = application:get_env(?MODULE, app),
	App2 = list_to_atom(App),
	cowboyd_srv:set(app, App2),
	{ok, Rootdir} = application:get_env(?MODULE, rootdir),
	cowboyd_srv:set(rootdir, Rootdir),
	{ok, Port} = application:get_env(?MODULE, port),
	cowboyd_srv:set(port, Port),
	{ok, Nba} = application:get_env(?MODULE, nba),
	cowboyd_srv:set(nba, Nba),
	ok = sync:go(),
	ok.

update_routes() ->
	Path = cowboyd_srv:get(path),
	App = cowboyd_srv:get(app),
	{ok, Routes} = file:script(Path ++ "/routes.config", [
		{'App', App}
	]),
	Routes2 = cowboy_router:compile(Routes),
	cowboy:set_env(http_listener, dispatch, Routes2),
	ok.

stop() -> application:stop(?MODULE).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, Path} = application:get_env(?MODULE, path),
	{ok, App} = application:get_env(?MODULE, app),
	App2 = list_to_atom(App),
	{ok, Routes} = file:script(Path ++ "/routes.config", [
		{'App', App2}
	]),
	Routes2 = cowboy_router:compile(Routes),
	{ok, Root} = application:get_env(?MODULE, rootdir),
	nomatch = re:run(recompile(Path, Root), "rebar_abort"),
	true = code:add_path(Path ++ "/ebin"),
	[ ok = code:add_paths(filelib:wildcard(Path ++ "/deps/*/ebin"))
		|| filelib:is_dir(Path ++ "/deps") ],
	ok = App2:start(),
	{ok, Port} = application:get_env(?MODULE, port),
	Port2 = list_to_integer(Port),
	{ok, Nba} = application:get_env(?MODULE, nba),
	Nba2 = list_to_integer(Nba),
	{ok, _} = cowboy:start_http(http_listener, Nba2, [{port, Port2}], [
		{env, [{dispatch, Routes2}]}
	]),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) -> ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Restart_strategy = {one_for_one, 5, 10},
	Children = [?CHILD(cowboyd_srv, worker)],
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

recompile(Path, Root) ->
	os:cmd("cd " ++ Path ++ " && " ++ Root ++ "/rebar get-deps compile && cd -").
