-module(h_notfound).
-author('chvanikoff <chvanikoff@gmail.com>').

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({tcp,http}, Req, _Opts) -> {ok, Req, undefined}.

handle(Req, State) ->
	{ok, Req2} = cowboy_req:reply(404,
		[{<<"content-type">>,<<"text/html">>}],
		<<"<h1>404 Page not found at webapp2</h1>">>,
		Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) -> ok.
