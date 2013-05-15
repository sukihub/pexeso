-module(pexeso).
-behaviour(application).
-export([
	start/2,
	stop/1
]).

start(normal, _Args) ->
	io:format("Starting entire pexeso application~n", []),
	pexeso_supervisor:start_link().

stop(_State) ->
	ok.