-module(pexeso).
-behaviour(application).
-export([
	start/2,
	stop/1
]).

start(normal, _Args) ->
	not_implemented.

stop(_State) ->
	ok.