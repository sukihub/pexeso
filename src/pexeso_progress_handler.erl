-module(pexeso_progress_handler).
-behaviour(gen_event).
 
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).
 
-record(state, {
	module,
 	pid
}).

init({Module, Pid}) ->
	{ok, #state{module = Module, pid = Pid}}.

handle_event(Action, S) ->
	Module = S#state.module,
	Module:pexeso_progress(S#state.pid, Action),
	{ok, S}.
	
handle_call(_, State) ->
	{ok, State}.
 
handle_info(_, State) ->
	{ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
 
terminate(_Reason, _State) ->
	ok.