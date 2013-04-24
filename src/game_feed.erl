-module(game_feed).
-behaviour(gen_event).
 
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).
 
init(Player) ->
	{ok, Player}.

handle_event(Event, Player) ->
	player:game_feed(Player, Event),
	{ok, Player}.
	
handle_call(_, State) ->
	{ok, State}.
 
handle_info(_, State) ->
	{ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
 
terminate(_Reason, _State) ->
	ok.