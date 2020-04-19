-module(pweb_pexeso_websocket).
-behaviour(boss_service_handler).

-export([
	init/0,
	handle_join/4,
	handle_close/4,
	handle_incoming/5,
	handle_broadcast/2,
	handle_info/2,
	terminate/2
]).

init() ->
	{ok, dict:new()}.

handle_join(_Url, _WebSocket, _SessionId, State) ->
	{reply, ok, State}.


handle_close(_Url, WebSocket, _SessionId, State) ->

	case dict:find(WebSocket, State) of
		error -> ok;
		{ok, Player} ->
			websocket_player:stop(Player),
			io:format("Player should have been stopped~n", [])
	end,

	{reply, ok, dict:erase(WebSocket, State)}.


handle_incoming(_Url, WebSocket, _SessionId, Message, State) ->
	{Command, Params} = parse_incoming_message(Message),
	NewState = handle_command(WebSocket, Command, Params, State),
	{noreply, NewState}.

handle_broadcast(_Message, State) ->
	{noreply, State}.

handle_info(_Message, State) ->
	{noreply, State}.

terminate(_Reason, _State) -> ok.

parse_incoming_message(Message) ->

	{struct, [ {<<"command">>, Command}, {<<"params">>, Params} ]} = mochijson2:decode(Message),
	CommandAtom = binary_to_existing_atom(Command, utf8),

	{CommandAtom, Params}.


handle_command(WebSocket, join, Params, Dict) ->
	
	{struct, [ {<<"game">>, Game}, {<<"player">>, Player} ]} = Params,
	
	dict:store(
		WebSocket,
		websocket_player:start(init_server:get_game(Game), Player, WebSocket),
		Dict
	);


handle_command(WebSocket, get_stats, _, Dict) ->
	
	{ok, Player} = dict:fetch(WebSocket, Dict),
	Stats = stats_to_json(websocket_player:get_stats(Player)),	

	WebSocket ! {text, Stats},

	Dict;


handle_command(WebSocket, get_playground, _, Dict) ->
	
	{ok, Player} = dict:fetch(WebSocket, Dict),
	Stats = playground_to_json(websocket_player:get_playground(Player)),	

	WebSocket ! {text, Stats},

	Dict;


handle_command(WebSocket, turn_card, Params, Dict) ->

	{struct, [ {<<"card">>, Card} ]} = Params,
	
	{ok, Player} = dict:fetch(WebSocket, Dict),
	websocket_player:turn_card(Player, Card),

	Dict;


handle_command(_, Message, _, Dict) ->
	io:format("Pexeso websocket: unexpected message ~p~n", [Message]),
	Dict.


stats_to_json(Stats) ->
	create_json_command(action_stats, [
		{stats, {struct, [ stat_to_struct(Stat) || Stat <- Stats ]}}
	]).

stat_to_struct({Name, Turns, Picks}) ->
	{Name, {struct, [ Turns, Picks ]}}.


playground_to_json(Playground) ->
	create_json_command(action_playground, [
		{playground, Playground}
	]).


create_json_command(Name, Params) ->
	mochijson2:encode(
		{struct, [
			{command, Name},
			{params, {struct, Params}}
		]}		
	).