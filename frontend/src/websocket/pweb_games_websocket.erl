-module(pweb_games_websocket).
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
	{ok, stateless}.

handle_join(_Url, _WebSocket, _SessionId, State) ->
	io:format("joined~n"),
	{reply, ok, State}.

handle_close(_Url, _WebSocket, _SessionId, State) ->
	{reply, ok, State}.

handle_incoming(Url, WebSocket, SessionId, Message, State) ->
	process_command(Url, WebSocket, SessionId, parse_command(Message), State),
	{noreply, State}.

handle_broadcast(_Message, State) ->
	{noreply, State}.

handle_info(_Message, State) ->
	{noreply, State}.

terminate(_Reason, _State) -> ok.


parse_command(Message) ->

	{struct, [ {<<"command">>, Command}, {<<"params">>, Params} ]} = mochijson2:decode(Message),
	CommandAtom = binary_to_existing_atom(Command, utf8),

	{CommandAtom, Params}.

%%
% User requested list of games.
%
process_command(_Url, WebSocket, _SessionId, {get_games, _}, _State) ->
	WebSocket ! {text, games_to_json(init_server:get_games())};

%%
% User wants to create a new game.
%
process_command(_Url, WebSocket, _SessionId, {create_game, Params}, _State) ->
	{struct, [ {<<"name">>, Name} ]} = Params,
	init_server:create_game(Name, default_cards());


process_command(_Url, _WebSocket, _SessionId, {Command, Params}, _State) ->
	io:format("Games websocket: unexpected command ~p with params ~p~n", [Command, Params]).


games_to_json(Games) ->
	mochijson2:encode([ game_to_proplist(Game) || Game <- Games ]).

game_to_proplist({Name, MainPid, BackupPid}) ->
	{ struct, [ 
		{name, Name}
	]}.


default_cards() ->
	[ red, blue, green, yellow, orange, black ].