-module(pexeso_game).
-behaviour(gen_server).

% Sample usage:
%
% % Create new pexeso game
% {ok, Game} = pexeso_game:start(
%	["John", "Lucy", "Suzi"], % list of player names
%	[house, car, boat, sun]   % list of card types (== images on cards)
% ).
%
% % Get list of cards available
% pexeso_game:get_playground(Game). % > [4,3,5,7,1,8,2,6]
%
% % Turn some card
% pexeso_game:turn_card(Game, 5). % > {turn, {5, sun}}
%
% % Turn non existing card
% pexeso_game:turn_card(Game, 9). % > not_found
%
% % Turn another correct card
% pexeso_game:turn_card(Game, 3). % > {fail, {3, boat, 5, sun}}
%
% % Lucy's turn
% pexeso_game:turn_card(Game, 2). % > {turn, {2, boat}}
%
% % Card 3 was boat too!
% pexeso_game:turn_card(Game, 3). % > {pick, {3, 2, boat}}
%
% % Get game stats
% pexeso_game:get_stats(Game). % > [{"Suzi",{turns,0},{picks,0}},
%                            	    {"Lucy",{turns,2},{picks,1}},
%	                                {"John",{turns,2},{picks,0}}]
%
% % When we'done, stop the game
% pexeso_game:stop(Game).

-export([
	start/1, 
	register_player/2,
	get_playground/1, 
	turn_card/3, 
	get_stats/1,
	stop/1
]).

-export([ 
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
	playground,
	players
}).

-record(player, {
	address,
	name,
	last_card = null,
	turns = 0,
	picks = 0
}).

%%
% public interface
%

start(CardTypes) when is_list(CardTypes) -> 
	gen_server:start(?MODULE, CardTypes, []).

register_player(Pid, Name) ->
	gen_server:call(Pid, {register_player, Name}).

get_playground(Pid) -> 
	gen_server:call(Pid, get_playground).

get_stats(Pid) ->
	gen_server:call(Pid, get_stats).

turn_card(Pid, Name, Card) ->
	gen_server:cast(Pid, {turn_card, Name, Card}).

stop(Pid) ->
	gen_server:cast(Pid, stop).

%%
% gen_server stuff
%

%%
% Server initialization.
%
init(CardTypes) -> 

	Players = dict:new(),

	% create new game and server state
	State = #state{
		playground = generate_playground(CardTypes),
		players = Players
	},

	% standard return
	{ok, State}.

%%
% Addss new player into the game.
%
handle_call({register_player, Name}, From, S) ->

	Player = #player{
		address = From,
		name = Name
	},

	State = S#state{ players = dict:store(Name, Player, S#state.players) },

	% TODO subscribe player to events
	% TODO broadcast new player

	{reply, ok, State};

%%
% Returns list of unturned/available cards.
%
handle_call(get_playground, _From, S) ->
	Cards = dict:fetch_keys(S#state.playground),
	{reply, Cards, S};

%%
% Returns statistics of current game.
%
handle_call(get_stats, _From, S) ->
	{reply, create_stats(S), S}.

%%
% Turns one card and reports what's on it.
%
handle_cast({turn_card, Name, CardId}, S) ->
	try

		% get the turned card
		Card = get_card(S#state.playground, CardId),

		% get player
		Player = get_player(S#state.players, Name),

		% figure out what action should be triggered
		Action = turned_card(Card, Player#player.last_card),

		% update state based on the action
		State = #state{
			playground = update_playground(Action, S#state.playground),
			players = update_players(S#state.players, Name, update_player(Action, Player))% dict:store(From, , S#state.players)
		},

		% reply with action and set new state
		% TODO broadcast Action
		{noreply, State}

	catch

		% if requested card does not exist
		% TODO broadcast not_found or do nothing
		throw:card_not_found -> {noreply, S};
		throw:player_not_found -> {noreply, S}

	end;

%%
% Manual stop of server.
%
handle_cast(stop, S) -> 
	{stop, normal, S}.

%%
% Other messages.
%
handle_info(Info, State) ->
	io:format("Unexpected message: ~p~n", [Info]),
	{noreply, State}.

%%
% Terminate.
%
terminate(_Reason, _State) -> ok.

%%
% Code reload.
%
code_change(_Old, State, _Extra) -> {ok, State}.

%% private

%%
% Returns {CardId, Value} pair for given CardId.
%
get_card(Playground, CardId) ->
	case dict:find(CardId, Playground) of
		{ok, Value} -> {CardId, Value};
		error -> throw(card_not_found)
	end.

%%
% Retruns player record or throws error if such player does not play the game.
%
get_player(Players, Key) ->
	case dict:find(Key, Players) of
		{ok, Player} -> Player;
		error -> throw(player_not_found)
	end.

%%
% First card was turned by current player.
%
turned_card(C, null) ->
	{turn, C};

%%
% User tries to turn already turned card.
% Do nothing.
%
turned_card(C, C) -> 
	{none, {}};

%%
% Second card was turned, and both have same values V.
% Remove them from playground.
% Continues current player.
%
turned_card({C1, V}, {C2, V}) when C1 /= C2 ->
	{pick, {C1, C2, V}};

%%
% Second card was turned, cards have different values V1 and V2.
% Continues next player.
%
turned_card({C1, V1}, {C2, V2}) ->
	{fail, {C1, V1, C2, V2}}.

%%
% If player picked cards, remove them from the playground.
%
update_playground({pick, {C1, C2, _}}, Playground) ->
	io:format("Removing cards ~p and ~p from playground~n", [C1, C2]),
	dict:erase(C1, dict:erase(C2, Playground));

%%
% Otherwise do nothing.
%
update_playground(_, Playground) -> Playground.

%%
% Sets updated player in players dictionary.
%
update_players(Players, Key, Player) ->
	dict:store(Key, Player, Players).

%%
% Add 1 turn to the current user.
%
update_player({turn, Card}, Player) -> 
	Player#player{
		turns = Player#player.turns + 1,
		last_card = Card
	};

%%
% Add 1 turn and 1 pick to the current user.
%
update_player({pick, _}, Player) -> 
	Player#player{
		turns = Player#player.turns + 1,
		picks = Player#player.picks + 1,
		last_card = null
	};

%%
% Add 1 turn to the current user.
%
update_player({fail, _}, Player) -> 
	Player#player{
		turns = Player#player.turns + 1,
		last_card = null
	};

%%
% Otherwise do nothing.
%
update_player(_, Player) -> 
	Player.

%%
% Returns a playground with randomly shuffled pairs of given card types.
%
generate_playground(CardTypes) ->
	generate_playground(
		CardTypes, 
		shuffled_list(length(CardTypes) * 2), 
		dict:new()
	).

generate_playground([], _, Dict) -> Dict;
generate_playground(_, [], Dict) -> Dict;
generate_playground([CardType | CardTypes], [I1, I2 | Indexes], Dict) ->
	generate_playground(
		CardTypes, Indexes,
		dict:store(I1, CardType, dict:store(I2, CardType, Dict))
	).

%%
% Returns shuffled list of numbers 1..N.
% This is probably not the best way to do it.
%
shuffled_list(N) ->
	random:seed(now()),
	WithOrder = [ { random:uniform(), I } || I <- lists:seq(1, N) ],
	[ I || {_, I} <- lists:sort(WithOrder) ].

%%
% Creates list of stats from the current player records.
%
create_stats(S) ->
	create_stat(dict:to_list(S#state.players), []).

create_stat([], Stats) -> Stats;
create_stat([ {_, P} | Players ], Stats) ->
	create_stat(
		Players, 
		[ {P#player.name, {turns, P#player.turns}, {picks, P#player.picks}} | Stats ]
	).