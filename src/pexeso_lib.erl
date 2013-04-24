-module(pexeso_lib).
-include_lib("pexeso.hrl").

-export([
	create/1,
	add_player/2,
	get_playground/1,
	get_stats/1,
	get_action/3,
	apply_action/2
]).

-record(state, {
	playground,
	players
}).

-record(player, {
	name,
	last_card = null,
	turns = 0,
	picks = 0
}).

%%
% Creates new pexeso game.
%
create(CardTypes) ->
	#state{
		playground = generate_playground(CardTypes),
		players = dict:new()
	}.

%%
% Adds new player.
%
add_player(State, Name) ->
	State#state{
		players = dict:store(
			Name, 
			#player{name = Name}, 
			State#state.players
		) 
	}.

%%
% Returns playground.
%
get_playground(State) ->
	dict:fetch_keys(State#state.playground).

%%
% Creates list of stats from the current player records.
%
get_stats(State) ->
	create_stats(dict:to_list(State#state.players), []).

%%
% Figure out the action based on the card.
%
get_action(State, Name, CardId) ->
	
	% get the turned card
	Card = get_card(State#state.playground, CardId),

	% get player
	Player = get_player(State#state.players, Name),

	% figure out what action should be triggered
	Move = turned_card(Card, Player#player.last_card),

	#action{player = Player#player.name, move = Move}.

%%
% Apply action to the game state.
%
apply_action(State, Action) ->

	Name = Action#action.player,

	% get player
	Player = update_player(
		Action#action.move,
		get_player(State#state.players, Name)
	),

	% update state based on the action
	State#state{
		playground = update_playground(Action#action.move, State#state.playground),
		players = update_players(State#state.players, Name, Player)
	}.

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

create_stats([], Stats) -> Stats;
create_stats([ {_, P} | Players ], Stats) ->
	create_stats(
		Players, 
		[ {P#player.name, {turns, P#player.turns}, {picks, P#player.picks}} | Stats ]
	).

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
	#move{type = turn, card_a = C};

%%
% User tries to turn already turned card.
% Do nothing.
%
turned_card(C, C) -> 
	#move{type = close, card_a = C};

%%
% Second card was turned, and both have same values V.
% Remove them from playground.
% Continues current player.
%
turned_card({C1, V} = CardA, {C2, V} = CardB) when C1 /= C2 -> 
	#move{type = pick, card_a = CardA, card_b = CardB};

%%
% Second card was turned, cards have different values V1 and V2.
% Continues next player.
%
turned_card(CardA, CardB) ->
	#move{type = fail, card_a = CardA, card_b = CardB}.

%%
% If player picked cards, remove them from the playground.
%
update_playground(Move = #move{type = pick}, Playground) ->

	{C1, _} = Move#move.card_a,
	{C2, _} = Move#move.card_b,

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
update_player(#move{type = turn, card_a = Card}, Player) -> 
	Player#player{
		turns = Player#player.turns + 1,
		last_card = Card
	};

%%
% Just close opened card.
%
update_player(#move{type = close}, Player) -> 
	Player#player{
		last_card = null
	};

%%
% Add 1 turn and 1 pick to the current user.
%
update_player(#move{type = pick}, Player) -> 
	Player#player{
		turns = Player#player.turns + 1,
		picks = Player#player.picks + 1,
		last_card = null
	};

%%
% Add 1 turn to the current user.
%
update_player(#move{type = fail}, Player) -> 
	Player#player{
		turns = Player#player.turns + 1,
		last_card = null
	};

%%
% Otherwise do nothing.
%
update_player(_, Player) -> 
	Player.