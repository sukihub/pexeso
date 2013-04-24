-module(pexeso_game).
-behaviour(gen_server).

-include_lib("pexeso.hrl").

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
	start_link/1, 
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
	game,
	event_manager
}).

%%
% public interface
%

start_link(CardTypes) when is_list(CardTypes) -> 
	gen_server:start_link(?MODULE, CardTypes, []).

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

	% start event manager process
	% TODO zistit co robi link
	{ok, EventManagerPid} = gen_event:start_link(),

	% create new game and server state
	State = #state{
		game = pexeso_lib:create(CardTypes),
		event_manager = EventManagerPid
	},

	% standard return
	{ok, State}.

%%
% Addss new player into the game.
%
handle_call({register_player, Name}, {PlayerPid, _}, S) ->

	State = S#state{ 
		game = pexeso_lib:add_player(S#state.game, Name)
	},

	gen_event:notify(
		S#state.event_manager, 
		#action{move = #move{type = join}, player = Name}
	),
	
	gen_event:add_handler(
		S#state.event_manager, 
		pexeso_progress_handler, 
		{player, PlayerPid}
	),
	
	{reply, ok, State};

%%
% Returns list of unturned/available cards.
%
handle_call(get_playground, _From, S) ->
	Cards = pexeso_lib:get_playground(S#state.game),
	{reply, Cards, S};

%%
% Returns statistics of current game.
%
handle_call(get_stats, _From, S) ->
	Stats = pexeso_lib:get_stats(S#state.game),
	{reply, Stats, S}.

%%
% Turns one card and reports what's on it.
%
handle_cast({turn_card, Name, CardId}, S) ->
	try

		Action = pexeso_lib:get_action(S#state.game, Name, CardId),

		% broadcast action
		gen_event:notify(S#state.event_manager, Action),

		% update state based on the action
		State = S#state{
			game = pexeso_lib:apply_action(S#state.game, Action)
		},

		{noreply, State}

	catch

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
