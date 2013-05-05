-module(pexeso_game).
-include_lib("pexeso.hrl").

-behaviour(gen_fsm).
-behaviour(pexeso_progress_listener).

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

	set_main/2,
	set_backup/3,

	register_player/3,
	get_playground/1, 
	get_stats/1,
	turn_card/3,
	heartbeat/1, 

	pause/1,

	pexeso_progress/2,

	stop/1
]).

-export([ 
	init/1,
	terminate/3,
	code_change/4,
	handle_info/3,

	idle/2, idle/3,
	main/2, main/3,
	backup/2, backup/3,
	handle_event/3, handle_sync_event/4
]).

-record(state, {
	game,
	players,
	event_manager,
	other_pid = null
}).

%%
% PUBLIC INTERFACE
%

start(CardTypes) when is_list(CardTypes) -> 
	gen_fsm:start(?MODULE, CardTypes, []).

%%
% For state init.
%
set_main(Pid, BackupPid) ->
	gen_fsm:sync_send_event(Pid, {set_main, BackupPid}).

set_backup(Pid, Game, Players) ->
	gen_fsm:sync_send_event(Pid, {set_backup, Game, Players}).

%%
% For state main.
%
register_player(Pid, Name, PlayerPid) ->
	gen_fsm:send_event(Pid, {register_player, Name, PlayerPid}).

turn_card(Pid, Name, Card) ->
	gen_fsm:send_event(Pid, {turn_card, Name, Card}).

heartbeat(Pid) ->
	gen_fsm:sync_send_event(Pid, heartbeat).

%%
% For state backup.
%
pexeso_progress(Pid, Action) ->
	gen_fsm:send_event(Pid, {pexeso_progress, Action}).

%%
% For any state.
%
get_playground(Pid) -> 
	gen_fsm:sync_send_all_state_event(Pid, get_playground).

get_stats(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, get_stats).

pause(Pid) ->
	gen_fsm:send_all_state_event(Pid, pause).

stop(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).

%%
% IMPLEMENTATION
%

%%
% Pexeso game initialization.
%
init(CardTypes) -> 

	% start event manager process
	{ok, EventManagerPid} = gen_event:start_link(),

	% create new pexeso game
	% and add event manager
	State = #state{ 
		game = pexeso_lib:create(CardTypes),
		players = dict:new(),
		event_manager = EventManagerPid 
	},

	% go to state idle
	{ok, idle, State}.

%%
% Idle state.
%

%%
% Tells this game to be main game. Can be sent by anybody.
%
idle({set_main, BackupPid}, _From, S) ->
	State = advance_to_main(BackupPid, S),
	{reply, ok, main, State};

%%
% Tells this game to be backup. Can only by sent by respective main game.
%
idle({set_backup, Game, Players}, {From, _}, S) ->

	% put game and players into state
	State = S#state{ 
		game = Game,
		players = Players,
		other_pid = From
	},

	% register all players in gen_event
	[ start_notifying(player, PlayerPid, S#state.event_manager) || {_, PlayerPid} <- dict:to_list(Players) ],

	{reply, ok, backup, State};


idle(Message, _From, S) ->
	unexpected(Message, idle),
	{reply, unexpected, idle, S}.

idle(Message, S) ->
	unexpected(Message, idle),
	{next_state, idle, S}.


%%
% Main state.
%

%%
% Received heartbeat from backup game.
% Only allowed to be called by backup game process itself.
%
main(heartbeat, {BackupPid, _}, S) when S#state.other_pid == BackupPid ->
	io:format("Backup is ok!~n"),
	{reply, ok, main, S};

main(Message, _From, S) ->
	unexpected(Message, main),
	{reply, unexpected, main, S}.

%%
% Adds new player into the game.
%
main({register_player, Name, PlayerPid}, S) ->
	
	% add player
	State = add_player(S, Name, PlayerPid),	

	% and notify all existing pexeso listeners
	gen_event:notify(
		S#state.event_manager, 
		#action{move = #join{pid = PlayerPid}, player = Name}
	),
	
	{next_state, main, State};

%%
% Turns one card and reports what's on it.
%
main({turn_card, Name, CardId}, S) ->
	try

		Action = pexeso_lib:get_action(S#state.game, Name, CardId),

		% broadcast action
		gen_event:notify(S#state.event_manager, Action),

		% update state based on the action
		State = S#state{
			game = pexeso_lib:apply_action(S#state.game, Action)
		},

		create_main_reply(State)

	catch

		throw:card_not_found -> 
			gen_event:notify(S#state.event_manager, #action{player = Name, move = #invalid{}}),
			{next_state, main, S};

		throw:player_not_found -> 
			{next_state, main, S}

	end;

main(Message, S) ->
	unexpected(Message, main),
	{next_state, main, S}.

%%
% Creates reply for turn_card.
%
create_main_reply(State) ->
	case pexeso_lib:is_finished(State#state.game) of
		false -> {next_state, main, State};
		true  -> {stop, normal, State}
	end.


%%
% Events in backup state.
%

%%
% New player joined the game.
%
backup({pexeso_progress, #action{ move = #join{} = Move, player = Name }}, S) ->
	State = add_player(S, Name, Move#join.pid),
	{next_state, backup, State};

%%
% Do nothing on invalid action.
%
backup({pexeso_progress, #action{ move = #invalid{} }}, S) ->
	{next_state, backup, S};

%%
% Other actions.
%
backup({pexeso_progress, Action = #action{}}, S) ->

	State = S#state{ 
		game = pexeso_lib:apply_action(S#state.game, Action) 
	},

	create_backup_reply(State);

%%
% Backup receives gameplay event, which suggests that main is down.
%
backup(Message = {Event, _, _}, S) when Event == register_player; Event == turn_card ->
	
	% contact init server to see what the hell is going on
	% Response = nothing;
	Response = {advance, S#state.other_pid},
	
	case Response of

		% ignore gameplay event, main works as expected
		nothing -> 
			{next_state, backup, S};

		% you are main a here is your new backup
		{advance, NewBackupPid} ->

			% advance self to main, and push game state to new backup
			State = advance_to_main(NewBackupPid, S),

			% notify players about the change
			[ player:new_servers(PlayerPid, self(), NewBackupPid) || {_, PlayerPid} <- dict:to_list(State#state.players) ],

			% resend original message to self (will already be in main state)
			gen_fsm:send_event(self(), Message),
			
			{next_state, main, State}

	end;

%%
% Heartbeat timeout.
%
backup(timeout, S) ->
	pexeso_game:heartbeat(S#state.other_pid),
	{next_state, backup, S};

backup(Message, S) ->
	unexpected(Message, backup),
	{next_state, backup, S}.

backup(Message, _From, S) ->
	unexpected(Message, backup),
	{reply, unexpected, backup, S}.

%%
% Creates reply for turn_card.
%
create_backup_reply(State) ->
	case pexeso_lib:is_finished(State#state.game) of
		false -> {next_state, backup, State};
		true  -> {stop, normal, State}
	end.

%%
% Events in all states.
%

%%
% Go back to idle.
%
handle_event(pause, _StateName, S) -> 
	{next_state, idle, S};

%%
% Manual stop of server.
%
handle_event(stop, _StateName, S) -> 
	{stop, normal, S};

handle_event(Message, StateName, S) -> 
	unexpected(Message, StateName),
	{next_state, StateName, S}.

%%
% Returns list of unturned/available cards.
%
handle_sync_event(get_playground, _From, StateName, S) ->
	Cards = pexeso_lib:get_playground(S#state.game),
	{reply, Cards, StateName, S};

%%
% Returns statistics of current game.
%
handle_sync_event(get_stats, _From, StateName, S) ->
	Stats = pexeso_lib:get_stats(S#state.game),
	{reply, Stats, StateName, S};

handle_sync_event(Message, _From, StateName, S) -> 
	unexpected(Message, StateName),
	{reply, unexpected, StateName, S}.

%%
% Other messages.
%
handle_info(Message, StateName, S) ->
	unexpected(Message, StateName),
	{next_state, StateName, S}.

%%
% Terminate.
%
terminate(_Reason, _StateName, _State) -> ok.

%%
% Code reload.
%
code_change(_Old, StateName, State, _Extra) -> {ok, StateName, State}.

%%
% For all unexpected messages.
%
unexpected(Message, State) ->
	io:format("Game ~p received unknown event ~p while in state ~p~n", [self(), Message, State]).

%%
% Sets this game to be main game.
%
advance_to_main(BackupPid, S) ->

	% send game state to backup
	pexeso_game:set_backup(BackupPid, S#state.game, S#state.players),
	
	% send all game progress events to backup game
	start_notifying(pexeso_game, BackupPid, S#state.event_manager),
	
	% remember new backup pid
	S#state{
		other_pid = BackupPid
	}.

%%
% Adds new player to the (1) gen_event, (2) pexeso_lib and (3) list of players.
%
add_player(State, Name, PlayerPid) ->
	
	start_notifying(player, PlayerPid, State#state.event_manager),

	State#state{ 
		game = pexeso_lib:add_player(State#state.game, Name),
		players = dict:store(Name, PlayerPid, State#state.players)
	}.

%%
% Adds module and pid to the gen event.
%
start_notifying(Module, Pid, EventManager) ->

	gen_event:add_handler(
		EventManager, 
		pexeso_progress_handler, 
		{Module, Pid}
	).