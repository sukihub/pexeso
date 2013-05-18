-module(pexeso_game).
-include_lib("pexeso.hrl").

-behaviour(gen_fsm).
-behaviour(pexeso_progress_listener).

-export([
	start/2, 

	set_main/2,
	set_backup/5,

	register_player/3,
	get_playground/1, 
	get_stats/1, get_stats/2,
	turn_card/3,

	pause/1,

	pexeso_progress/2,
	no_heartbeat/1,

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
	name,
	game,
	players,
	other_pid = null,
	event_manager,
	heartbeat
}).

%%
% PUBLIC INTERFACE
%

start(Name, CardTypes) when is_list(CardTypes) -> 
	gen_fsm:start(?MODULE, {Name, CardTypes}, []).

%%
% For state init.
%
set_main(Pid, BackupPid) ->
	gen_fsm:sync_send_event(Pid, {set_main, BackupPid}).

set_backup(Pid, Name, Game, Players, HeartbeatPid) ->
	gen_fsm:sync_send_event(Pid, {set_backup, Name, Game, Players, HeartbeatPid}).

%%
% For state main.
%
register_player(Pid, Name, PlayerPid) ->
	gen_fsm:send_event(Pid, {register_player, Name, PlayerPid}).

turn_card(Pid, Name, Card) ->
	gen_fsm:send_event(Pid, {turn_card, Name, Card}).

no_heartbeat(Pid) ->
	gen_fsm:send_event(Pid, no_heartbeat).

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

get_stats(Pid, Timeout) ->
	gen_fsm:sync_send_all_state_event(Pid, get_stats, Timeout).

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
init({Name, CardTypes}) -> 

	% start event manager process
	{ok, EventManagerPid} = gen_event:start_link(),
	% start heartbeat process
	{ok, HeartbeatPid} = pexeso_game_heartbeat:start_link(self()),

	% create new pexeso game
	% and add event manager
	State = #state{ 
		name = Name,
		game = pexeso_lib:create(CardTypes),
		players = dict:new(),
		event_manager = EventManagerPid,
		heartbeat = HeartbeatPid
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
idle({set_backup, Name, Game, Players, HeartbeatPid}, {From, _}, S) ->

	% put game and players into state
	State = S#state{
		name = Name, 
		game = Game,
		players = Players,
		other_pid = From
	},

	% register all players in gen_event
	[ start_notifying(player, PlayerPid, S#state.event_manager) || {_, PlayerPid} <- dict:to_list(Players) ],

	% start sending heartbeat
	pexeso_game_heartbeat:set_sending(S#state.heartbeat, HeartbeatPid),

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

%%
% Received heartbeat from backup game.
% Only allowed to be called by backup game process itself.
%
main(no_heartbeat, S) ->
	
	% contact init server to see what the hell is going on
	case init_server:backup_down(S#state.name, self(), S#state.other_pid) of

		% ignore no_heartbeat event, backup works as expected
		nope -> 
			{next_state, main, S};

		% here is your new backup
		{yop, NewBackupPid} ->

			% assign new backup, notify players
			State = new_backup(NewBackupPid, S),

			{next_state, main, State}

	end;

main(Message, S) ->
	unexpected(Message, main),
	{next_state, main, S}.

%%
% Creates reply for turn_card.
%
create_main_reply(State) ->

	case pexeso_lib:is_finished(State#state.game) of
		
		false -> 
			{next_state, main, State};

		true  -> 

			gen_event:notify(
				State#state.event_manager, 
				{stop, pexeso_lib:get_stats(State#state.game)}
			),

			init_server:game_finished(State#state.name),

			{stop, normal, State}

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
% We are done.
%
backup({pexeso_progress, {stop, _}}, S) ->
	{stop, normal, S};

%%
% Backup receives gameplay event, which suggests that main is down.
%
backup(Message = {Event, _, _}, S) when Event == register_player; Event == turn_card ->
	
	% contact init server to see what the hell is going on
	case init_server:main_down(S#state.name, S#state.other_pid, self()) of

		% ignore gameplay event, main works as expected
		nope -> 
			{next_state, backup, S};

		% you are main a here is your new backup
		{yop, NewBackupPid} ->

			% assign new backup, notify players
			State = new_backup(NewBackupPid, S),

			% resend original message to self (will already be in main state)
			gen_fsm:send_event(self(), Message),
			
			{next_state, main, State}

	end;
	

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
terminate(_Reason, _StateName, State) -> 
	pexeso_game_heartbeat:stop(State#state.heartbeat),
	ok.

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
% Assigns new backup to this game and notifies players about the change.
%
new_backup(NewBackupPid, S) ->

	% advance self to main, and push game state to new backup
	State = advance_to_main(NewBackupPid, S),

	% notify players about the change
	[ player:new_servers(PlayerPid, self(), NewBackupPid) || {_, PlayerPid} <- dict:to_list(State#state.players) ],

	State.


%%
% Sets this game to be main game.
%
advance_to_main(BackupPid, S) ->

	% wait for heartbeats from backup
	pexeso_game_heartbeat:set_listening(S#state.heartbeat),

	% send game state to backup
	pexeso_game:set_backup(BackupPid, S#state.name, S#state.game, S#state.players, S#state.heartbeat),
	
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