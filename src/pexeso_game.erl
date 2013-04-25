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
	set_backup/2,

	register_player/2,
	get_playground/1, 
	get_stats/1,
	turn_card/3,
	heartbeat/1, 

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

set_backup(Pid, MainPid) ->
	gen_fsm:sync_send_event(Pid, {set_backup, MainPid}).

%%
% For state main.
%
register_player(Pid, Name) ->
	gen_fsm:sync_send_event(Pid, {register_player, Name}).

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
	% TODO zistit co robi link
	{ok, EventManagerPid} = gen_event:start_link(),

	% create new pexeso game
	% and add event manager
	State = #state{ 
		game = pexeso_lib:create(CardTypes),
		event_manager = EventManagerPid 
	},

	% go to state idle
	{ok, idle, State}.

%%
% Idle state.
%

idle({set_main, BackupPid}, _From, S) ->

	State = S#state{
		other_pid = BackupPid
	},

	% send all game progress events to backup game
	gen_event:add_handler(
		S#state.event_manager,
		pexeso_progress_handler,
		{pexeso_game, BackupPid}
	),

	{reply, ok, main, State};


idle({set_backup, MainPid}, _From, S) ->

	State = S#state{ 
		other_pid = MainPid 
	},

	% remember to send heartbeat every now and then
	{reply, ok, backup, State, ?HEARTBEAT_DELAY};


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
% Addss new player into the game.
%
main({register_player, Name}, {PlayerPid, _}, S) ->

	% firstly notify all existing pexeso listeners
	gen_event:notify(
		S#state.event_manager, 
		#action{move = #join{pid = PlayerPid}, player = Name}
	),

	% now add player
	State = add_player(S, Name, PlayerPid),	
	
	{reply, ok, main, State};

%%
% Received heartbeat from backup game.
% Only allowed to be called by backup game process itself.
%
main(heartbeat, {BackupPid, _}, S) when S#state.other_pid == BackupPid ->
	io:format("Backup is ok"),
	{reply, ok, main, S};

main(Message, _From, S) ->
	unexpected(Message, main),
	{reply, unexpected, main, S}.

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

		create_reply(main, State)

	catch

		throw:card_not_found -> {next_state, main, S};
		throw:player_not_found -> {next_state, main, S}

	end;

main(Message, S) ->
	unexpected(Message, main),
	{next_state, main, S}.

%%
% Creates reply for turn_card.
%
create_reply(StateName, State) ->
	case pexeso_lib:is_finished(State#state.game) of
		false -> {next_state, StateName, State};
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
% Other actions.
%
backup({pexeso_progress, Action = #action{}}, S) ->

	State = S#state{ 
		game = pexeso_lib:apply_action(S#state.game, Action) 
	},

	create_reply(backup, State);


backup(Message, S) ->
	unexpected(Message, backup),
	{next_state, backup, S}.

backup(Message, _From, S) ->
	unexpected(Message, backup),
	{reply, unexpected, backup, S}.

%%
% Events in all states.
%

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
	io:format("~p received unknown event ~p while in state ~p~n", [self(), Message, State]).


add_player(State, Name, PlayerPid) ->
	
	gen_event:add_handler(
		State#state.event_manager, 
		pexeso_progress_handler, 
		{player, PlayerPid}
	),

	State#state{ 
		game = pexeso_lib:add_player(State#state.game, Name)
	}.