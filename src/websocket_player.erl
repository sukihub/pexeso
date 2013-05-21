-module(websocket_player).
-include_lib("pexeso.hrl").

-behaviour(gen_fsm).
-behaviour(pexeso_progress_listener).
 
-export([
	start/3,
	pexeso_progress/2, 
	turn_card/2,
	get_stats/1,
	get_playground/1,
	new_servers/3,
	stop/1
]).

-export([ 
	init/1,
	terminate/3,
	code_change/4,
	handle_info/3,

	idle/2, idle/3,
	playing/2, playing/3,
	waiting/2, waiting/3,
	waiting_backup/2, waiting_backup/3,
	handle_event/3, handle_sync_event/4
]).

-record(state, {
	name,
	game_pid,
	backup_pid,
	last_action = null,
	redoing = false,
	websocket
}).

% PUBLIC INTERFACE

start(Pids, Name, WebSocket) ->
	gen_fsm:start(?MODULE, {Pids, Name, WebSocket}, []).

pexeso_progress(Pid, Action) ->
	gen_fsm:send_all_state_event(Pid, {pexeso_progress, Action}).

turn_card(Pid, CardId) ->
	gen_fsm:send_event(Pid, {turn_card, CardId}).

new_servers(Pid, MainPid, BackupPid) ->
	gen_fsm:send_all_state_event(Pid, {new_servers, MainPid, BackupPid}).

get_stats(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, get_stats).

get_playground(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, get_playground).

stop(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).

% PRIVATE IMPLEMENTATION

init({{GamePid, BackupPid}, Name, WebSocket}) ->
	
	pexeso_game:register_player(GamePid, Name, self()),

	State = #state{
		name = Name,
		game_pid = GamePid,
		backup_pid = BackupPid,
		last_action = join,
		websocket = WebSocket,
		redoing = false
	},

	{ok, waiting, State, ?CLIENT_WAIT}.

%%
% In idle state, do nothing.
%
idle(timeout, S) ->
	io:format("Player ~p shutting down~n", [S#state.name]),
	{stop, normal, S};

idle(Message, S) ->
	unexpected(Message, idle),
	{next_state, idle, S}.

idle(Message, _From, S) ->
	unexpected(Message, idle),
	{reply, unexpected, idle, S}.

%%
% In playing state, player can turn cards.
%
playing({turn_card, CardId}, S) ->

	% remember last action	
	State = S#state{
		last_action = {turn, CardId},
		redoing = false
	},

	% execute action
	pexeso_game:turn_card(S#state.game_pid, S#state.name, CardId),
	% wait for reply from game
	wait_for_reply(State);


playing(Message, S) ->
	unexpected(Message, playing),
	{next_state, playing, S}.

playing(Message, _From, S) ->
	unexpected(Message, playing),
	{reply, unexpected, playing, S}.


%%
% Main game server did not respond.
%
waiting(timeout, S) ->
	
	% Redo action on backup server
	redo_last_action(S#state.backup_pid, S),
	State = S#state{ redoing = true },

	% Wait for reply from backup server
	wait_for_backup_reply(State);



waiting(Message, S) ->
	unexpected(Message, waiting),
	wait_for_reply(S).

waiting(Message, _From, S) ->
	unexpected(Message, waiting),
	{reply, unexpected, waiting, S, ?CLIENT_WAIT}.

%%
% Even backup game server did not respond.
%
waiting_backup(timeout, S) ->
	% log, haha ;-)
	io:format("Both main game and backup game did not reply, ~p idely sitting~n", [S#state.name]),

	State = S#state{redoing = false},

	% go to idle and wait for new game and backup servers
	{next_state, idle, State, 10000};


waiting_backup(Message, S) ->
	unexpected(Message, waiting_backup),
	wait_for_backup_reply(S).

waiting_backup(Message, _From, S) ->
	unexpected(Message, waiting_backup),
	{reply, unexpected, waiting_backup, S, ?CLIENT_WAIT}.

%%
% Game progress events.
% After receiving one, always go to 'playing' state.
%

handle_event({pexeso_progress, Action = #action{move = #turn{}}}, _StateName, State) ->
	
	{CardId, Content} = Action#action.move#turn.card,

	Json = create_json_command(action_turn, [
		{player, Action#action.player},
		{card_id, CardId},
		{content, Content}
	]),

	State#state.websocket ! {text, Json},

	{next_state, playing, check_for_redo(State)};


handle_event({pexeso_progress, Action = #action{move = #close{}}}, _StateName, State) ->
	
	io:format("Closing ~p~n", [Action]),

	{CardId, Content} = Action#action.move#close.card,

	Json = create_json_command(action_close, [
		{player, Action#action.player},
		{card_id, CardId},
		{content, Content}
	]),

	State#state.websocket ! {text, Json},

	{next_state, playing, check_for_redo(State)};


handle_event({pexeso_progress, Action = #action{move = #pick{}}}, _StateName, State) ->
	
	{CardId1, Content1} = Action#action.move#pick.card_a,
	{CardId2, Content2} = Action#action.move#pick.card_b,

	Json = create_json_command(action_pick, [
		{player, Action#action.player},
		{card_id_1, CardId1},
		{content_1, Content1},
		{card_id_2, CardId2},
		{content_2, Content2}
	]),

	State#state.websocket ! {text, Json},

	{next_state, playing, check_for_redo(State)};


handle_event({pexeso_progress, Action = #action{move = #fail{}}}, _StateName, State) ->
		
	{CardId1, Content1} = Action#action.move#fail.card_a,
	{CardId2, Content2} = Action#action.move#fail.card_b,

	Json = create_json_command(action_fail, [
		{player, Action#action.player},
		{card_id_1, CardId1},
		{content_1, Content1},
		{card_id_2, CardId2},
		{content_2, Content2}
	]),

	State#state.websocket ! {text, Json},

	{next_state, playing, check_for_redo(State)};


handle_event({pexeso_progress, Action = #action{move = #invalid{}}}, _StateName, State) ->
	io:format("neplatny tah: ~p~n", [Action]),
	{next_state, playing, check_for_redo(State)};


handle_event({pexeso_progress, Action = #action{move = #join{}}}, _StateName, State) ->
	
	Json = create_json_command(action_join, [
		{player, Action#action.player}
	]),
	
	State#state.websocket ! {text, Json},

	{next_state, playing, check_for_redo(State)};


handle_event({pexeso_progress, {stop, _Stats}}, _StateName, State) ->
	
	Json = create_json_command(action_stop, []),
	State#state.websocket ! {text, Json},

	{stop, normal, State};


%%
% Hey, here are new main and backup games.
%
handle_event({new_servers, MainPid, BackupPid}, _StateName, S) ->
	
	io:format("nove herne procesy~n"),

	% save pids of new main and backup game
	State = S#state{
		game_pid = MainPid,
		backup_pid = BackupPid
	},

	{next_state, playing, State};

%%
% Stop yourself.
%
handle_event(stop, _StateName, S) ->
	{stop, normal, S};

handle_event(Message, StateName, S) ->
	unexpected(Message, StateName),
	{next_state, StateName, S}.

handle_sync_event(get_stats, _From, StateName, S) ->
	% TODO what if main is down
	{reply, pexeso_game:get_stats(S#state.game_pid), StateName, S};

handle_sync_event(get_playground, _From, StateName, S) ->
	% TODO what if backup is down
	{reply, pexeso_game:get_playground(S#state.game_pid), StateName, S};

handle_sync_event(Message, _From, StateName, S) ->
	unexpected(Message, StateName),
	{reply, unexpected, StateName, S}.

handle_info(Info, _StateName, State) ->
	io:format("Unexpected message: ~p~n", [Info]),
	{noreply, State}.

terminate(_Reason, _StateName, _State) -> ok.

code_change(_Old, _StateName, State, _Extra) -> {ok, State}.

%%
% Generate return value that will wait for reply from game server.
%
wait_for_reply(State) ->
	{next_state, waiting, State, ?CLIENT_WAIT}.

%%
% Generate return value that will wait for reply from backup server.
%
wait_for_backup_reply(State) ->
	{next_state, waiting_backup, State, ?CLIENT_WAIT}.

%%
% Re-does last action, that probably failed to execute.
%
redo_last_action(TargetPid, S = #state{ last_action = join }) ->
	pexeso_game:register_player(TargetPid, S#state.name, self());

redo_last_action(TargetPid, S = #state{ last_action = {turn, CardId} }) ->
	pexeso_game:turn_card(TargetPid, S#state.name, CardId).

%%
% For all unexpected messages.
%
unexpected(Message, State) ->
	io:format("Player ~p received unknown event ~p while in state ~p~n", [self(), Message, State]).


create_json_command(Name, Params) ->
	mochijson2:encode(
		{struct, [
			{command, Name},
			{params, {struct, Params}}
		]}		
	).

check_for_redo(State) ->
	case State#state.redoing of
		true ->
			io:format("~p was redoing, requesting new game pids~n", [State#state.name]),
			{MainPid, BackupPid} = pexeso_game:request_new_pids(State#state.backup_pid),
			State#state{
				game_pid = MainPid,
				backup_pid = BackupPid
			};
		false ->
			State
	end.
