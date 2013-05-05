-module(player).
-include_lib("pexeso.hrl").

-behaviour(gen_fsm).
-behaviour(pexeso_progress_listener).
 
-export([
	start_link/3,
	pexeso_progress/2, 
	turn_card/2,
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
	last_action = null
}).

% PUBLIC INTERFACE

start_link(GamePid, BackupPid, Name) ->
	gen_fsm:start_link(?MODULE, {GamePid, BackupPid, Name}, []).

pexeso_progress(Pid, Action) ->
	gen_fsm:send_all_state_event(Pid, {pexeso_progress, Action}).

turn_card(Pid, CardId) ->
	gen_fsm:send_event(Pid, {turn_card, CardId}).

new_servers(Pid, MainPid, BackupPid) ->
	gen_fsm:send_all_state_event(Pid, {new_servers, MainPid, BackupPid}).

stop(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).

% PRIVATE IMPLEMENTATION

init({GamePid, BackupPid, Name}) ->
	
	pexeso_game:register_player(GamePid, Name, self()),

	State = #state{
		name = Name,
		game_pid = GamePid,
		backup_pid = BackupPid,
		last_action = join
	},

	{ok, waiting, State, ?CLIENT_WAIT}.

%%
% In idle state, do nothing.
%
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
		last_action = {turn, CardId}
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
	% Wait for reply from backup server
	wait_for_backup_reply(S);

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
	% go to idle and wait for new game and backup servers
	{next_state, idle, S};

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
	io:format("otocena karticka: ~p~n", [Action]),
	{next_state, playing, State};

handle_event({pexeso_progress, Action = #action{move = #pick{}}}, _StateName, State) ->
	io:format("zobrate karticky: ~p~n", [Action]),
	{next_state, playing, State};

handle_event({pexeso_progress, Action = #action{move = #fail{}}}, _StateName, State) ->
	io:format("smola: ~p~n", [Action]),
	{next_state, playing, State};

handle_event({pexeso_progress, Action = #action{move = #invalid{}}}, _StateName, State) ->
	io:format("neplatny tah: ~p~n", [Action]),
	{next_state, playing, State};

handle_event({pexeso_progress, Action = #action{move = #join{}}}, _StateName, State) ->
	io:format("pridal sa hrac: ~p~n", [Action#action.player]),
	{next_state, playing, State};

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

handle_sync_event(Message, _From, StateName, S) ->
	unexpected(Message, StateName),
	{reply, unexpected, StateName, S}.

%handle_info({'DOWN', _, process, _, _Reason}, _State) ->
%	exit(normal);

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