-module(pexeso_game_heartbeat).
-include_lib("pexeso.hrl").

-behaviour(gen_fsm).

-export([
	start_link/1,
	set_listening/1,
	set_sending/2,
	send/1,
	stop/1
]).

-export([
	init/1,

	idle/2, idle/3,
	listening/2, listening/3,
	sending/2, sending/3,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4
]).

-record(state, {
	owner_pid,
	other_pid = null
}).

-define(LISTENING_TIMEOUT, 11000).
-define(SENDING_TIMEOUT, 5000).

%%
% PUBLIC INTERFACE
%

start_link(OwnerPid) ->
	gen_fsm:start_link(?MODULE, OwnerPid, []).

set_listening(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, set_listening).

set_sending(Pid, MainPid) ->
	gen_fsm:sync_send_all_state_event(Pid, {set_sending, MainPid}).

send(Pid) ->
	gen_fsm:send_event(Pid, heartbeat).

stop(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).

%%
% PRVATE IMPLEMENTATION
%
init(OwnerPid) ->
	State = #state{ owner_pid = OwnerPid },
	{ok, idle, State}.

idle(_Message, S) ->
	{next_state, idle, S}.

idle(_Message, _From, S) ->
	{reply, unexpected, idle, S}.


listening(timeout, S) ->
	io:format("Did not receive heartbeat from backup~n"),
	pexeso_game:no_heartbeat(S#state.owner_pid),
	{next_state, listening, S, ?LISTENING_TIMEOUT};

listening(heartbeat, S) ->
	{next_state, listening, S, ?LISTENING_TIMEOUT};

listening(_Message, S) ->
	{next_state, listening, S, ?LISTENING_TIMEOUT}.

listening(_Message, _From, S) ->
	{reply, unexpected, listening, S, ?LISTENING_TIMEOUT}.	


sending(timeout, S) ->
	pexeso_game_heartbeat:send(S#state.other_pid),
	{next_state, sending, S, ?SENDING_TIMEOUT};

sending(_Message, S) ->
	{next_state, sending, S, ?SENDING_TIMEOUT}.

sending(_Message, _From, S) ->
	{reply, unexpected, sending, S, ?SENDING_TIMEOUT}.	


handle_event(stop, _StateName, S) ->
	{stop, normal, S};

handle_event(_Message, StateName, S) ->
	{next_state, StateName, S}.
	

handle_sync_event(set_listening, {OwnerPid, _}, _StateName, S) when OwnerPid == S#state.owner_pid ->
	
	io:format("Started listening for backup~n"),
	
	{reply, ok, listening, S, ?LISTENING_TIMEOUT};


handle_sync_event({set_sending, MainPid}, {OwnerPid, _}, _StateName, S) when OwnerPid == S#state.owner_pid ->
	
	io:format("Started sending heartbeat~n"),
	
	State = S#state{ other_pid = MainPid },
	pexeso_game_heartbeat:send(MainPid),

	{reply, ok, sending, State, ?SENDING_TIMEOUT};


handle_sync_event(_Message, _From, StateName, S) ->
	{reply, unexpected, StateName, S}.

handle_info(Info, _StateName, State) ->
	io:format("Unexpected message: ~p~n", [Info]),
	{noreply, State}.

terminate(_Reason, _StateName, _State) -> ok.

code_change(_Old, _StateName, State, _Extra) -> {ok, State}.