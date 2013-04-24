-module(player).
-include_lib("pexeso.hrl").

-behaviour(gen_server).
-behaviour(pexeso_progress_listener).
 
-export([
	start_link/2,
	pexeso_progress/2, 
	turn_card/2
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
	name,
	game_pid,
	backup_pid = null
}).

% public api

start_link(GamePid, Name) ->
	gen_server:start_link(?MODULE, {GamePid, Name}, []).

pexeso_progress(Pid, Action) ->
	gen_server:cast(Pid, {pexeso_progress, Action}).

turn_card(Pid, CardId) ->
	gen_server:cast(Pid, {turn_card, CardId}).

% gen server stuff

init({GamePid, Name}) ->
	
	pexeso_game:register_player(GamePid, Name),
	monitor(process, GamePid),

	{ok, #state{game_pid = GamePid, name = Name}}.
 

handle_call(_, _From, State) ->
	{reply, ok, State}.

handle_cast({pexeso_progress, Action = #action{move = #move{type = turn}}}, State) ->
	io:format("otocena karticka: ~p~n", [Action]),
	{noreply, State};

handle_cast({pexeso_progress, Action = #action{move = #move{type = pick}}}, State) ->
	io:format("zobrate karticky: ~p~n", [Action]),
	{noreply, State};

handle_cast({pexeso_progress, Action = #action{move = #move{type = fail}}}, State) ->
	io:format("smola: ~p~n", [Action]),
	{noreply, State};

handle_cast({pexeso_progress, Action = #action{move = #move{type = join}}}, State) ->
	io:format("pridal sa hrac: ~p~n", [Action#action.player]),
	{noreply, State};

handle_cast({turn_card, CardId}, S) ->
	pexeso_game:turn_card(S#state.game_pid, S#state.name, CardId),
	{noreply, S}.	
	
handle_info({'DOWN', _, process, _, _Reason}, _State) ->
	exit(normal);

handle_info(Info, State) ->
	io:format("Unexpected message: ~p~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _Extra) -> {ok, State}.