-module(player).
-behaviour(gen_server).
 
-export([
	start/1,
	game_feed/2, 
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

% public api

start(GamePid) ->
	gen_server:start(?MODULE, GamePid, []).

game_feed(Pid, Action) ->
	gen_server:cast(Pid, {game_feed, Action}).

turn_card(Pid, CardId) ->
	gen_server:cast(Pid, {turn_card, CardId}).

% gen server stuff

init(GamePid) ->
	pexeso_game:register_player(GamePid, self()),
	{ok, GamePid}.
 
handle_call(_, _From, State) ->
	{reply, ok, State}.

handle_cast({game_feed, {{turn, Card}, _Player}}, State) ->
	io:format("otocena karticka: ~p~n", [Card]),
	{noreply, State};

handle_cast({game_feed, {{pick, Cards}, _Player}}, State) ->
	io:format("zobrate karticky: ~p~n", [Cards]),
	{noreply, State};

handle_cast({game_feed, {{fail, Cards}, _Player}}, State) ->
	io:format("smola: ~p~n", [Cards]),
	{noreply, State};

handle_cast({turn_card, CardId}, State) ->
	pexeso_game:turn_card(State, self(), CardId),
	{noreply, State}.	
	
handle_info(Info, State) ->
	io:format("Unexpected message: ~p~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _Extra) -> {ok, State}.