-module(player).
-behaviour(gen_server).
 
-export([ 
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3,
	game_feed/2, start/1
]).

game_feed(Pid, Action) ->
	gen_server:call(Pid, {game_feed, Action}).
		
start(GamePid) ->
	{ok, PlayerPid} = gen_server:start(player,[GamePid],[]),
	%subscribe player,
	pexeso_game:register_player(GamePid, PlayerPid),
	%subscribe sockets,
	{ok, PlayerPid}.

init([GamePid]) ->
	{ok, GamePid}.
 
handle_call({game_feed, {{turn, {Card, Value}}, Player}}, From, State) ->
	io:format("otocena karticka: ~p~n", [Value]),
	{reply, turned, State};

handle_call({game_feed, {{pick, {Card1, Card2, Value}}, Player}}, From, State) ->
	{reply, picked, State};

handle_call({game_feed, {{fail, {Card1, Value1, Card2, Value2}}, Player}}, From, State) ->
	{reply, failed, State}.	
	
handle_cast(stop, State) -> 
	{stop, normal, State};
	
handle_cast(_, State) ->
	{ok, State}.

handle_info(Info, State) ->
	io:format("Unexpected message: ~p~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _Extra) -> {ok, State}.