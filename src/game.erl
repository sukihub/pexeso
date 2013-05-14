-module(game).
-behaviour(gen_server).
-export([start/0,start_link/2, turn/4, pick/5, fail/6]).
 
-export([ 
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3, start/0
]).

start() ->
	{ok, Pid} = gen_server:start(game,[],[]),
	{ok, Pid}.
 
start_link(Pid, PlayerPid) ->
	gen_server:call(Pid, {start_link, PlayerPid}).
 
turn(Pid, Card, Value, Player) ->
	gen_server:call(Pid, {turn, {Card, Value}, Player}).

pick(Pid, Card1, Card2, Value, Player) ->
	gen_server:call(Pid, {pick, {Card1, Card2, Value}, Player}).

fail(Pid, Card1, Card2, Value1, Value2, Player) ->
	gen_server:call(Pid, {turn, {Card1, Value1, Card2, Value2}, Player}).


init([]) ->
	{ok, Pid} = gen_event:start_link(),
	io:format("cosi ~p", [self()]),
	{ok, Pid}.

handle_call({turn, {Card, Value}, Player}, From, EventManagerPid) ->
	gen_event:notify(EventManagerPid, {turn, {Card, Value}, Player}),
	{reply, turned, EventManagerPid};

handle_call({pick, {Card1, Card2, Value}, Player}, From, EventManagerPid) ->
	gen_event:notify(EventManagerPid, {turn, {Card1, Card2, Value}, Player}),
	{reply, picked, EventManagerPid};

handle_call({fail, {Card1, Value1, Card2, Value2}, Player}, From, EventManagerPid) ->
	gen_event:notify(EventManagerPid, {turn, {Card1, Value1, Card2, Value2}, Player}),
	{reply, failed, EventManagerPid};	
	
handle_call({start_link, PlayerPid}, From, EventManagerPid) ->
	gen_event:add_handler(EventManagerPid, game_feed, [PlayerPid]),
	{reply, start_link, EventManagerPid}.	
	
handle_cast(stop, State) -> 
	{stop, normal, State};
	
handle_cast(_, State) ->
	{ok, State}.

handle_info(Info, State) ->
	io:format("Unexpected message: ~p~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _Extra) -> {ok, State}.