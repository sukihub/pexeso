-module(game_server).
-behavior(gen_server).

-export([
	start_link/0,
	create_game/3,
	stop/1
]).

-export([ 
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-define(TIMEOUT, 120000).

% PUBLIC API

start_link() ->
	gen_server:start_link(?MODULE, nothing, []).

create_game(Pid, Name, Cards) ->
	gen_server:call(Pid, {create_game, Name, Cards}).

stop(Pid) ->
	gen_server:cast(Pid, stop).

% GEN_SERVER

init(_) ->
	io:format("Game server ~p stared~n", [self()]),
	register(),
	{ok, stateless, ?TIMEOUT}.

handle_call({create_game, Name, Cards}, _From, State) ->
	{ok, Game} = pexeso_game:start(Name, Cards),
	{reply, Game, State, ?TIMEOUT};

handle_call(Message, _From, State) ->
	unexpected(Message),
	{reply, unexpected, State, ?TIMEOUT}.

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(Message, State) ->
	unexpected(Message),
	{noreply, State, ?TIMEOUT}.

handle_info(timeout, State) ->
	register(),
	{noreply, State, ?TIMEOUT};

handle_info(Message, State) ->
	unexpected(Message),
	{noreply, State, ?TIMEOUT}.

terminate(_Reason, _State) ->
	io:format("Game server ~p stopped~n", [self()]),
	ok.

code_change(_Old, State, _Extra) -> 
	{ok, State}.

unexpected(Message) ->
	io:format("Game server ~p received unexpected message ~p~n", [self(), Message]).

register() ->
	io:format("Registering game server ~p~n", [self()]),
	init_server:register_game_server(self()).
