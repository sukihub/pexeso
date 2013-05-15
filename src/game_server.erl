-module(game_server).
-behavior(gen_server).

-export([
	start_link/1,
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

-record(state, {
	init_servers
}).

% PUBLIC API

start_link(InitServers) ->
	gen_server:start_link(?MODULE, InitServers, []).

create_game(Pid, Name, Cards) ->
	gen_server:call(Pid, {create_game, Name, Cards}).

stop(Pid) ->
	gen_server:cast(Pid, stop).

% GEN_SERVER

init(InitServers) ->

	io:format("Game server ~p stared~n", [self()]),

	random:seed(now()),
	register(InitServers),

	{ok, #state{ init_servers = InitServers }, ?TIMEOUT}.


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
	register(State#state.init_servers),
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

register(InitServers) ->
	Index = random:uniform(length(InitServers)),
	Init = lists:nth(Index, InitServers),
	io:format("Registering game server ~p at ~p~n", [self(), Init]),
	init_server:register_game_server(Init, self()).
