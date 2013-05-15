-module(init_server).
-behaviour(gen_server).
-define(DELAY, 10000).

-export([
	start_link/3,
	gossip_games/2,
	create_game/2,
	get_time/1,	
	register_game_server/2,
	stop/1,
	create_gossip_message/4
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
	id,
	vector_clock,
	init_servers_count,
	init_servers,
	games,
	game_servers
}).

-record(game, {
	game_pid,
	backup_pid,
	name
}).

-record(game_server, {
	pid,
	games_count = 0,
	backup_count = 0,
	dead = 0
}).

% public api

start_link(Name, Id, ServersCount) ->
	gen_server:start_link({local, Name}, ?MODULE, {Name, Id, ServersCount}, []).

create_game(Pid, Name, Cards) ->
	gen_server:cast(Pid, {create_game, Name, Cards}).
	
register_game_server(Pid, GameServerPid) ->
	gen_server:cast(Pid, {register_game_server, GameServerPid}).
	
get_time(Pid) ->
	gen_server:cast(Pid, {get_time}).
	
gossip_games(Pid, Msg) ->
	gen_server:cast(Pid, {gossip_games, Msg}).

stop(Pid) ->
	gen_server:cast(Pid, stop).

% gen server stuff

% inicializia
% nacita sa zoznam Pidciek zo suboru
% do suboru sa prida novovytvoreny (pre ucely testovania takto)
init({Name, Id, ServersCount}) ->
	io:format("Init server ~p ~p started~n", [Name, self()]),
	Pid = self(),
	%writeline("init_servers.conf",Pid),
	%InitServers = readlines("init_servers.conf"),
	VectorClock = dict:new(),
	State = #state{init_servers = [], games = [], game_servers = [], vector_clock = dict:store(Id,0,VectorClock), id = Id, init_servers_count = ServersCount},
	erlang:send_after(?DELAY, self(), trigger),
	{ok, State}.
 

handle_call(_, _From, State) ->
	{reply, ok, State}.

% create_game
%
handle_cast({create_game, Name, Cards}, State) ->
	
	% TODO uistit sa, ze GameServer a BackupServer su ROZNE!!
	% mozno bude lepsie, ak sa v jednom kroku return-nu obe
	GameServer = find_least_busy_game_server(State#state.game_servers),
	BackupServer = find_least_busy_game_server(State#state.game_servers),

	% TODO co ak nie su dostupne ziadne (aspon 2) game servery? 
	% hodit chybu alebo vratit error

	% create games
	NewGamePid = game_server:create_game(GameServer, Name, Cards), 
	NewBackupPid = game_server:create_game(BackupServer, Name, Cards), 
	pexeso_game:set_main(NewGamePid, NewBackupPid),

	Games = State#state.games,
	Id = State#state.id,
	NewGame = #game{name = Name, game_pid = NewGamePid, backup_pid = NewBackupPid},
	
	VectorClock = State#state.vector_clock,
	
	NewState = State#state{games = [NewGame | Games], vector_clock = time_update(Id, VectorClock)},

	
	{noreply, NewState};


% register_game_server
% zaregistruje novy game server	
handle_cast({register_game_server, GameServerPid}, State) ->
	NewGameServer = #game_server{pid = GameServerPid},
	GameServers = State#state.game_servers,
	NewState = State#state{game_servers = [NewGameServer | GameServers]},
	%io:format("Unexpected message: ~p~n", [NewState]),
	{noreply, NewState};

handle_cast({get_time}, State) ->
	VectorClock = State#state.vector_clock,
	Id = State#state.id,
	%CurrentTime = dict:fetch(self(),VectorClock),
	CurrentTime = time_get(Id, VectorClock),
	io:format("Unexpected message: ~p~n", [CurrentTime]),
	{noreply, State};
	
handle_cast({gossip_games, Msg}, State) ->
	{FromGames, FromId, FromTime} = Msg,
	VectorClock = State#state.vector_clock,
	LocalGames = State#state.games,
	Id = State#state.id,
	LocalFromTime = time_get(FromId, VectorClock),
	io:format("~p Received gossip: ~p ~p ~p ~n", [Id, FromGames, FromId, FromTime]),
	{RecentTime, RecentGames} = resolve_lists_games(LocalGames, LocalFromTime, FromGames, FromTime),
	NewState = State#state{games = RecentGames, vector_clock = dict:store(FromId, RecentTime, VectorClock)},
	{noreply, NewState};
	
handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info({'DOWN', _, process, _, _Reason}, _State) ->
	exit(normal);
	
handle_info(trigger, State) ->
	
	Games = State#state.games,
	Id = State#state.id,
	VectorClock = State#state.vector_clock,
	Count = State#state.init_servers_count,
	CurrentTime = time_get(Id, VectorClock),
	{ToPid, Msg} = create_gossip_message(Id, Games, CurrentTime, Count), %FromId, List, CurrentTime, InitServersCount
	?MODULE:gossip_games(ToPid, Msg),
	
	io:format("init_server_~p: ~p~n", [Id, length(Games)]),
	erlang:send_after(?DELAY, self(), trigger),
	
	{noreply, State};

handle_info(Info, State) ->
	io:format("Unexpected message: ~p~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) -> 
	io:format("Init server ~p stopped~n", [self()]),
	ok.

code_change(_Old, State, _Extra) -> {ok, State}.


% zapisanie PID do suboru
writeline(FileName,Pid) ->
	file:write_file(FileName, io_lib:fwrite("~p\n", [Pid]), [append]).

% nacitanie vsetkych PID zo suboru
readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

get_all_lines(Device, Accum) ->
    case io:fread(Device, "", "~s") of
        eof  -> file:close(Device), Accum;
        {ok, [N]} -> get_all_lines(Device, Accum ++ [list_to_pid(N)])
    end.
	
	
time_update(Id, VectorClock) ->
	CurrentTime = dict:fetch(Id,VectorClock),
	dict:store(Id,CurrentTime+1,VectorClock).
	%NewState = State#state{vector_clock = dict:store(self(),CurrentTime+1)},
	%NewState.
	
time_get(Id, VectorClock) ->
	case dict:is_key(Id, VectorClock) of
		true -> 
			dict:fetch(Id,VectorClock);
		false ->
			%NewVectorClock = dict:store(Id,0,VectorClock),
			%{dict:fetch(Id,NewVectorClock), NewVectorClock}
			0
		end.
	
	
resolve_lists_games(CurrentGames,CurrentTime,ColleaguesGames,ColleaguesTime) ->
	case CurrentTime >= ColleaguesTime of
		true -> {CurrentTime, CurrentGames};
		false -> {ColleaguesTime, ColleaguesGames}
	end.

create_gossip_message(FromId, List, CurrentTime, InitServersCount) ->
	ToId = random:uniform(InitServersCount),
	ToPid = list_to_atom("init_server"++lists:flatten(io_lib:format("~p", [ToId]))),
	Msg = {List, FromId, CurrentTime},
	{ToPid, Msg}.

% najdenie game servera s najmenej hrami	
find_least_busy_game_server([]) ->
	nil;
find_least_busy_game_server(GameServers = [H|T]) ->
	find_least_busy_game_server(T, H).
	
find_least_busy_game_server([],Min) ->
	Min;	
find_least_busy_game_server(GameServers = [H|T],Min) ->
	case Min#game_server.games_count > H#game_server.games_count of
		true -> find_least_busy_game_server(T, H);
		false -> find_least_busy_game_server(T, Min)
	end.