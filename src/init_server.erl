-module(init_server).
-behaviour(gen_server).

-export([
	start/0,
	create_game/2, 
	register_game_server/2,
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

-record(state, {
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
	backup_count = 0
}).

% public api

start() ->
	gen_server:start_link(?MODULE, [], []).

create_game(Pid, Name) ->
	gen_server:cast(Pid, {create_game, Name}).
	
register_game_server(Pid, GameServerPid) ->
	gen_server:cast(Pid, {register_game_server, GameServerPid}).

stop(Pid) ->
	gen_server:cast(Pid, stop).

% gen server stuff

% inicializia
% nacita sa zoznam Pidciek zo suboru
% do suboru sa prida novovytvoreny (pre ucely testovania takto)
init([]) ->
	Pid = self(),
	writeline("init_servers.conf",Pid),
	InitServers = readlines("init_servers.conf"),
	{ok, #state{init_servers = InitServers, games = [], game_servers = []}}.
 

handle_call(_, _From, State) ->
	{reply, ok, State}.

% create_game
%
handle_cast({create_game,Name}, State) ->
	GameServer = find_least_busy_game_server(State#state.game_servers),
	% TODO vytvorit hru na serveri
	%zatial
	{ok, NewGamePid} = game:start(), 
	{ok, NewBackupPid} = game:start(), 
	Games = State#state.games,
	NewGame = #game{name = Name, game_pid = NewGamePid, backup_pid = NewBackupPid},
	NewState = State#state{games = [NewGame | Games]},
	%io:format("Unexpected message: ~p~n", [NewState]),
	{noreply, State};

% register_game_server
% zaregistruje novy game server	
handle_cast({register_game_server, GameServerPid}, State) ->
	NewGameServer = #game_server{pid = GameServerPid},
	GameServers = State#state.game_servers,
	NewState = State#state{game_servers = [NewGameServer | GameServers]},
	%io:format("Unexpected message: ~p~n", [NewState]),
	{noreply, NewState};
	
handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info({'DOWN', _, process, _, _Reason}, _State) ->
	exit(normal);

handle_info(Info, State) ->
	io:format("Unexpected message: ~p~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) -> ok.

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