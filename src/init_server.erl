-module(init_server).
-behaviour(gen_server).
-define(DELAY, 10000).

-export([
	start_link/2,
	gossip_games/2,
	gossip_game_servers/2,
	create_game/2,
	get_games/0,
	get_game_servers/0,
	get_game/1,
	get_time/2,
	game_finished/1,
	register_game_server/1,
	main_down/3,
	backup_down/3,
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
	id,
	vector_clock,
	init_servers,
	games,
	game_servers,
	vector_clock_games = dict:new(),
	vector_clock_game_servers = dict:new()
}).

-record(game, {
	name,
	game_pid,
	backup_pid,
	finished = false
}).

-record(game_server, {
	finished = false
}).

% public api

start_link(Name, OthersNames) ->
	gen_server:start_link(?MODULE, {Name, OthersNames}, []).

create_game(Name, Cards) ->
	gen_server:cast(pexeso_supervisor:pick_init_server(), {create_game, Name, Cards}).

get_games() ->
	gen_server:call(pexeso_supervisor:pick_init_server(), get_games).

get_game_servers() ->
	gen_server:call(pexeso_supervisor:pick_init_server(), get_game_servers).

get_game(Name) ->
	try_all(pexeso_supervisor:shuffle_init_servers(), {get_game, Name}).

register_game_server(GameServerPid) ->
	gen_server:cast(pexeso_supervisor:pick_init_server(), {register_game_server, GameServerPid}).
	
%get_time(Name, GameName) ->
%	gen_server:cast(global:whereis_name(Name), {get_time, GameName}).

game_finished(Name) ->
	gen_server:cast(pexeso_supervisor:pick_init_server(), {game_finished, Name}).

gossip_games(Name, Msg) ->
	gen_server:cast(global:whereis_name(Name), {gossip_games, Msg}).

gossip_game_servers(Pid, Msg) ->
	gen_server:cast(Pid, {gossip_game_servers, Msg}).

main_down(Name, Main, Backup) ->
	try_all(pexeso_supervisor:shuffle_init_servers(), {main_down, Name, Main, Backup}).

backup_down(Name, Main, Backup) ->
	try_all(pexeso_supervisor:shuffle_init_servers(), {backup_down, Name, Main, Backup}).

try_all([ InitServer | InitServers ], Message) ->
	Pid = global:whereis_name(InitServer),
	case gen_server:call(Pid, Message) of
		try_next -> try_all(InitServers, Message);
		Else -> Else
	end;

try_all([], _Message) ->
	throw(not_found_on_any_init).

stop(Name) ->
	gen_server:cast(global:whereis_name(Name), stop).

% gen server stuff

%%
% Init server initialization.
%
init({Name, OthersNames}) ->

	global:register_name(Name, self()),
	io:format("Init server ~p ~p started~n", [Name, self()]),

	random:seed(now()),

	VectorClock = dict:new(),

	State = #state{
		games = dict:new(),
		game_servers = dict:new(),
		vector_clock = dict:store(Name, 0, VectorClock),
		id = Name,
		init_servers = lists:delete(Name, OthersNames)
	},

	erlang:send_after(?DELAY, self(), trigger),

	{ok, State}.


%%
% Returns "pretty" list of currently running games.
%
handle_call(get_games, _From, State) ->
	Result = [ {G#game.name, G#game.game_pid, G#game.backup_pid} || {_, G} <- dict:to_list(State#state.games), G#game.finished == false ],
	{reply, Result, State};

%%
% Returns list of currently running games.
%
handle_call(get_game_servers, _From, State) ->
	Result = [ {G#game_server.finished} || {_, G} <- dict:to_list(State#state.game_servers) ],
	{reply, Result, State};

%%
% Returns "pretty" details of given game.
%
handle_call({get_game, Name}, _From, State) ->

	case dict:find(Name, State#state.games) of

		error ->
			{reply, try_next, State};

		{ok, Game} ->
			Result = { Game#game.game_pid, Game#game.backup_pid },
			{reply, Result, State}

	end;

%%
% Some game reports that its main or backup is down.
%
handle_call({Down, Name, MainPid, BackupPid}, _From, State) when Down == main_down; Down == backup_down ->

	case dict:find(Name, State#state.games) of

		error ->
			{reply, try_next, State};

		{ok, Game} ->

			case is_game_running(Down, MainPid, BackupPid) of

				true -> {reply, nope, State};

				false ->

					try
						% replace game
						{NewPid, NewState} = replace_game(Down, Game, MainPid, BackupPid, State),

						% return correct response
						{reply, {yop, NewPid}, NewState}

					catch
						throw:not_enough_game_servers -> {reply, try_next, State}
					end

			end

	end;


%%
% Do not crash if somebody sends stupid message.
%
handle_call(_, _From, State) ->
	{reply, ok, State}.


%%
% Creates new pexeso game at least busy game servers.
%
handle_cast({create_game, Name, Cards}, State) ->

	try

		ensure_game_not_exists(Name, State),
		NewState = initialize_games(Name, Cards, State),

		{noreply, NewState}

	catch

		throw:game_already_exists ->
			io:format("Game with name ~p already exists, choose better one~n", [Name]),
			{noreply, State};

		throw:not_enough_game_servers ->
			io:format("~p does not have enough game servers to start new game~n", [State#state.id]),
			{noreply, State}

	end;


% register_game_server
% zaregistruje novy game server
handle_cast({register_game_server, GameServerPid}, State) ->

	case dict:is_key(GameServerPid, State#state.game_servers) of

		true ->
			{noreply, State};

		false ->
			GameServers = dict:store(
				GameServerPid,
				#game_server{},
				State#state.game_servers),
			NewState = State#state{

				game_servers = GameServers,

				vector_clock_game_servers = time_update(GameServerPid, State#state.id, State#state.vector_clock_game_servers)

			},

			io:format("~p knows new game server: ~p~n", [State#state.id, GameServerPid]),

			{noreply, NewState}

	end;


handle_cast({get_time, GameName}, State) ->
	VectorClockGames = State#state.vector_clock_games,
	Id = State#state.id,
	%CurrentTime = dict:fetch(self(),VectorClock),
	CurrentTime = time_get(GameName, Id, VectorClockGames),
	io:format("Unexpected message: ~p~n", [CurrentTime]),
	{noreply, State};


handle_cast({gossip_games, {FromGames, FromInitServerName, FromVectorClockGames}}, State) ->

	VectorClockGames = State#state.vector_clock_games,
	LocalGames = State#state.games,
	Id = State#state.id,

	{RecentGames, RecentVectorClockGames} = resolve_list(game, LocalGames, VectorClockGames, FromGames, FromVectorClockGames, FromInitServerName),
	NewState = State#state{games = RecentGames, vector_clock_games = RecentVectorClockGames},

	io:format("~p updated games from ~p~nlist: ~p~n", [Id, FromInitServerName, dict:fetch_keys(RecentGames)]),

	{noreply, NewState};

handle_cast({gossip_game_servers, {FromGameServers, FromInitServerName, FromVectorClockGameServers}}, State) ->

	VectorClockGames = State#state.vector_clock_game_servers,
	LocalGames = State#state.game_servers,
	Id = State#state.id,

	{RecentGames, RecentVectorClockGames} = resolve_list(game_server, LocalGames, VectorClockGames, FromGameServers, FromVectorClockGameServers, FromInitServerName),
	NewState = State#state{game_servers = RecentGames, vector_clock_game_servers = RecentVectorClockGames},

	io:format("~p updated game servers from ~p~nlist: ~p~n", [Id, FromInitServerName, dict:fetch_keys(RecentGames)]),

	{noreply, NewState};


handle_cast({game_finished, Name}, State) ->

	NewState = State#state{
		games = dict:update(Name, fun(G) -> G#game{finished = time()} end, State#state.games),

		vector_clock_games = time_update(Name, State#state.id, State#state.vector_clock_games)
	},

	{noreply, NewState};


handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info({'DOWN', _, process, _, _Reason}, _State) ->
	exit(normal);


handle_info(trigger, State) ->

	Games = State#state.games,
	GameServers = State#state.game_servers,
	Id = State#state.id,
	VectorClockGames = State#state.vector_clock_games,
	VectorClockGameServers = State#state.vector_clock_game_servers,
	InitServers = State#state.init_servers,

	{ToPid, MsgGames} = create_gossip_message(Id, Games, VectorClockGames, InitServers),
	{ToPid2, MsgGameServers} = create_gossip_message(Id, GameServers, VectorClockGameServers, InitServers),

	?MODULE:gossip_games(ToPid, MsgGames),
	?MODULE:gossip_game_servers(ToPid2, MsgGameServers),

	NewGames = erase_old_elements(game, Games, dict:fetch_keys(Games)),
	NewGameServers = erase_old_elements(game_server, GameServers, dict:fetch_keys(GameServers)),

	erlang:send_after(?DELAY, self(), trigger),

	NewState = State#state{
		% add new game to the list
		games = NewGames,
		game_servers = NewGameServers
	},

	{noreply, NewState};


handle_info(Info, State) ->
	io:format("Unexpected message: ~p~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	io:format("Init server ~p stopped~n", [self()]),
	ok.

code_change(_Old, State, _Extra) -> {ok, State}.

%%
% Updates vector clock of specified game (increments value of element of specified init_server)
%
time_update(GameName, InitServerName, VectorClockList) ->
	case dict:is_key(GameName, VectorClockList) of
		true -> VectorClock = dict:fetch(GameName, VectorClockList);
		false -> VectorClock = dict:new()
	end,
	case dict:is_key(InitServerName, VectorClock) of
		true -> CurrentTime = dict:fetch(InitServerName, VectorClock);
		false -> CurrentTime = 0
	end,
	NewVectorClock = dict:store(InitServerName,CurrentTime+1, VectorClock),
	NewVectorClockList = dict:store(GameName, NewVectorClock, VectorClockList),
	NewVectorClockList.

%%
% Sets vector clock of specified game (sets value of element of specified init_server)
%
time_set(GameName, InitServerName, VectorClockList, Value) ->
	case dict:is_key(GameName, VectorClockList) of
		true -> VectorClock = dict:fetch(GameName, VectorClockList);
		false -> VectorClock = dict:new()
	end,
	NewVectorClock = dict:store(InitServerName,Value, VectorClock),
	NewVectorClockList = dict:store(GameName, NewVectorClock, VectorClockList),
	NewVectorClockList.

%%
% Gets time of specified game and init_server
%
time_get(GameName, InitServerName, VectorClockList) ->
	case dict:is_key(GameName, VectorClockList) of
		true ->
			VectorClock = dict:fetch(GameName, VectorClockList),
			case dict:is_key(InitServerName, VectorClock) of
				true ->
					dict:fetch(InitServerName,VectorClock);
				false ->
					0
			end;
		false ->
			0
	end.


%%
% Merges two lists (local and remote) with respect to the vector clocks
%
resolve_list(Type, Games, VectorClockList, FromGames, FromVectorClockList, InitServerName) ->
	{NewGames, NewVectorClockList} = resolve_list_from(Type, Games, VectorClockList, dict:to_list(FromGames), FromVectorClockList, InitServerName),
	{NewGames, NewVectorClockList}.

resolve_list_from(_, Games, VectorClockList, [], _, _) ->
	{Games, VectorClockList};

resolve_list_from(Type, Games, VectorClockList, [H|T], FromVectorClockList, InitServerName) ->
	{Name, Value} = H,

	case Type of
		game ->
			Finished = Value#game.finished;
		game_server ->
			Finished = Value#game_server.finished
	end,

	% checks if game in remote init_server exists in local init_server
	case dict:is_key(Name, Games) of

		true ->
			TimeLocal = time_get(Name, InitServerName, VectorClockList),
			TimeFrom = time_get(Name, InitServerName, FromVectorClockList),

			% checks if local time for specified  init_server is higher than remote time
			case TimeLocal >= TimeFrom of

				true ->
					% keeps local values
					resolve_list_from(Type, Games, VectorClockList, T, FromVectorClockList, InitServerName);

				false ->
					% uses value and time of remote init_server
					NewVectorClockList = time_set(Name, InitServerName, VectorClockList, InitServerName),
					resolve_list_from(Type, dict:store(Name, Value, Games), NewVectorClockList, T, FromVectorClockList, InitServerName)
			end;

		false ->
			case Finished == false of
				false ->
					resolve_list_from(Type, Games, VectorClockList, T, FromVectorClockList, InitServerName);
				true ->
					% uses value and time of remote init_server
					NewVectorClockList = time_set(Name, InitServerName, VectorClockList, InitServerName),
					resolve_list_from(Type, dict:store(Name, Value, Games), NewVectorClockList, T, FromVectorClockList, InitServerName)
			end
	end.


erase_old_elements(_, Dictionary, []) ->
	Dictionary;
erase_old_elements(Type, Dictionary, [H|T]) ->
	Element = dict:fetch(H, Dictionary),
	case Type of
		game ->
			Finished = Element#game.finished;
		game_server ->
			Finished = Element#game_server.finished
	end,
	case (Finished /= false andalso calendar:time_to_seconds(time()) - calendar:time_to_seconds(Finished) > 30) of
		true ->
			erase_old_elements(Type, dict:erase(H, Dictionary), T);
		false ->
			erase_old_elements(Type, Dictionary, T)
	end.


%%
% Creates messages to be used for gossiping
%
create_gossip_message(FromId, List, VectorClockList, InitServers) ->

	ToId = random:uniform(length(InitServers)),
	ToPid = lists:nth(ToId, InitServers),
	Msg = {List, FromId, VectorClockList},

	{ToPid, Msg}.


%%
% Throws error if game with same name already exists.
%
ensure_game_not_exists(Name, State) ->
	case dict:is_key(Name, State#state.games) of
		true -> throw(game_already_exists);
		false -> ok
	end.


initialize_games(Name, Cards, State) ->

	{MainPid, BackupPid, NewGameServers, NewVectorClock} = create_games(State#state.game_servers, Name, Cards, State#state.vector_clock_game_servers, State#state.id),

	% TODO this line will timeout if either game or backup are not created
	pexeso_game:set_main(MainPid, BackupPid),

	% return updated state
	State#state{

		% add new game to the list
		games = dict:store(
			Name,
			#game{name = Name, game_pid = MainPid, backup_pid = BackupPid},
			State#state.games
		),

		% add updated servers to the list of other servers
		game_servers = NewGameServers,
		vector_clock_game_servers = NewVectorClock,

		% new vector clock
		vector_clock_games = time_update(Name, State#state.id, State#state.vector_clock_games)
	}.


create_games(GameServers, Name, Cards, VectorClock, InitServer) ->
	create_games(shuffle_game_servers(GameServers), GameServers, Name, Cards, VectorClock, InitServer, null).


create_game(GameServers, Name, Cards, VectorClock, InitServer, Skip) ->

	% game must be on a server different from Skip
	PossibleServers = dict:erase(Skip, GameServers),

	create_games(shuffle_game_servers(PossibleServers), GameServers, Name, Cards, VectorClock, InitServer, just_one).


create_games([ Server | Rest ], GameServers, Name, Cards, VectorClock, InitServer, First) ->

	try

		% try to create game
		Pid = game_server:create_game(Server, Name, Cards),

		case First of

			% only first game was created, remember it and continue
			null -> create_games(Rest, GameServers, Name, Cards, VectorClock, InitServer, Pid);

			% second game was created, we can return
			_ -> { First, Pid, GameServers, VectorClock }

		end

	catch

		% game server does not exist, or timeouted
		exit:{Reason, _} when Reason == timeout; Reason == noproc ->

			% recusively find another one
			create_games(
				Rest, dict:store(Server, #game_server{finished = time()}, GameServers),
				Name, Cards, time_update(Server, InitServer, VectorClock), InitServer, First
			)

	end;


create_games([], _, _, _, _, _, _) ->
	throw(not_enough_game_servers).


shuffle_game_servers(Servers) ->
	RandomList = [{random:uniform(), X} || X <- dict:fetch_keys(Servers)],
    [X || {_, X} <- lists:sort(RandomList)].


replace_game(Down, Game, MainPid, BackupPid, State) ->

	% create new game
	{just_one, NewPid, GameServers, VectorClock} = create_game(State#state.game_servers, Game#game.name, [], State#state.vector_clock_game_servers, State#state.id, 0),

	NewGame = replace_down_process(Down, Game, MainPid, BackupPid, NewPid),

	io:format("~p replacing game info: ~p -> ~p~n", [self(), Game, NewGame]),

	NewState = State#state{

		games = dict:store(NewGame#game.name, NewGame, State#state.games),
		game_servers = GameServers,
		vector_clock_game_servers = VectorClock
	},

	{NewPid, NewState}.


replace_down_process(main_down, Game, _, BackupPid, NewPid) ->
	Game#game{
		game_pid = BackupPid,
		backup_pid = NewPid
	};

replace_down_process(backup_down, Game, MainPid, _, NewPid) ->
	Game#game{
		game_pid = MainPid,
		backup_pid = NewPid
	}.


is_game_running(Down, MainPid, BackupPid) ->

	Pid = case Down of
		main_down -> MainPid;
		backup_down -> BackupPid
	end,

	try
		pexeso_game:get_stats(Pid, 500), true
	catch
		exit:{Reason, _} when Reason == timeout; Reason == noproc -> false
	end.
