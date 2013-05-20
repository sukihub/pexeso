-module(init_server).
-behaviour(gen_server).
-define(DELAY, 10000).

-export([
	start_link/2,
	gossip_games/2,
	create_game/2,
	get_games/0,
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

%-record(game_server, {
%	pid,
%	games_count = 0
%}).

% public api

start_link(Name, OthersNames) ->
	gen_server:start_link({local, Name}, ?MODULE, {Name, OthersNames}, []).

create_game(Name, Cards) ->
	gen_server:cast(pexeso_supervisor:pick_init_server(), {create_game, Name, Cards}).

get_games() ->
	gen_server:call(pexeso_supervisor:pick_init_server(), get_games).

get_game(Name) ->
	try_all(pexeso_supervisor:shuffle_init_servers(), {get_game, Name}).

register_game_server(GameServerPid) ->
	gen_server:cast(pexeso_supervisor:pick_init_server(), {register_game_server, GameServerPid}).

get_time(Pid, GameName) ->
	gen_server:cast(Pid, {get_time, GameName}).

game_finished(Name) ->
	gen_server:cast(pexeso_supervisor:pick_init_server(), {game_finished, Name}).

gossip_games(Pid, Msg) ->
	gen_server:cast(Pid, {gossip_games, Msg}).

main_down(Name, Main, Backup) ->
	try_all(pexeso_supervisor:shuffle_init_servers(), {main_down, Name, Main, Backup}).

backup_down(Name, Main, Backup) ->
	try_all(pexeso_supervisor:shuffle_init_servers(), {backup_down, Name, Main, Backup}).

try_all([ InitServer | InitServers ], Message) ->
	case gen_server:call(InitServer, Message) of
		try_next -> try_all(InitServers, Message);
		Else -> Else
	end;

try_all([], _Message) ->
	throw(not_found_on_any_init).

stop(Pid) ->
	gen_server:cast(Pid, stop).

% gen server stuff

%%
% Init server initialization.
%
init({Name, OthersNames}) ->

	io:format("Init server ~p ~p started~n", [Name, self()]),

	random:seed(now()),

	VectorClock = dict:new(),

	State = #state{
		games = dict:new(),
		game_servers = sets:new(),
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
	Result = [ {G#game.name, G#game.game_pid, G#game.backup_pid, G#game.finished} || {_, G} <- dict:to_list(State#state.games) ],
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

	case sets:is_element(GameServerPid, State#state.game_servers) of

		true ->
			{noreply, State};

		false ->
			GameServers = sets:add_element(GameServerPid, State#state.game_servers),
			NewState = State#state{game_servers = GameServers},

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

	{RecentGames, RecentVectorClockGames} = resolve_lists_games(LocalGames, VectorClockGames, FromGames, FromVectorClockGames, FromInitServerName),
	NewState = State#state{games = RecentGames, vector_clock_games = RecentVectorClockGames},

	io:format("~p updated games from ~p~nlist: ~p~n", [Id, FromInitServerName, dict:fetch_keys(RecentGames)]),

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
	Id = State#state.id,
	VectorClockGames = State#state.vector_clock_games,
	InitServers = State#state.init_servers,

	{ToPid, Msg} = create_gossip_message(Id, Games, VectorClockGames, InitServers),

	?MODULE:gossip_games(ToPid, Msg),

	NewGames = erase_old_games(Games, dict:fetch_keys(Games)),

	erlang:send_after(?DELAY, self(), trigger),

	NewState = State#state{
		% add new game to the list
		games = NewGames
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
resolve_lists_games(Games, VectorClockList, FromGames, FromVectorClockList, InitServerName) ->
	{NewGames, NewVectorClockList} = resolve_lists_from(Games, VectorClockList, dict:to_list(FromGames), FromVectorClockList, InitServerName),
	{NewGames, NewVectorClockList}.

resolve_lists_from(Games, VectorClockList, [], _, _) ->
	{Games, VectorClockList};

resolve_lists_from(Games, VectorClockList, [H|T], FromVectorClockList, InitServerName) ->
	{Key, Value} = H,
	Name = Key,

	% checks if game in remote init_server exists in local init_server
	case dict:is_key(Name, Games) of

		true ->
			TimeLocal = time_get(Name, InitServerName, VectorClockList),
			TimeFrom = time_get(Name, InitServerName, FromVectorClockList),

			% checks if local time for specified  init_server is higher than remote time
			case TimeLocal >= TimeFrom of

				true ->
					% keeps local values
					resolve_lists_from(Games, VectorClockList, T, FromVectorClockList, InitServerName);

				false ->
					% uses value and time of remote init_server
					NewVectorClockList = time_set(Name, InitServerName, VectorClockList, InitServerName),
					resolve_lists_from(dict:store(Name, Value, Games), NewVectorClockList, T, FromVectorClockList, InitServerName)
			end;

		false ->
			case Value#game.finished == false of
				false ->
					resolve_lists_from(Games, VectorClockList, T, FromVectorClockList, InitServerName);
				true ->
					% uses value and time of remote init_server
					NewVectorClockList = time_set(Name, InitServerName, VectorClockList, InitServerName),
					resolve_lists_from(dict:store(Name, Value, Games), NewVectorClockList, T, FromVectorClockList, InitServerName)
			end
	end.


erase_old_games(Dictionary, []) ->
	Dictionary;
erase_old_games(Dictionary, [H|T]) ->
	Game =  dict:fetch(H, Dictionary),
	case (Game#game.finished /= false andalso calendar:time_to_seconds(time()) - calendar:time_to_seconds(Game#game.finished) > 30) of
		true ->
			erase_old_games(dict:erase(H, Dictionary), T);
		false ->
			erase_old_games(Dictionary, T)
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

	{MainPid, BackupPid, NewGameServers} = create_games(State#state.game_servers, Name, Cards),

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

		% new vector clock
		vector_clock_games = time_update(Name, State#state.id, State#state.vector_clock_games)
	}.


create_games(GameServers, Name, Cards) ->
	create_games(shuffle_game_servers(GameServers), GameServers, Name, Cards, null).


create_game(GameServers, Name, Cards, Skip) ->

	% game must be on a server different from Skip
	PossibleServers = sets:del_element(Skip, GameServers),

	create_games(shuffle_game_servers(PossibleServers), GameServers, Name, Cards, just_one).


create_games([ Server | Rest ], GameServers, Name, Cards, First) ->

	try

		% try to create game
		Pid = game_server:create_game(Server, Name, Cards),

		case First of

			% only first game was created, remember it and continue
			null -> create_games(Rest, GameServers, Name, Cards, Pid);

			% second game was created, we can return
			_ -> { First, Pid, GameServers }

		end

	catch

		% game server does not exist, or timeouted
		exit:{Reason, _} when Reason == timeout; Reason == noproc ->

			% recusively find another one
			create_games(
				Rest, sets:del_element(Server, GameServers),
				Name, Cards, First
			)

	end;


create_games([], _, _, _, _) ->
	throw(not_enough_game_servers).


shuffle_game_servers(Servers) ->
	RandomList = [{random:uniform(), X} || X <- sets:to_list(Servers)],
    [X || {_, X} <- lists:sort(RandomList)].


replace_game(Down, Game, MainPid, BackupPid, State) ->

	% create new game
	{just_one, NewPid, GameServers} = create_game(State#state.game_servers, Game#game.name, [], 0),

	NewGame = replace_down_process(Down, Game, MainPid, BackupPid, NewPid),

	io:format("~p replacing game info: ~p -> ~p~n", [self(), Game, NewGame]),

	NewState = State#state{
		games = dict:store(NewGame#game.name, NewGame, State#state.games),
		game_servers = GameServers
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
