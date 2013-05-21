-module(pexeso_supervisor).
-behavior(supervisor).

-export([
	start_link/0,
	pick_init_server/0,
	shuffle_init_servers/0,
	init/1
]).

start_link() ->
	random:seed(now()),
	supervisor:start_link({local, ?MODULE}, ?MODULE, nothing).

pick_init_server() ->
	InitServers = get_init_servers(),
	I = random:uniform(length(InitServers)),
	global:whereis_name(lists:nth(I, InitServers)).

init(_) ->

	InitServers = get_init_servers(),

	{ok, {{one_for_one, 10, 60}, [

		{ init_servers,
			{ init_server_supervisor, start_link, [InitServers] },
			permanent, 5000, supervisor, [ init_servers_supervisor ] },

		{ game_servers,
			{ game_server_supervisor, start_link, [4] },
			permanent, 5000, supervisor, [ game_server_supervisor ] }

	]}}.

get_init_servers() ->
	[ init1, init2, init3 ].

shuffle_init_servers() ->
    RandomList = [{random:uniform(), X} || X <- get_init_servers()],
    [X || {_, X} <- lists:sort(RandomList)].