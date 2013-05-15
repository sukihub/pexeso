-module(pexeso_supervisor).
-behavior(supervisor).

-export([
	start_link/0,
	init/1
]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, nothing).

init(_) ->

	InitServers = get_init_servers(),

	{ok, {{one_for_one, 10, 60}, [

		{ init_servers,
			{ init_server_supervisor, start_link, [InitServers] },
			permanent, 5000, supervisor, [ init_servers_supervisor ] },

		{ game_servers,
			{ game_server_supervisor, start_link, [4, InitServers] },
			permanent, 5000, supervisor, [ game_server_supervisor ] }

	]}}.

get_init_servers() ->
	[ init1, init2, init3 ].