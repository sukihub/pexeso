-module(game_server_supervisor).
-behavior(supervisor).

-export([
	start_link/2,
	new_servers/1,

	init/1
]).

start_link(NumberOfServers, InitServers) ->
	Result = supervisor:start_link({local, ?MODULE}, ?MODULE, InitServers),
	new_servers(NumberOfServers),
	Result.

new_servers(NumberOfServers) ->
	new_servers(NumberOfServers, 0).

new_servers(NumberOfServers, Current) when Current < NumberOfServers ->
	supervisor:start_child(?MODULE, []),
	new_servers(NumberOfServers, Current+1);

new_servers(NumberOfServers, Current) when Current >= NumberOfServers ->
	ok.

init(InitServers) ->

	{ok, {{simple_one_for_one, 10, 60}, [

		{ game_server, 
			{ game_server, start_link, [InitServers] }, 
			permanent, 1000, worker, [ game_server ] }

	] }}.