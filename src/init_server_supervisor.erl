-module(init_server_supervisor).
-behavior(supervisor).

-export([
	start_link/1,
	init/1
]).

start_link(Names) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Names).

init(Names) ->
	ChildSpecs = [ generate_childspec(Name, Names) || Name <- Names ],
	{ok, {{one_for_one, 10, 60}, ChildSpecs }}.


generate_childspec(Name, Names) ->
	{ Name, 
		{ init_server, start_link, [Name, Names] }, 
		permanent, 1000, worker, [ init_server ] }.