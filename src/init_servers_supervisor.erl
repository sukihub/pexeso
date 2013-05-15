-module(init_servers_supervisor).
-behaviour(supervisor).
 
-export([start_link/1]).
-export([init/1]).
 
start_link(Count) ->
	supervisor:start_link({local,?MODULE}, ?MODULE, [Count]),
	start_children(Count) .
	
start_children(0) ->
	ok;
start_children(Count) ->
	start_children(Count,Count).
start_children(0,_) ->
	ok;
start_children(Id,Count) ->
	{ok, Pid} = supervisor:start_child(?MODULE,[Id,Count]),
	register(list_to_atom("init_server"++lists:flatten(io_lib:format("~p", [Id]))),Pid),
	start_children(Id-1,Count).
	
init([Count]) ->
	{ok, {{simple_one_for_one, 3, 60},
		[{init_server_1,
		{init_server, start, []},
		permanent, 1000, worker, [init_server]}
	]}}.