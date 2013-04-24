-module(pexeso_progress_listener).

-export([
	behaviour_info/1
]).

behaviour_info(callbacks) ->
	[{pexeso_progress, 2}];

behaviour_info(_) -> undefined.