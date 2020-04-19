var Games = (function()
{
	var connection = null;
	var updatedCallback = function(){};

	var initialize = function(callback)
	{
		if (connection != null) return;

		connection = new WebSocket('ws://192.168.137.1:8001/websocket/games', 'games');

	    connection.onopen = function()
	    {
	        console.info('WebSocket is opened');
	        callback();
	    };

	    connection.onmessage = function(msg)
	    {
	        updatedCallback(JSON.parse(msg.data));
	    };

	    connection.onerror = function(event)
	    {
	    	console.error(event);
	    };
	};

	var updated = function(callback)
	{
		updatedCallback = callback;
	};

	var refresh = function()
	{
		connection.send(JSON.stringify( 
			{command: 'get_games', params: {}} 
		));
	};

	var create = function(name)
	{
		connection.send(JSON.stringify( 
			{command: 'create_game', params: { name: name }} 
		));
	};

	return {
		initialize: initialize,
		updated: updated,
		refresh: refresh,
		create: create
	};
}
)();