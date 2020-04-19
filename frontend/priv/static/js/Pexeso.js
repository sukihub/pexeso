var Command = (function()
{
	function Command(name, params)
	{
		if (typeof params === 'undefined')
		{
			var parsed = JSON.parse(name);

			this.name = parsed.command;
			this.params = parsed.params;
		}
		else
		{
			this.name = name;
			this.params = params;
		}
	}

	Command.prototype.build = function()
	{
		return JSON.stringify({ command: this.name, params: this.params });
	};

	Command.prototype.get = function(param)
	{
		return this.params[param];
	};

	Command.prototype.isActionJoin = function()
	{
		return this.name == 'action_join';
	};

	Command.join = function(game, player)
	{
		return new Command('join', { game: game, player: player });
	};

	Command.getPlayground = function()
	{
		return new Command('get_playground', {});
	};

	Command.getStats = function()
	{
		return new Command('get_stats', {});
	};

	Command.turnCard = function(i)
	{
		return new Command('turn_card', { card: i });
	};

	return Command;
}
)();

var Pexeso = (function()
{
	function Pexeso(game, player, callback)
	{
		this.onplaygroundupdate = 
		this.onstatsupdate = function(){};

		var self = this;
		var connection = new WebSocket('ws://192.168.137.1:8001/websocket/pexeso', 'pexeso');

		connection.onopen = function()
		{
			self.join(game, player, callback);
		};

		//this.connection.onmessage = function(msg)
	    //{
	    //   updatedCallback(JSON.parse(msg.data));
	    //};

	    connection.onerror = function(event)
	    {
	    	console.error(event);
	    };

	    this.connection = connection;
	}

	Pexeso.prototype.join = function(game, player, callback)
	{
		var self = this;

		this.connection.onmessage = function(msg)
		{
			var command = new Command(msg.data);

			if (!command.isActionJoin() || command.get('player') != player) return;

			self.connection.onmessage = function(msg) { self.receivedGameplayMessage(msg); };
			self.getPlayground();
			self.getStats();

			callback();
		};


		this.connection.send(Command.join(game, player).build());
	};

	Pexeso.prototype.getPlayground = function()
	{
		this.connection.send(Command.getPlayground().build());
	};

	Pexeso.prototype.getStats = function()
	{
		this.connection.send(Command.getStats().build());
	};

	Pexeso.prototype.turnCard = function(i)
	{
		this.connection.send(Command.turnCard(i).build());
	};

	Pexeso.prototype.receivedGameplayMessage = function(msg)
	{
		var command = new Command(msg.data);

		var reaction = this.actionResponses[command.name];
		if (typeof reaction !== 'undefined') reaction.call(this, command.params);
	};

	Pexeso.prototype.actionResponses = {
		
		action_playground: function(params)
		{
			params.playground.sort();
			this.onplaygroundupdate(params.playground);
		},

		action_stats: function(params)
		{
			this.onstatsupdate(params.stats);
		},

		action_turn: function(params)
		{
			this.onturn(params.player, params.card_id, params.content);
		},

		action_close: function(params)
		{
			this.onclose(params.player, params.card_id, params.content);
		},

		action_pick: function(params)
		{
			this.onpick(params.player, params.card_id_1, params.content_1, params.card_id_2, params.content_2);
		},

		action_fail: function(params)
		{
			this.onfail(params.player, params.card_id_1, params.content_1, params.card_id_2, params.content_2);
		},

		action_join: function(params)
		{
			//this.onjoin(params.player);
			this.getStats();
		},

		action_stop: function(params)
		{
			this.connection.close();
			this.onstop();
		}
	};

	return Pexeso;
}
)();