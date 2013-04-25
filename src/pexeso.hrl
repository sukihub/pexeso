-record(action, {
	player,
	move
}).

-record(turn, {
	card 
}).

-record(close, {
	card 
}).

-record(pick, {
	card_a,
	card_b
}).

-record(fail, {
	card_a,
	card_b
}).

-record(join, {
	pid
}).

-record(card, {
	id, 
	content
}).

-define(HEARTBEAT_DELAY, 10000).