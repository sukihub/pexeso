-record(action, {
	player,
	move
}).

-record(move, {
	type,
	card_a = null, 
	card_b = null
}).

-record(card, {
	id, 
	content
}).