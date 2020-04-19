-module(pweb_greeting_controller, [Req]).
-compile(export_all).
 
hello('GET', []) ->
    {output, "<strong>Greetings fellow erlangers!</strong>"}.