-module(chopstick).
-export([start/0, request/1, return/1, quit/1]).

start() ->
	spawn_link(fun() -> available() end).

request(Stick) ->
	Stick ! {request, self()},
	receive
		granted ->
			ok
		after 100 ->
			no
	end.

return(Stick) ->
	Stick ! return,
	ok.

quit(Stick) ->
	Stick ! quit,
	ok.

available() ->
	receive
		{request, Pid} ->
			Pid ! granted,
			gone();
		quit ->
			ok
	end.

gone() ->
	receive
		return ->
			available();
		quit ->
			ok
	end.
