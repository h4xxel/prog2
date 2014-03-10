-module(dinner).
-export([start/0]).

start() ->
	spawn(fun() -> init() end).
	
init() ->
	random:seed(now()),
	C1 = chopstick:start(),
	C2 = chopstick:start(),
	C3 = chopstick:start(),
	C4 = chopstick:start(),
	C5 = chopstick:start(),
	Ctrl = self(),
	philosopher:start(5, C1, C2, "Arne", Ctrl),
	philosopher:start(5, C2, C3, "Goesta", Ctrl),
	philosopher:start(5, C3, C4, "Berit", Ctrl),
	philosopher:start(5, C4, C5, "Otto", Ctrl),
	philosopher:start(5, C5, C1, "Sverker", Ctrl),
	wait(5, [C1, C2, C3, C4, C5]).

wait(0, Chopsticks) ->
	lists:foreach(fun(C) -> chopstick:quit(C) end, Chopsticks),
	done;
wait(N, Chopsticks) ->
	io:format("Waiting~n"),
	receive
		done ->
			wait(N - 1, Chopsticks);
		abort ->
			exit(abort)
	end.
