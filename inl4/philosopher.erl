-module(philosopher).
-export([start/5]).


start(Hunger, StickRight, StickLeft, Name, Control) ->
	spawn_link(fun() -> init(Hunger, StickRight, StickLeft, Name, Control) end).

init(Hunger, StickRight, StickLeft, Name, Control) ->
	random:seed(now()),
	thinking(Hunger, StickRight, StickLeft, Name, Control).

thinking(0, _, _, Name, Control) ->
	io:format("~s is done.~n", [Name]),
	Control ! done,
	done;
thinking(Hunger, StickRight, StickLeft, Name, Control) ->
	io:format("~s is thinking.~n", [Name]),
	sleep(500, 200),
	waiting(Hunger, StickRight, StickLeft, Name, Control).

waiting(Hunger, StickRight, StickLeft, Name, Control) ->
	io:format("~s is waiting for left chopstick.~n", [Name]),
	case chopstick:request(StickLeft) of
		no ->
			thinking(Hunger, StickRight, StickLeft, Name, Control);
		_ -> ok
	end,
	io:format("~s is waiting for right chopstick.~n", [Name]),
	case chopstick:request(StickRight) of
		no ->
			chopstick:return(StickLeft),
			thinking(Hunger, StickRight, StickLeft, Name, Control);
		_ -> ok
	end,
	eating(Hunger, StickRight, StickLeft, Name, Control).
	
eating(Hunger, StickRight, StickLeft, Name, Control) ->
	io:format("~s is eating, omnomnom!.~n", [Name]),
	sleep(500, 200),
	chopstick:return(StickLeft),
	chopstick:return(StickRight),
	thinking(Hunger - 1, StickRight, StickLeft, Name, Control).

sleep(T, D) ->
	timer:sleep(T ).%+ random:uniform(D)).
