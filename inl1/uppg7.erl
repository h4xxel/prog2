-module(uppg7).
-export([fib/1, fib1/1, fibb/0]).

% Very naïve and stupid fibonnaci counter
fib(0) ->
	0;
fib(1) ->
	1;
fib(N) ->
	fib(N - 1) + fib(N - 2).

% A tad better
fib1(N) ->
	fib1(0, 1, N).

fib1(Fib1, _, 0) ->
	Fib1;
fib1(Fib1, Fib2, N) ->
	fib1(Fib2, Fib1 + Fib2, N - 1).


fibb() ->
	Ls = [8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40],
	N = 10,
	Bench = fun(L) ->
		T = time(N, fun() -> fib1(L*100) end),
		io:format("n: ~4w fib(n) calculated in: ~8w us~n", [L, T])
	end,
	lists:foreach(Bench, Ls).

time(N, F)->
	%% time in micro seconds
	T1 = now(),
	loop(N, F),
	T2 = now(),
	timer:now_diff(T2, T1).
	
loop(N, Fun) ->
	if N == 0 -> ok;
		true -> Fun(), 
		loop(N-1, Fun) 
	end.
