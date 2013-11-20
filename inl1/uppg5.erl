-module(uppg5).
-export([nreverse/1]).
-export([reverse/1]).
-export([bench/0]).

nreverse([]) ->
	[];
nreverse([H|T]) ->
	nreverse(T) ++ [H].

reverse(L) ->
	reverse(L, []).

reverse([], R) ->
	R;
reverse([H|T], R) ->
	reverse(T, [H|R]).

bench() ->
	Ls = [16, 32, 64, 128, 256, 512],
	N = 100,
	Bench = fun(L) ->
		S = lists:seq(1,L),
		Tn = time(N, fun() -> nreverse(S) end),
		Tr = time(N, fun() -> reverse(S) end),
		io:format("length: ~10w nrev: ~8w us rev: ~8w us~n", [L, Tn, Tr])
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
