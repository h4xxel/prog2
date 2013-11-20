-module(uppg6).
-export([unpack1/1]).
-export([unpack2/1]).

unpack1(Bin) ->
	unpack1(Bin, []).

unpack1(0, []) ->
	[0];
unpack1(0, Acc) ->
	Acc;
unpack1(Bin, Acc) ->
	unpack1(Bin bsr 1, [Bin band 1 | Acc]).


unpack2(Bin) ->
	lists:map(fun(C) -> C - 48 end, integer_to_list(Bin, 2)).
