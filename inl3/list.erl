-module(list).
-export([take/2, drop/2, append/2, member/2, position/2]).

take(_, 0) ->
	[];
take([H | T], N) ->
	[H | take(T, N - 1)].
	
drop(List, 0) ->
	List;
drop([_ | T], N) ->
	drop(T, N - 1).

append(List1, List2) ->
	List1 ++ List2.

member([], _) ->
	false;
member([E | _], E) ->
	true;
member([_ | T], E) ->
	member(T, E).

position([E | _], E) ->
	1;
position([_ | T], E) ->
	1 + position(T, E).
