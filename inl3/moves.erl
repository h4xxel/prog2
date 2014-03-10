-module(moves).
-export([move/2]).

%Snyggkod™
single({one, N}, {Main, One, Two}) ->
	if N < 0 ->
		{list:append(Main, list:take(One, -N)), list:drop(One, -N), Two};
	N > 0 ->
		{list:take(Main, length(Main) - N), list:append(list:drop(Main, length(Main) - N), One), Two};
	true ->
		{Main, One, Two}
	end;
single({two, N}, {Main, One, Two}) ->
	if N < 0 ->
		{list:append(Main, list:take(Two, -N)), One, list:drop(Two, -N)};
	N > 0 ->
		{list:take(Main, length(Main) - N), One, list:append(list:drop(Main, length(Main) - N), Two)};
	true ->
		{Main, One, Two}
	end.

move([], State) ->
	[State];
move([Move | Moves], State) ->
	[State | move(Moves, single(Move, State))].


