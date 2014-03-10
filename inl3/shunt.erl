-module(shunt).
-compile(export_all).

split(Train, Wagon) ->
	Pos = list:position(Train, Wagon),
	{list:take(Train, Pos - 1), list:drop(Train, Pos)}.

find([], []) ->
	[];
find(Train1, [H2 | T2]) ->
	{H1, T1} = split(Train1, H2),
	H1Len = length(H1),
	T1Len = length(T1),
	[
		{one, T1Len + 1},
		{two, H1Len},
		{one, -(T1Len + 1)},
		{two, -H1Len}
	] ++ find(T1 ++ H1, T2).

few([], []) ->
	[];
few([H2 | T1], [H2 | T2]) ->
	few(T1, T2);
few(Train1, [H2 | T2]) ->
	{H1, T1} = split(Train1, H2),
	H1Len = length(H1),
	T1Len = length(T1),
	[
		{one, T1Len + 1},
		{two, H1Len},
		{one, -(T1Len + 1)},
		{two, -H1Len}
	] ++ few(T1 ++ H1, T2).

randomize(List) ->
	randomize(List, []).

randomize([], Acc) ->
	Acc;
randomize(List, Acc) ->
	Item = lists:nth(random:uniform(length(List)), List),
	randomize(lists:delete(Item, List), [Item | Acc]).

test(N) ->
	Cars = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z],
	Train = list:take(Cars, N),
	Finished = randomize(Train),
	{ResultFind, _, _} = lists:last(moves:move(find(Train, Finished), {Train, [], []})),
	{ResultFew, _, _} = lists:last(moves:move(few(Train, Finished), {Train, [], []})),
	io:format("Train: ~w Goal: ~w  Find: ~w Few: ~w~n", [Train, Finished, ResultFind, ResultFew]),
	ResultFind = Finished,
	ResultFew = Finished,
	ok.
	
