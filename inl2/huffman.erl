-module(huffman).
-export([test/0, table/1, encode/2, decode/2]).
-include("lipsum.hrl").

sample() ->
	%"the quick brown fox jumps over the lazy dog this is a sample text that we will use when we build up a table we will only handle lower case letters and no punctuation symbols the frequency will of course not represent english but it is probably not that far off".
	?LIPSUM.

text() ->
	%"this is somthing that we should encode".
	?LIPSUM.

freq_increase_or_add(Key, null) ->
	{node, Key, 1, null, null};
freq_increase_or_add(Key, {node, K, V, Left, Right}) ->
	if Key > K ->
		{node, K, V, freq_increase_or_add(Key, Left), Right};
	Key < K ->
		{node, K, V, Left, freq_increase_or_add(Key, Right)};
	true ->
		{node, K, V + 1, Left, Right}
	end.

freq_insert(Node, null) ->
	Node;
freq_insert(Node, {node, K, V, L, R}) ->
	{node, _, Value, _, _} = Node,
	if Value > V ->
		{node, K, V, L, freq_insert(Node, R)};
	true ->
		{node, K, V, freq_insert(Node, L), R}
	end.

freq_resort(Freq) ->
	freq_resort(Freq, null).

freq_resort(null, Tree) ->
	Tree;
freq_resort({node, K, V, Left, Right}, Tree) ->
	T1 = freq_resort(Left, Tree),
	T2 = freq_insert({node, K, V, null, null}, T1),
	freq_resort(Right, T2).

freq_pop_lowest(null) ->
	null;
freq_pop_lowest({node, K, V, null, R}) ->
	{freq, K, V, R};
freq_pop_lowest({node, K, V, L, R}) ->
	{freq, Key, Value, Tree} = freq_pop_lowest(L),
	{freq, Key, Value, {node, K, V, Tree, R}}.

freq(Text) ->
	freq(Text, null).

freq([], Tree) ->
	Tree;
freq([H|T], Tree) ->
	freq(T, freq_increase_or_add(H, Tree)).

huffman({node, K, V, null, null}) ->
	{fulhakk, K, V};
huffman(Freq) ->
	{freq, K1, V1, F1} = freq_pop_lowest(Freq),
	{freq, K2, V2, F2} = freq_pop_lowest(F1),
	huffman(freq_insert({node, {{fulhakk, K1, V1}, {fulhakk, K2, V2}}, V1 + V2, null, null}, F2)).

codes(Huffman) ->
	codes(Huffman, []).

codes({fulhakk, {Left, Right}, _}, Bits) ->
	codes(Left, Bits ++ [0]) ++ codes(Right, Bits ++ [1]);
codes({fulhakk, Key, _}, Bits) ->
	[{code, Key, Bits}].

table(Sample) ->
	Freq = freq(Sample),
	Sorted = freq_resort(Freq),
	Tree = huffman(Sorted),
	{codes(Tree), Tree}.

encode(Text, Table) ->
	lists:flatmap(fun(Char) ->
		{value, {code, _, Bin}} = lists:keysearch(Char, 2, Table),
		Bin
	end, Text).

decode(Seq, Tree) ->
	decode(Seq, Tree, Tree).

decode([], _, {fulhakk, {_, _}, _}) ->
	[];
decode([], _, {fulhakk, Key, _}) ->
	[Key];
decode([H | Seq], Tree, {fulhakk, {Left, Right}, _}) ->
	case H of
		0 -> decode(Seq, Tree, Left);
		1 -> decode(Seq, Tree, Right)
	end;
decode(Seq, Tree, {fulhakk, Key, _}) ->
	[Key | decode(Seq, Tree, Tree)].

test() ->
	Sample = sample(),
	{timer, TimeTable, {Table, Tree}} = time(fun() -> table(Sample) end),
	Text = text(),
	{timer, TimeEnc, Seq} = time(fun() -> encode(Text, Table) end),
	{timer, TimeDec, Out} = time(fun() -> decode(Seq, Tree) end),
	io:format("built table ~w us~nencoded to ~w bits ~w us~nraw ~w bits, decoded ~w us~n", [TimeTable, length(Seq), TimeEnc, length(Text)*8, TimeDec]),
	%Out.

time(Fun)->
	%% time in micro seconds
	T1 = now(),
	Res = Fun(),
	T2 = now(),
	{timer, timer:now_diff(T2, T1), Res}.