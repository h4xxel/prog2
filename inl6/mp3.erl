-module(mp3).
-compile(export_all).

-include("settings.hrl").

id3_string(<<>>) ->
	[];
id3_string(<<0, _/binary>>) ->
	[];
id3_string(<<C, Rest/binary>>) ->
	[C | id3_string(Rest)].


id3_tag(S, Size) ->
	{ok, Data} = file:pread(S, Size-128, 128),
	case Data of
		<<"TAG", Title:(30*8)/bitstring, Artist:(30*8)/bitstring, Album:(30*8)/bitstring, Year:(4*8)/bitstring, Comment:(28*8)/bitstring, 0:8, Track:8/bitstring, Genre:8/bitstring>> ->
			{ok, {id3, [{title, Title}, {artist, Artist}, {album, Album}, {year, Year}, {comment, Comment}, {track, Track}, {genre, Genre}]}, Size-128};
		_ ->
			{ok, na, Size}
	end.

id3_field(_, na) ->
	"na";
id3_field(Field, {id3, Tags}) ->
	case lists:keyfind(Field, 1, Tags) of
		{year, Value} ->
			<<Year:(8*4)>> = Value,
			integer_to_list(Year);
		{track, Value} ->
			<<Track>> = Value,
			string:right(integer_to_list(Track), 2, $0);
		{genre, Value} ->
			<<Genre>> = Value,
			integer_to_list(Genre);
		{Field, Value} ->
			id3_string(Value);
		false ->
			"na"
	end.

read_tags(File) ->
	Size = filelib:file_size(File),
	{ok, S} = file:open(File, [read, binary, raw]),
	{ok, Id3, _} = id3_tag(S, Size),
	file:close(S),
	Id3.

read_file(File) ->
	case file:open(File, [read, binary, raw]) of
		{ok, S} ->
			Size = filelib:file_size(File),
			{ok, Id3, End} = id3_tag(S, Size),
			{ok, Data} = file:pread(S, 0, End),
			file:close(S),
			Title = id3_field(title, Id3),
			{mp3, Title, Data};
		_ ->
			error
	end.
