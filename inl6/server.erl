-module(server).
-export([start/0]).
-compile(export_all).

-include("settings.hrl").

start() ->
	spawn(fun() -> init() end).

init() ->
	{ok, Listen} = gen_tcp:listen(?PORT, ?OPT),
	?LOG("~w~n", [Listen]),
	spawn_server(50,Listen),
	wait(Listen).

wait(Listen) ->
	receive
		stop ->
			gen_tcp:close(Listen),
			ok
	end.

spawn_server(0, _) ->
	ok;
spawn_server(N, Listen) ->
	spawn_link(fun() -> listener(Listen) end),
	spawn_server(N - 1, Listen).

listener(Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Socket} ->
			?LOG("server: connect~n", []),
			handle(Socket),
			listener(Listen);
		_ ->
			ok
	end.

generate_url([], Filename) ->
	"<a href=\"mp3/" ++ Filename ++ "\">" ++ Filename ++ "</a>";
generate_url(Title, Filename) ->
	"<a href=\"mp3/" ++ Filename ++ "\">" ++ Title ++ "</a>".

urldecode([]) ->
	[];
urldecode([$%, N1, N2 | T]) ->
	[list_to_integer([N1, N2], 16) | urldecode(T)];
urldecode([H | T]) ->
	[H | urldecode(T)].

list_directory() ->
	{ok, Dir} = file:list_dir("music"),
	Mp3s = lists:foldl(fun(E, Acc) ->
		case lists:reverse(E) of
			[$3, $p, $m, $. | _] ->
				Filename = E,
				Id3 = mp3:read_tags("music/" ++ urldecode(Filename)),
				Artist = mp3:id3_field(artist, Id3),
				Album = mp3:id3_field(album, Id3),
				Track = mp3:id3_field(track, Id3),
				Title = mp3:id3_field(title, Id3),
				["<tr>\n" ++
					"<td>" ++ Artist ++ "</td>\n" ++
					"<td>" ++ Album ++ "</td>\n" ++
					"<td>" ++ Track ++ "</td>\n" ++
					"<td>" ++ generate_url(Title, Filename) ++ "</td>\n" ++
				"</tr>\n" | Acc];
			_ ->
				Acc
		end
	end, [], Dir),
	List = lists:flatten(lists:sort(Mp3s)),
	Response = list_to_binary("<!DOCTYPE html>\n<html>\n<head>\n<title>Streaming server</title>\n</head>\n<body>\n" ++
		"<h1>Streaming server</h1>\n" ++
		"<table border=\"1\">\n<tr><th>Artist</th><th>Album</th><th>Track</th><th>Title</th></tr>\n" ++
		List ++
		"</table>\n</body>\n</html>\n"),
	{dir, byte_size(Response), Response}.

handle(Socket) ->
	case read_request(Socket) of
		{ok, Request, _} ->
			?LOG("server: received request ~p~n", [Request]),
			{get, Resource, Version, Headers} = Request,
			case Resource of
				"/" ->
					Meta = na,
					{dir, Size, Data} = list_directory(),
					Segments = [{seg, Data}],
					Response = icy:encode_response(Version, [
						{"Content-Type", "text/html; charset=utf-8"},
						{"Content-Length", integer_to_list(Size)}
					]);
				[$/, $m, $p, $3, $/ | File] ->
					case mp3:read_file("music/" ++ urldecode(File)) of
						{mp3, Title, Data} ->
							Segments = icy:segments(Data),
							case lists:keyfind("Icy-MetaData", 2, Headers) of
								{header, _, "1"} ->
									Meta = icy:encode_meta([{title, Title}]),
									Response = icy:encode_response(icy, [
										{"icy-metaint", integer_to_list(?CHUNK_SIZE)},
										{"Content-Type", "audio/mpeg"}
									]);
								false ->
									Meta = na,
									Response = icy:encode_response(icy, [
										{"Content-Type", "audio/mpeg"}
									])
							end;
						error ->
							Meta = na,
							Segments = [],
							Response = icy:encode_404([])
					end;
				_ ->
					Meta = na,
					Segments = [],
					Response = icy:encode_404([])
			end,
			?LOG("response: ~p~n", [Response]),
			gen_tcp:send(Socket, Response),
			loop(Meta, Segments, Socket, 0),
			gen_tcp:close(Socket);
		{error, Error} ->
			?ERROR("server: ~s~n", [Error])
	end.

loop(_, [], _, _) ->
	?LOG("server: done ~n", []),
	ok;
loop(na, [{seg, Segment}|Rest], Socket, N) ->
	gen_tcp:send(Socket, Segment),
	loop(na, Rest, Socket, N + 1);
loop(Meta, [{seg, Segment}|Rest], Socket, N) ->
	gen_tcp:send(Socket, Segment),
	%?LOG("server: sent segment ~w ~n", [N]),
	gen_tcp:send(Socket, Meta),
	%?LOG("response: ~p~n", [Meta]),
	%?LOG("server: sent header ~w ~n", [N]),
	loop(Meta, Rest, Socket, N + 1).

read_request(Socket) ->
	reader(fun(More) -> icy:parse_request(More) end, Socket).

reader(Parser, Socket) ->
	inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, More} ->
			case Parser(More) of
				{ok, Parsed, Rest} ->
					{ok, Parsed, Rest};
				{more, Cont} ->
					reader(Cont, Socket);
				{error, Error} ->
					{error, Error}
			end;
		{tcp_closed, Socket} ->
			{error, "server closed connection"};
		stop ->
			ok
	after ?TIMEOUT ->
		gen_tcp:close(Socket),
		{error, "timeout"}
	end.
