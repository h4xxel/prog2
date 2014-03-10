-module(icy).
-compile(export_all).

-include("settings.hrl").

decode_resource(Segment) ->
	decode_resource(Segment, []).

decode_resource(<<>>, _) ->
	more;
decode_resource(<<32, Rest/binary>>, Resource) ->
	{ok, Resource, Rest};
decode_resource(<<C, Rest/binary>>, Resource) ->
	decode_resource(Rest, Resource ++ [C]).

decode_version(Segment) when size(Segment) < 10 ->
	more;
decode_version(<<"HTTP/1.0", 13, 10, Rest/binary>>) ->
	{ok, v10, Rest};
decode_version(<<"HTTP/1.1", 13, 10, Rest/binary>>) ->
	{ok, v11, Rest}.

header(<<>>, _) ->
	more;
header(<<13, 10, Rest/binary>>, Acc) ->
	{ok, Acc, Rest};
header(<<C, Rest/binary>>, Acc) ->
	header(Rest, Acc ++ [C]).

split_header_value([32 | Tail]) ->
	split_header_value(Tail);
split_header_value(Value) ->
	Value.

split_header([$: | Tail], Acc) ->
	{header, Acc, split_header_value(Tail)};
split_header([C | Tail], Acc) ->
	split_header(Tail, Acc ++ [C]).

headers(Segment, Acc) ->
	case header(Segment, []) of
		{ok, [], Rest} ->
			{ok, Acc, Rest};
		{ok, Header, Rest} ->
			headers(Rest, [split_header(Header, []) | Acc]);
		more ->
			more
	end.

request_line(Segment) when size(Segment) < 4 ->
	more;
request_line(<<$G, $E, $T, 32, R1/binary>>) ->
	case decode_resource(R1) of
		{ok, Resource, R2} ->
			case decode_version(R2) of
				{ok, Version, R3} ->
					{ok, get, Resource, Version, R3};
				more ->
					more;
				Other ->
					{error, "strange version: " ++ Other}
			end;
		more ->
			more;
		error ->
			{error, "failed to parse resource"}
		end;
request_line(Line) ->
	{error, "not a GET request: " ++ Line}.

parse_request(Segment) ->
	case request_line(Segment) of
		{ok, Method, Resource, Version, R1} ->
			case headers(R1, []) of
				{ok, Headers, Body} ->
					{ok, {Method, Resource, Version, Headers}, Body};
				more ->
					{more, fun(More) -> parse_request(<<Segment/binary, More/binary>>) end}
			end;
		{ok, Req, _} ->
			{error, "invalid request: " ++ Req};
		more ->
			{more, fun(More) -> parse_request(<<Segment/binary, More/binary>>) end}
	end.

padding(Encoded) ->
	Size = byte_size(Encoded),
	Pad = Size rem 16,
	K = Size div 16 + 1,
	{K, <<Encoded/binary, 0:(8*(16 - Pad))>>}.

encode_meta_info([]) ->
	<<>>;
encode_meta_info([Meta | Tail]) ->
	case Meta of
		{title, Title} ->
			%should escape quotes, I'm lazy
			Next = encode_meta_info(Tail),
			Escaped = lists:map(fun(C) ->
				case C of
					$' -> $`;
					C -> C
				end
			end, Title),
			EncodedTitle = list_to_binary(Escaped),
			<<"StreamTitle='", EncodedTitle/binary, "';", Next/binary>>;
		_ ->
			encode_meta_info(Tail)
	end.

encode_meta(Meta) ->
	Encoded = encode_meta_info(Meta),
	{K, Padded} = padding(Encoded),
	<<K/integer, Padded/binary>>.

segments(<<Chunk:(?CHUNK_SIZE)/binary, Rest/binary>>) ->
	[{seg, Chunk}| segments(Rest)];
segments(_) ->
	[].

encode_header([]) ->
	[13, 10];
encode_header([{Key, Value} | Tail]) ->
	[Key ++ ": " ++ Value ++ [13, 10] | encode_header(Tail)].

%~ encode_response_(Header) ->
	%~ Status = "ICY 200 OK\r\n",
	%~ MetaInt = "icy-metaint:" ++ integer_to_list(?CHUNK_SIZE) ++ "\r\n",
	%~ Reply = Status ++ MetaInt ++ encode_header(Header),
	%~ list_to_binary(Reply).

encode_404(Headers) ->
	list_to_binary("HTTP/1.1 404 Not Found\r\n" ++ encode_header(Headers)).
	
encode_response(icy, Headers) ->
	list_to_binary("ICY 200 OK\r\n" ++ encode_header(Headers));
encode_response(v10, Headers) ->
	list_to_binary("HTTP/1.0 200 OK\r\n" ++ encode_header(Headers));
encode_response(v11, Headers) ->
	list_to_binary("HTTP/1.1 200 OK\r\n" ++ encode_header(Headers)).
