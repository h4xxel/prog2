-module(pool).
-compile(export_all).
-export([start/1, stop/0]).

start(Port) ->
	register(pool, spawn(fun() -> init(Port) end)).
	
stop() ->
	exit(whereis(pool), "time to die").

init(Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} ->
			handler(Listen),
			gen_tcp:close(Listen),
			ok;
			{error, _} ->
				error
	end.

handler(Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Client} ->
			spawn(fun() -> request(Client) end);
			%request(Client);
		{error, _} ->
			error
	end,
	handler(Listen).

request(Client) ->
	Recv = gen_tcp:recv(Client, 0),
	case Recv of
		{ok, Str} ->
			Response = reply(http:parse_request(Str)),
			gen_tcp:send(Client, Response);
		{error, Error} ->
			io:format("rudy: error: ~w~n", [Error])
	end,
	gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
	timer:sleep(10),
	http:ok(URI ++ "\r\n").
