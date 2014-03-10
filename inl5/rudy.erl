-module(rudy).
-compile(export_all).
-export([start/1, stop/0]).

pool([]) ->
	%io:format("pool depleted~n"),
	receive
		{done, Pid} ->
			%io:format("handler returned~n"),
			pool([{handler, Pid}])
	end;
pool(Handlers) ->
	receive
		{accept, Sock} ->
			%io:format("pooled request~n"),
			[{handler, Pid} | T] = Handlers,
			Pid ! {handle, Sock, self()},
			pool(T);
		{done, Pid} ->
			%io:format("handler returned~n"),
			pool([{handler, Pid} | Handlers])
	end.

start(Port) ->
	register(rudy, spawn(fun() -> init(Port) end)).
	
stop() ->
	exit(whereis(rudy), "time to die").

init(Port) ->
	Pool = spawn(fun() -> pool([]) end),
	spawn_pool(50, Pool),
	Opt = [list, {active, false}, {reuseaddr, true}, {backlog, 1000}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} ->
			acceptor(Listen, Pool),
			gen_tcp:close(Listen),
			ok;
			{error, _} ->
				error
	end.

spawn_pool(0, _) ->
	ok;
spawn_pool(N, Pool) ->
	Pool ! {done, spawn(fun() -> handler() end)},
	spawn_pool(N - 1, Pool).

acceptor(Listen, Pool) ->
	case gen_tcp:accept(Listen) of
		{ok, Client} ->
			%io:format("accepted connection~n"),
			Pool ! {accept, Client};
		{error, _} ->
			error
	end,
	acceptor(Listen, Pool).

handler() ->
	receive
		{handle, Client, Pool} ->
			request(Client),
			Pool ! {done, self()},
			handler();
		quit ->
			ok
	end.

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

loop(0) ->
	done;
loop(N) ->
	loop(N - 1).

reply({{get, URI, _}, _, _}) ->
	loop(100000000),
	http:ok(URI ++ "\r\n").
