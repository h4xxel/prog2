-module(test).
-export([crap/0, bench/3]).

crap() ->
	try rudy:stop()
	catch
		error:Error -> ok
	end,
	rudy:start(8080),
	bench("localhost", 8080).

wait(0) ->
	ok;
wait(N) ->
	receive
		done ->
			wait(N - 1)
	end.

bench(Host, Port) ->
	bench(Host, Port, 100).

bench(Host, Port, N) ->
	register(test, self()),
	Start = now(),
	run(N, Host, Port),
	wait(N),
	Finish = now(),
	timer:now_diff(Finish, Start).

run(0, _, _) ->
	ok;
run(N, Host, Port) ->
	spawn(fun() -> request(Host, Port) end),
	run(N-1, Host, Port).

request(Host, Port) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	{ok, Server} = gen_tcp:connect(Host, Port, Opt),
	gen_tcp:send(Server, http:get("/gurka")),
	{ok, Reply} = gen_tcp:recv(Server, 0),
	%io:format("~s~n", [Reply]),
	gen_tcp:close(Server),
	test ! done,
	ok.