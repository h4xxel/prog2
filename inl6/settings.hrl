-define(OPT,[binary,
	{packet, 0},
	{reuseaddr, true},
	{active, false},
	{nodelay, true}]).
-define(PORT, 8080).
-define(HEADER, [{’icy-name’, "h4xxel.org"},
	{’icy-genre’, "Pirate"},
	{’icy-notice1’, "Pirate radio"}]).
-define(TIMEOUT, 5000).

-define(ERROR(Msg, Arg), io:format(Msg, Arg)).

-define(LOG(Msg, Arg), io:format(Msg, Arg)).
%-define(LOG(Msg, Arg), ok).

-define (CHUNK_SIZE, 8192).
