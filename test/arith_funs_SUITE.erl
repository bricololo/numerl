-module(arith_funs_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [{group, exported}].

groups() ->
	[{exported, [shuffle], [phi, d, sigma, moebius, omega]},
	 {internal, [shuffle], []}].

phi(_) ->
	Fun = fun(List) -> arith_funs:phi(List) end,
	Primes =  [2, 3, 5, 23, 67, 65537],

	% testing prime numbers
	Input1 = [[{P, 1}] || P <- Primes],
	Result1 = [P - 1 || P <- Primes],
	Result1 = [Fun(E) || E <- Input1],

	% testing prime powers
	Input2 = [[E] || E <- lists:zip(Primes, [7, 6, 5, 4, 3, 2])],
	% Result2 = [1*2^6, 2*3^5, 4*5^4, 22*23^3, 66*67^2, 65536*65537^1],
	Result2 = [64, 486, 2500, 267_674, 296_274, 4_295_032_832],
	Result2 = [Fun(E) || E <- Input2],

	% testing semi primes
	L = lists:zip(tl(Primes), lists:sublist(Primes, length(Primes) - 1)),
	Input3 = [[{M, 1},{N, 1}] || {M, N} <- L],
	Result3 = [(M - 1) * (N - 1) || [{M, _}, {N, _}] <- Input3],
	Result3 = [2, 8, 88, 1_452, 4_325_376],
	Result3 = [Fun(E) || E <- Input3].

d(_) ->
	Fun = fun(List) -> arith_funs:tau(List) end,
	Primes =  [2, 3, 5, 23, 67, 65537],

	% testing prime numbers
	Input1 = [[{P, 1}] || P <- Primes],
	Result1 = [2 || _ <- Primes],
	Result1 = [Fun(E) || E <- Input1],

	% testing prime powers
	Input2 = [[E] || E <- lists:zip(Primes, [7, 6, 5, 4, 3, 2])],
	Result2 = [E + 1 || [{_, E}] <- Input2],
	Result2 = [8, 7, 6, 5, 4, 3],
	Result2 = [Fun(E) || E <- Input2],

	% testing semi primes
	L = lists:zip(tl(Primes), lists:sublist(Primes, length(Primes) - 1)),
	Input3 = [[{M, 1},{N, 1}] || {M, N} <- L],
	Result3 = [4 || _ <- Input3],
	Result3 = [Fun(E) || E <- Input3].

sigma(_) ->
	Fun = fun(List) -> arith_funs:sigma(List) end,
	Primes =  [2, 3, 5, 23, 67, 65537],

	% testing prime numbers
	Input1 = [[{P, 1}] || P <- Primes],
	Result1 = [P + 1 || P <- Primes],
	Result1 = [Fun(E) || E <- Input1],

	% testing prime powers
	Input2 = [[E] || E <- lists:zip(Primes, [7, 6, 5, 4, 3, 2])],
	Result2 = [(numerl:ipow(P, E + 1) - 1) div (P - 1) || [{P, E}] <- Input2],
	Result2 = [255, 1093, 3906, 292_561, 305_320, 4_295_163_907],
	Result2 = [Fun(E) || E <- Input2],

	% testing semi primes
	L = lists:zip(tl(Primes), lists:sublist(Primes, length(Primes) - 1)),
	Input3 = [[{M, 1}, {N, 1}] || {M, N} <- L],
	Result3 = [(M + 1) * (N + 1) || [{M, _}, {N, _}] <- Input3],
	Result3 = [12, 24, 144, 1_632, 4_456_584],
	Result3 = [Fun(E) || E <- Input3].

moebius(_) ->
	Fun = fun(List) -> arith_funs:mu(List) end,
	Primes =  [2, 3, 5, 23, 67, 65537],

	% testing prime numbers
	Input1 = [[{P, 1}] || P <- Primes],
	Result1 = [-1 || _ <- Primes],
	Result1 = [Fun(E) || E <- Input1],

	% testing prime powers
	Input2 = [[E] || E <- lists:zip(Primes, [7, 6, 5, 4, 3, 2])],
	Result2 = [0 || _ <- Primes],
	Result2 = [Fun(E) || E <- Input2],

	% testing semi primes
	L = lists:zip(tl(Primes), lists:sublist(Primes, length(Primes) - 1)),
	Input3 = [[{M, 1},{N, 1}] || {M, N} <- L],
	Result3 = [1 || _ <- L],
	Result3 = [Fun(E) || E <- Input3].

omega(_) ->
	Fun = fun(List) -> arith_funs:omega(List) end,
	Primes =  [2, 3, 5, 23, 67, 65537],

	% testing prime numbers
	Input1 = [[{P, 1}] || P <- Primes],
	Result1 = [1 || _ <- Primes],
	Result1 = [Fun(E) || E <- Input1],

	% testing prime powers
	Input2 = [[E] || E <- lists:zip(Primes, [7, 6, 5, 4, 3, 2])],
	% Result2 = [1*2^6, 2*3^5, 4*5^4, 22*23^3, 66*67^2, 65536* 65577^1],
	Result2 = [1 || _ <- Primes],
	Result2 = [Fun(E) || E <- Input2],

	% testing semi primes
	L = lists:zip(tl(Primes), lists:sublist(Primes, length(Primes) - 1)),
	Input3 = [[{M, 1},{N, 1}] || {M, N} <- L],
	Result3 = [2 || _ <- tl(Primes)],
	Result3 = [Fun(E) || E <- Input3].
