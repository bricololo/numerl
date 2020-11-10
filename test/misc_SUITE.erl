-module(misc_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [{group, exported}].

groups() ->
	[{exported, [shuffle], [fib, fibm, fast_fibm, lucas, fact]},
	 {internal, [shuffle], []}].

fib(_) ->
	F = fun(N) -> misc:fib(N) end,

	1 = F(2),
	2 = F(3),
	3 = F(4),
	5 = F(5),
	8 = F(6),

	R = fib_def(1000),
	R = F(1000),
	ok.

fibm(_) ->
	F = fun(N) -> misc:fib(N) end,
	G = fun(N) -> misc:fibm(N, N) end,

	Result = lists:duplicate(10, true),
	Result = [F(N) rem N =:= G(N) || N <- lists:seq(100, 1000, 100)],

	H = fun(N, M) -> misc:fibm(N, M) end,
	[] =
		[{N, F(N) rem 1000, H(N, 1000)} ||
			N <- lists:seq(1, 1000),
			(F(N) rem 1000) =/= H(N, 1000)],
	ok.

fast_fibm(_) ->
	F = fun(N, M) -> misc:fast_fibm(N, M) end,
	T = fun(N, M) -> {fib_def(N) rem M, fib_def(N + 1) rem M} end,

	[] =
		[{N, M, F(N, M),  C} ||
			N <- lists:seq(8, 2000, 17),
			M <- [10, 1234567, 9876543210987654321012345],
			C <- [T(N, M)],
			F(N, M) =/= C],
	ok.

lucas(_) ->
	F = fun(N) -> misc:lucas(N) end,

	3 = F(2),
	4 = F(3),
	7 = F(4),
	11 = F(5),
	18 = F(6),

	[] = [N || N <- lists:seq(12, 2000, 19), F(N) =/= lucas_def(N)],

	% for all n: v_2n = u_n * v_n where v_i = fib(i)
	[] =
		[{N, misc:fib(2 * N), F(N), misc:fib(N)} ||
			N <- lists:seq(500, 600),
			misc:fib(2 * N) =/= F(N) * misc:fib(N)],
	ok.

fact(_) ->
	[] = [N || N <- lists:seq(20, 50), misc:fact(N) =/= fact_def(N)],
	ok.

fib_def(0) -> 0;
fib_def(1) -> 1;
%fib_def(N) -> fib_def(N - 1) + fib_def(N - 2).
% doing a pure recursion is really too slow...
fib_def(N) -> fib_def(0, 1, N, 2).

fib_def(A, B, N, N) -> A + B;
fib_def(A, B, Lim, N) -> fib_def(B, A + B, Lim, N + 1).

lucas_def(0) -> 2;
lucas_def(1) -> 1;
%lucas_def(N) -> lucas_def(N - 1) + lucas_def(N - 2).
% doing a pure recursion is really too slow...
lucas_def(N) -> fib_def(2, 1, N, 2).

fact_def(0) -> 1;
fact_def(N) -> N * fact_def(N - 1).
