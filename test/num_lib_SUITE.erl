-module(num_lib_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [{group, exported}].

groups() ->
	[{exported, [shuffle], [fib, fibm, fast_fibm, lucas, fact, is_square]},
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

is_square(_) ->
	Isq = fun(N) -> num_lib:is_square(N) end,

	false = Isq(-5),
	{true, 0} = Isq(0),
	{true, 1} = Isq(1),
	false = Isq(2),

	{true, 2} = Isq(4),
	false = Isq(8),
	{true, _} = Isq(9 bsl 56),
	false = Isq(11 bsl 56),
	{true, _} = Isq(9 bsl 100),
	false = Isq(17 bsl 100),

	L = [X * X || X <- lists:seq(0, 1_000)],
	L = [X || X <- lists:seq(0, 1_000_000), Isq(X) =/= false],

	false = Isq(8), % ok mod 4 but an odd number of trailing 0
	false = Isq(48), % ok mod 4, even number of trailing 0 but not a square
	false = Isq(33), % ok mod 8 but not mod 208
	false = Isq(17), % ok mod 8 and 208 but not mod 231
	false = Isq(273), % ok mod 8, 208 and 231 but not mod 145
	false = Isq(1401), % ok mod 8, 208, 231 and 145 but not mod 37
	false = Isq(7401), % ok mod 8, 208, 231, 145 and 37 but not mod 17
	false = Isq(1785), % ok mod 8, 208, 231 and 145, 37 and 17 but not square
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
