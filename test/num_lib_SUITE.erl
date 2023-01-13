-module(num_lib_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [{group, exported}].

groups() ->
	[{exported, [shuffle],
		[fib, fibm, fast_fibm, lucas, lucasm, fact, is_square, is_cube]},
	 {internal, [shuffle], []}].

fib(_) ->
	F = fun(N) -> num_lib:fib(N) end,

	0 = F(0),
	1 = F(1),
	1 = F(2),
	2 = F(3),
	3 = F(4),
	5 = F(5),
	8 = F(6),

	R = fib_def(1000),
	R = F(1000),
	ok.

fibm(_) ->
	F = fun(N) -> num_lib:fib(N) end,
	G = fun(N) -> num_lib:fibm(N, N) end,

	Result = lists:duplicate(10, true),
	Result = [F(N) rem N =:= G(N) || N <- lists:seq(100, 1000, 100)],

	H = fun(N, M) -> num_lib:fibm(N, M) end,
	[] =
		[{N, F(N) rem 1000, H(N, 1000)} ||
			N <- lists:seq(0, 1000),
			(F(N) rem 1000) =/= H(N, 1000)],
	ok.

fast_fibm(_) ->
	F = fun(N, M) -> num_lib:fast_fibm(N, M) end,
	T = fun(N, M) -> {fib_def(N) rem M, fib_def(N + 1) rem M} end,

	[] =
		[{N, M, F(N, M),  C} ||
			N <- lists:seq(8, 2000, 17),
			M <- [10, 1234567, 9876543210987654321012345],
			C <- [T(N, M)],
			F(N, M) =/= C],
	ok.

lucas(_) ->
	F = fun(N) -> num_lib:lucas(N) end,

	2 = F(0),
	1 = F(1),
	3 = F(2),
	4 = F(3),
	7 = F(4),
	11 = F(5),
	18 = F(6),

	[] = [N || N <- lists:seq(12, 2000, 19), F(N) =/= lucas_def(N)],

	% for all n: v_2n = u_n * v_n where v_i = fib(i)
	[] =
		[{N, num_lib:fib(2 * N), F(N), num_lib:fib(N)} ||
			N <- lists:seq(500, 600),
			num_lib:fib(2 * N) =/= F(N) * num_lib:fib(N)],
	ok.

lucasm(_) ->
	F2 = fun(N, M) -> num_lib:lucasm(N, M) end,
	F4 = fun(A, B, N, M) -> num_lib:lucasm(A, B, N, M) end,

	2 = F2(0, 10),
	2 = F4(2, 1, 0, 10),
	3 = F4(3, 1, 0, 10),

	1 = F2(1, 10),
	1 = F4(2, 1, 1, 10),
	2 = F4(3, 2, 1, 10),

	3 = F2(2, 10),
	3 = F4(2, 1, 2, 10),
	5 = F4(3, 2, 2, 10),

	1 = F2(5, 10),
	8 = F2(6, 10),

	% TODO: find a relation between fib seq and lucas seq when {A, B} is not
	% {2, 1}
%	L = fun(N) -> F4(3, 2, N, 10_000) end,
%	F = fun(N) -> num_lib:fibm(N, 10_000) end,
%	[] =
%		[{N, F(2 * N), L(N), F(N)} ||
%			N <- lists:seq(123, 456, 17),
%			F(2 * N) =/= L(N) * F(N)],
	ok.

fact(_) ->
	[] = [N || N <- [0, 1 | lists:seq(20, 50)], num_lib:fact(N) =/= fact_def(N)],
	ok.

is_square(_) ->
	Isq = fun(N) -> is_power:square(N) end,

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

	false = Isq(8), % ok mod 4 but an odd number of trailing 0
	false = Isq(48), % ok mod 4, even number of trailing 0 but not a square
	false = Isq(17), % ok mod 8 but not mod 225
	false = Isq(241), % ok mod 8 and 225 but not mod 247
	false = Isq(481), % ok mod 8, 225 and 247 but not mod 253
	false = Isq(1881), % ok mod 8, 225, 247 and 253 but not mod 217
	false = Isq(28809), % ok mod 8, 225, 247, 253 and 217 but not a square

	Result = [I || I <- lists:seq(0, 1_000_000), false =/= Isq(I)],
	Result = [I * I || I <- lists:seq(0, 1_000)],

	ok.

is_cube(_) ->
	F = fun(N) -> is_power:cube(N) end,

	% edge cases
	{true, 0} = F(0),
	{true, 1} = F(1),
	false = F(7),
	{true, 2} = F(8),
	false = F(9),
	{true, 20} = F(8_000),
	false = F(7_999),
	false = F(8_001),

	% negative numbers
	{true, -1} = F(-1),
	false = F(-7),
	false = F(-9),
	{true, -5} = F(-125),
	{true, -20} = F(-8_000),

	% coverage...
	false = F(252 * 247 * 103), % would pass all the mod tests but is discarded
								% because it is not the product of an odd number
								% by a power of 8
	false = F(252 * 247 * 103 * 2), % pass all the mod tests but is not a cube
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
