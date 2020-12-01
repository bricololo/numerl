-module(numerl_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(Large, 1_000_000_000_000_000_000_000_000). % 10^24

all() -> [{group, exported}].

groups() ->
	[
		{exported,
			[shuffle],
			[gcd, egcd, is_square, isqrt, icubrt, iroot, ipow,
				ipowm, jacobi]
		},
		{internal, [shuffle], []}
	].

gcd(_) ->
	Gcd = fun(A, B) -> numerl:gcd(A, B) end,

	L1 = [Gcd(A, 0) || A <- lists:seq(1, 10)],
	L1 = lists:seq(1, 10),
	L2 = [Gcd(A, 1) || A <- lists:seq(1, 10)],
	L2 = lists:duplicate(10, 1),
	L3 = [Gcd(2, A) =:= 1 || A <- lists:seq(1, 19, 2)],
	L3 = lists:duplicate(10, true),
	L4 = [Gcd(A,B) =:= Gcd(B,A) || A <- lists:seq(1,5), B <- lists:seq(1,5)],
	L4 = lists:duplicate(25, true),

	2 = Gcd(10, 6),
	2 = Gcd(10, -6),
	2 = Gcd(-10, 6),
	2 = Gcd(-10, -6),

	900000000090000000009 =
		Gcd(12345678901234567890123456789, 98765432109876543210987654321),
	ok.

egcd(_) ->
	F = fun(A,B) -> numerl:egcd(A, B) end,

	{5, 0, 1} = F(0, 5),
	{5, 1, 0} = F(5, 0),
	{1, -50, 51} = F(103, 101),
	{5, -1, -1} = F(20, -25),
	ok.

is_square(_) ->
	Isq = fun(N) -> numerl:is_square(N) end,

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

	L = [X * X || X <- lists:seq(0, 51)],
	L = [X || X <- lists:seq(0, 2700), Isq(X) =/= false],

	%false = Isq(193), % ok mod 8 and 63 but not mod 65
	%false = Isq(585), % ok mod 8, 63 and 65 but not mod 11
	%false = Isq(2545), % ok mod 8, 63, 65 and 11 but not a square
	ok.

isqrt(_) ->
	Isqrt = fun(N) -> numerl:isqrt(N) end,

	undefined = Isqrt(-1),
	0 = Isqrt(0),
	1 = Isqrt(1),
	1 = Isqrt(2),
	1 = Isqrt(3),
	2 = Isqrt(4),
	2 = Isqrt(8), % trigger the case where we have to decrease B
	N = rand:uniform(?Large),
	R = Isqrt(N),
	true = root_check(N, R, 2),
	ok.

icubrt(_) ->
	Icubrt = fun(N) -> numerl:icubrt(N) end,

	-1 = Icubrt(-1),
	-2 = Icubrt(-2),
	-2 = Icubrt(-8),
	-3 = Icubrt(-9),
	0 = Icubrt(0),
	1 = Icubrt(1),
	1 = Icubrt(2),
	1 = Icubrt(7),
	2 = Icubrt(8),
	2 = Icubrt(26),
	10 = Icubrt(1024),
	N = rand:uniform(?Large),
	R = Icubrt(N),
	true = root_check(N, R, 3),
	ok.

iroot(_) ->
	Iroot = fun(N, P) -> numerl:iroot(N, P) end,

	P = 4 + rand:uniform(10),
	N = rand:uniform(?Large),
	R = Iroot(N, P),
	true = root_check(N, R, P),
	ok.

ipow(_) ->
	F = fun(N, P) -> numerl:ipow(N, P) end,
	N = 1234567890,

	undefined = F(0, 0),
	0 = F(0, N),
	1 = F(N, 0),
	1 = F(1, N),
	N = F(N, 1),
	1 = F(-1, N),
	-1 = F(-1, N + 1),

	L1 = [1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049, 177147, 531441],
	L1 = [F(3, E) || E <- lists:seq(0, 12)],
	L2 = [1,6,36,216,1296,7776,46656,279936,1679616,10077696,60466176],
	L2 = [F(6, E) || E <- lists:seq(0, 10)],
	ok.

ipowm(_) ->
	F = fun(N, P) -> numerl:ipowm(N, P, 1024) end,
	N = 1234567890,
	P = 123456789012345678949, % a prime larger than 2^64

	undefined = F(0, 0),
	0 = F(0, N),
	1 = F(N, 0),
	1 = F(1, N),
	1 = numerl:ipowm(2, P - 1, P),
	1 = numerl:ipowm(3, P - 1, P),
	ok.

jacobi(_) ->
	F = fun(A, P) -> numerl:jacobi(A, P) end,

	0 = F(5, 25),
	0 = F(10, 25),

	1 = F(1, 47),
	1 = F(2, 47),

	-1 = F(5, 47),
	-1 = F(10, 47),

	ok.

root_check(N, R, P) ->
	B = numerl:ipow(R, P),
	A = numerl:ipow(R + 1, P),
	(B =< N) andalso (A > N).
