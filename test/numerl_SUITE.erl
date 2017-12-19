-module(numerl_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [{group, exported}].

groups() ->
	[{exported,
		[shuffle],
		[gcd,
%		 egcd,
		 is_square,
		 isqrt]
%		 icubrt,
%		 iroot,
%		 ipow,
%		 ipowm,
%		 jacobi]
	 },
	 {internal, [shuffle], []}].

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

	false = Isq(193), % ok mod 8 and 63 but not mod 65
	false = Isq(585), % ok mod 8, 63 and 65 but not mod 11
	false = Isq(2545), % ok mod 8, 63, 65 and 11 but not a square
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
	ok.