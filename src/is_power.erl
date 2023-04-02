-module(is_power).

-export([check/2, square/1, cube/1, prime/1]).

% returns {true, sqrt(N)} if N is a square and false otherwise
square(N) -> check(N, 2).

% returns {true, cubrt(N)} if N is a cube and false otherwise
cube(N) -> check(N, 3).

% returns {true, K} if N = K^P and false otherwise
check(0, _) -> {true, 0};
check(1, _) -> {true, 1};
check(N, 1) -> {true, N};
check(N, Even) when N < 0, Even band 1 =:= 0 -> false;
check(N, Odd) when N < 0, Odd band 1 =:= 1 ->
	case check(-N, Odd) of false -> false; {true, R} -> {true, -R} end;
check(N, P) when N < 1 bsl P -> false;
check(N, P) when N band 1 =:= 1 -> power_test(N, P);
check(N, P) ->
	case parity_test(N, P) of
		true -> power_test(N, P);
		false -> false
	end.

% return {true, P, K} if N = K^P with P prime and false otherwise
prime(N) when N > 1, N band 1 =:= 0 ->
	K = num_util:log_est(N)-1,
	case 1 bsl K of N -> {true, 2, K}; _ -> false end;
prime(N) -> prime(N, N).

prime(N, Q) ->
	case primes:rabin_miller_test(Q) of
		{false, A} ->
			case numerl:gcd(numerl:ipow(A, Q)-A, Q) of
				1 -> false;
				Q -> false;
				D -> prime(N, D)
			end;
		true -> prime_finish(N, Q)
	end.

prime_finish(N, P) -> prime_finish(N, P, [{P, 1}]).

prime_finish(N, P, [{H, K} | _] = L) when H < N ->
	prime_finish(N, P, [{H*H, K bsl 1} | L]);
prime_finish(N, P, L) -> prime_finish_k(N, P, tl(L), 0).

prime_finish_k(1, P, _, K) -> {true, P, K};
prime_finish_k(N, P, [{H, _} | T], K) when N < H -> prime_finish_k(N, P, T, K);
prime_finish_k(N, P, [{H, W} | T], K) when N rem H =:= 0 ->
	prime_finish_k(N div H, P, T, K+W);
%prime_finish_k(_, _, [], _) -> false;
prime_finish_k(_, _, _, _) -> false.

%%
%% implementation
%%

power_test(N, 1) -> {true, N};
power_test(N, 2) -> p2nd_test(N);
power_test(N, 3) -> p3rd_test(N);
power_test(N, 5) -> p5th_test(N);
power_test(N, 7) -> p7th_test(N);
power_test(N, 11) -> p11th_test(N);
power_test(N, Even) when Even band 1 =:= 0 ->
	Shift = num_util:p2(Even),
	case power_test(N, Even bsr Shift) of
		false -> false;
		{true, R} ->
			case power_test_r(R, 2, 1 bsl Shift) of
				false -> false;
				Else -> Else
			end
	end;
power_test(N, Triple) when Triple rem 3 =:= 0 ->
	{Rem, Trp} = adjust(Triple, 3, 1),
	case power_test(N, Rem) of
		false -> false;
		{true, R} ->
			case power_test_r(R, 3, 1, Trp) of
				false -> false;
				Else -> Else
			end
	end;
power_test(N, Fifth) when Fifth rem 5 =:= 0 ->
	{Rem, Fth} = adjust(Fifth, 5, 1),
	case power_test(N, Rem) of
		false -> false;
		{true, R} ->
			case power_test_r(R, 5, 1, Fth) of
				false -> false;
				Else -> Else
			end
	end;
power_test(N, Seventh) when Seventh rem 7 =:= 0 ->
	{Rem, Sth} = adjust(Seventh, 7, 1),
	case power_test(N, Rem) of
		false -> false;
		{true, R} ->
			case power_test_r(R, 7, 1, Sth) of
				false -> false;
				Else -> Else
			end
	end;
power_test(N, Eleventh) when Eleventh rem 11 =:= 0 ->
	{Rem, Eth} = adjust(Eleventh, 11, 1),
	case power_test(N, Rem) of
		false -> false;
		{true, R} ->
			case power_test_r(R, 11, 1, Eth) of
				false -> false;
				Else -> Else
			end
	end;
power_test(N, P) ->
	R = numerl:iroot(N, P),
	is_root(N, R, numerl:ipow(R, P)).

power_test_r(N, P, Goal) -> power_test_r(N, P, 1, Goal).

power_test_r(N, 1, Goal, Goal) -> {true, N};
power_test_r(N, P, Reach, Goal) -> % P is either 2, 3, 5, 7 or 11
	case power_test(N, P) of
		false -> false;
		{true, R} -> power_test_r(R, P, Reach*P, Goal)
	end.

parity_test(N, M) ->
	case N band(1 bsl M-1) of
		0 -> case num_util:p2(N) rem M of 0 -> true; _ -> false end;
		_ -> false
	end.

is_power(false, _, _) -> false;
is_power(true, N, P) ->
	R = numerl:iroot(N, P),
	is_root(N, R, numerl:ipow(R, P)).

test(Val, Mod, List) ->
	R = Val rem Mod,
	T = min(R, Mod-R),
	lists:member(T, List).

test_tuple(Val, Mod, Tuple) ->
	R = Val rem Mod,
	T =  1+min(R, Mod-R),
	element(T, Tuple).

is_root(N, Root, N) -> {true, Root};
is_root(_, _, _) -> false.

adjust(V, D, P) when V rem D =/= 0 -> {V, P};
adjust(V, D, P) -> adjust(V div D, D, P*D).

-spec p2nd_test(N :: integer()) -> false | {true, integer()}.
% @doc
% Returns false if N is not the square of an integer and {true, numerl:isqrt(N)}
% when N is a square.
% a fast test, most of the time computing the square root of N is avoided, as an
% example: 99.54% of the odd non squares below 10^8 are discarded without
% calling numerl:isqrt/1
p2nd_test(N) when N < 0 -> false;
p2nd_test(N) -> square(N, N band 3).

square(N, 0) -> even_square(N, num_util:p2(N));
square(N, 1) -> odd_square(N, N band 7);
square(_, _) -> false.

even_square(_, P2) when P2 band 1 =:= 1 -> false;
even_square(N, P2) ->
	N_odd = N bsr P2,
	even_square_(odd_square(N_odd, N_odd band 7), P2).

even_square_(false, _) -> false;
even_square_({true, Root}, P2) -> {true, Root bsl (P2 bsr 1)}.

odd_square(N, 1) -> square_mod_test(N);
odd_square(_, _) -> false.

square_mod_test(N) ->
	% 3 051 123 075 = 225*247*253*217
	T = N rem 3_051_123_075,
	is_power(
		square_test_bin(T, 225, t2(225)) andalso % 3*3*5*5
		square_test_bin(T, 247, t2(247)) andalso % 13*19
		square_test_bin(T, 253, t2(253)) andalso % 11*23
		square_test_bin(T, 217, t2(217)),        % 7*31
		N,
		2).

square_test_bin(T, Mod, Mask) -> (1 bsl (T rem Mod)) band Mask =/= 0.

t2(225) ->
%	[0, 1, 4, 9, 16, 19, 25, 31, 34, 36, 46, 49, 54, 61, 64, 76, 79, 81, 91, 94,
%		99, 100, 106, 109, 121, 124, 126, 136, 139, 144, 151, 154, 166, 169,
%		171, 175, 181, 184, 189, 196, 199, 211, 214, 216];
% integer_to_list(
%	lists:sum([1 bsl I ||
%		I <- [0,1,4,9,16,19,25,31,34,36,46,49,54,61,64,76,79,81,91,94,99,100,
%			106,109,121,124,126,136,139,144,151,154,166,169,171,175,181,184,189,
%			196,199,211,214,216]]),16).
	16#1480090_21208A40_04810900_52002418_48029001_20424014_82090213;
t2(247) ->
%	[0, 1, 4, 9, 16, 17, 23, 25, 26, 30, 35, 36, 38, 39, 42, 43, 49, 55, 61, 62,
%		64, 66, 68, 74, 77, 81, 82, 87, 92, 95, 100, 101, 104, 114, 118, 120,
%		121, 130, 131, 133, 134, 139, 140, 142, 144, 152, 153, 156, 157, 159,
%		168, 169, 172, 178, 182, 191, 194, 195, 196, 199, 207, 209, 218, 220,
%		225, 233, 234, 235, 237, 244];
	16#102E02_1402809C_80441300_B301586C_03440130_90862415_60820CD8_46830213;
t2(253) ->
%	[0, 1, 3, 4, 9, 12, 16, 23, 25, 26, 27, 31, 36, 47, 48, 49, 55, 58, 59, 64,
%		69, 70, 71, 75, 77, 78, 81, 82, 92, 93, 100, 104, 108, 110, 115, 119,
%		121, 124, 133, 141, 144, 146, 147, 154, 163, 165, 169, 170, 174, 177,
%		179, 185, 187, 188, 190, 192, 196, 202, 207, 209, 210, 213, 220, 223,
%		225, 231, 232, 234, 236, 242, 243, 246];
	16#4C1582_90268411_5A0A4628_040D2020_12885110_300668E1_0C838010_8E81121B;
t2(217) ->
%	[0, 1, 2, 4, 7, 8, 9, 14, 16, 18, 25, 28, 32, 35, 36, 39, 49, 50, 51, 56,
%		63, 64, 67, 70, 71, 72, 78, 81, 93, 95, 98, 100, 102, 107, 109, 112,
%		113, 121, 126, 128, 133, 134, 140, 142, 144, 149, 155, 156, 162, 163,
%		165, 169, 175, 183, 186, 190, 191, 193, 196, 200, 204, 205, 211, 214].
	16#483112_C480822C_18215061_42032854_A00241C9_810E0099_12054397.

% @doc
% a fast test. Try avoiding computing the cube root if not needed. Returns false
% if N is not the cube of an integer and {true, icubrt(N)} when N is a cube.
p3rd_test(N) when N < 8_000 ->
	small_cube(
		lists:member(
			N,
			[8, 27, 64, 125, 216, 343, 512, 729, 1_000, 1_331, 1_728, 2_197,
				2_744, 3_375, 4_096, 4_913, 5_832, 6_859]),
			N);
p3rd_test(N) ->
	% 6 411 132 = 252*247*103
	T = N rem 6_411_132,
	is_power(
		element(1+N band 7,
			{true, true, false, true, false, true, false, true}) andalso
		test_tuple(T, 252, t3(252)) andalso % 2*2*3*3*7
		test(T, 247, t3(247)) andalso % 13*19
		test(T, 103, t3(103)),        % 103 is prime
		N,
		3).


small_cube(false, _) -> false;
small_cube(true, N) -> {true, numerl:icubrt(N)}.

%t3(252) -> [0, 1, 8, 27, 28, 35, 36, 55, 63, 64, 71, 91, 99, 125];
t3(252) ->
	{
		true, % 0
		true, % 1
		false, false, false, false, false, false,
		true, % 8
		false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false,
		true, % 27
		true, % 28
		false, false, false, false, false, false,
		true, % 35
		true, % 36
		false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false,
		true, % 55
		false, false, false, false, false, false, false,
		true, % 63
		true, % 64
		false, false, false, false, false, false,
		true, % 71
		false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false,
		true, % 91
		false, false, false, false, false, false, false,
		true, % 99
		false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false,
		true, % 125
		false
	};
t3(247) -> [0,1,8,12,18,26,27,31,38,39,57,64,65,77,83,96,103,122];
t3(103) -> [0, 1, 3, 8, 9, 10, 13, 14, 22, 23, 24, 27, 30, 31, 34, 37, 39, 42].

p5th_test(N) ->
	% 236 240 064 225 = 225*121*251*191*181
	T = N rem (236_240_064_225),
	is_power(
		test_tuple(T, 225, t5(225)) andalso % 3*3*5*5
		test_tuple(T, 121, t5(121)) andalso % 11*11
		test(T, 251, t5(251)) andalso % prime
		test(T, 191, t5(191)) andalso % prime
		test(T, 181, t5(181)),        % prime
		N,
		5).

%t5(225) -> [0,1,7,18,25,26,32,43,49,50,68,74,76,82,99,100,101,107];
t5(225) ->
	{
		true, % 0
		true, % 1
		false, false, false, false, false,
		true, % 7
		false, false, false, false, false, false, false, false, false, false,
		true, % 18
		false, false, false, false, false, false,
		true, % 25
		true, % 26
		false, false, false, false, false,
		true, % 32
		false, false, false, false, false, false, false, false, false, false,
		true, % 43
		false, false, false, false, false,
		true, % 49
		true, % 50
		false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false,
		true, % 68
		false, false, false, false, false,
		true, % 74
		false,
		true, % 76
		false, false, false, false, false,
		true, % 82
		false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false,
		true, % 99
		true, % 100
		true, % 101
		false, false, false, false, false,
		true, % 107
		false, false, false, false, false
	};
%t5(121) -> [0, 1, 10, 12, 21, 23, 32, 34, 43, 45, 54, 56];
t5(121) ->
	{
		true, % 0
		true, % 1
		false, false, false, false, false, false, false, false,
		true, % 10
		false,
		true, % 12
		false, false, false, false, false, false, false, false,
		true, % 21
		false,
		true, % 23
		false, false, false, false, false, false, false, false,
		true, % 32
		false,
		true, % 34
		false, false, false, false, false, false, false, false,
		true, % 43
		false,
		true, % 45
		false, false, false, false, false, false, false, false,
		true, % 54
		false,
		true, % 56
		false, false, false, false
	};
t5(251) ->
	[0, 1, 2, 4, 5, 8, 10, 16, 20, 25, 32, 40, 47, 50, 51, 63, 64, 69, 80, 91,
		94, 100, 102, 113, 123, 125];
t5(191) -> [0,1,5,6,11,14,25,30,31,32,36,37,38,41,52,55,66,69,70,84];
t5(181) -> [0,1,7,17,19,26,32,39,43,48,49,61,62,65,72,73,80,88,89].

p7th_test(N) ->
	% 486 791 137 = 49*239*211*197),
	T = N rem (486_791_137),
	is_power(
		test_tuple(T, 49, t7(49))   andalso % 7*7
		test(T, 239, t7(239)) andalso % prime
		test(T, 211, t7(211)) andalso % prime
		test(T, 197, t7(197)),        % prime
		N,
		7).

%t7(49) -> [0, 1, 18, 19];
t7(49) ->
	{
		true, % 0
		true, % 1
		false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false,
		true, % 18
		true, % 19
		false, false, false, false, false
	};
t7(239) -> [0,1,6,22,23,28,36,40,51,52,67,71,73,75,76,101,107,111];
t7(211) -> [0,1,10,14,15,19,21,23,55,61,71,74,77,83,100,104,107];
t7(197) -> [0, 1, 6, 14, 19, 20, 33, 36, 68, 69, 77, 83, 84, 87, 93].

p11th_test(N) ->
	% 443 607 417 = 121*199*89*207
	T = N rem (443_607_417),
	is_power(
		test_tuple(T, 121, t11(121)) andalso % 11*11
		test(T, 199, t11(199)) andalso % prime
		test(T, 89, t11(89))   andalso % prime
		test(T, 207, t11(207)),        % 3*3*23
		N,
		11).

%t11(121) -> [0, 1, 3, 9, 27, 40];
t11(121) ->
	{
		true, % 0
		true, % 1
		false,
		true, % 3
		false, false, false, false, false,
		true, % 9
		false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false,
		true, % 27
		false, false, false, false, false, false, false, false, false, false,
		false, false,
		true, % 40
		false, false, false, false, false, false, false, false, false, false,
		false, false, false, false, false, false, false, false, false, false
	};
t11(199) -> [0, 1, 19, 21, 24, 37, 43, 58, 92, 93];
t11(89) -> [0, 1, 12, 34, 37];
t11(207) -> [0, 1, 22, 23, 45, 46, 47, 68, 70, 91, 92].
