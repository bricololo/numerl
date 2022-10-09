%% This module regroups small utility functions which are of limited interest by
%% themselve but are needed as building blocks of more useful functions.

-module(num_lib).

-export([fib/1, fibm/2, fast_fibm/2]).
-export([lucas/1, lucas/3, lucasm/2, lucasm/4]).
-export([fact/1]).
-export([is_square/1, is_cube/1]).

-define(N64, 18_446_744_073_709_551_616).

% Fibonacci
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> element(1, fast_fib(N)).

% modular Fibonacci
fibm(0, _) -> 0;
fibm(1, _) -> 1;
fibm(N, M) -> element(1, fast_fibm(N, M)).

% modular fast Fibonacci
fast_fibm(1, _) -> {1, 1};
fast_fibm(2, _) -> {1, 2};
fast_fibm(N, M) ->
	{A, B} = fast_fibm(N bsr 1, M),
	A2 = A * A rem M,
	B2 = B * B,
	P = (A * B) bsl 1,
	fast_fibm(A2, B2, P, N band 1, M).


% Nth lucas number
lucas(N) -> lucas(2, 1, N).

% Nth lucas number of parameter A, B
lucas(A, _, 0) -> A;
lucas(_, B, 1) -> B;
lucas(A, B, N) ->
	{F1, F2} = fast_fib(N - 1),
	A * F1 + B * F2.

lucasm(N, M) -> lucasm(2, 1, N, M).

lucasm(A, _, 0, M) -> A rem M;
lucasm(_, B, 1, M) -> B rem M;
lucasm(A, B, N, M) ->
	{F1, F2} = fast_fibm(N - 1, M),
	(A * F1 + B * F2) rem M.

% Factorial
fact(0) -> 1;
fact(1) -> 1;
fact(N) when N band 1 =:= 1 -> N * fact(N - 1);
fact(N) -> fact(N, N - 2, 1, []).

-spec is_square(N :: integer()) -> false | {true, integer()}.
% @doc
% Returns false if N is not the square of an integer and {true, numerl:isqrt(N)}
% when N is a square.
% a fast test, most of the time computing the square root of N is avoided, as an
% example: 99.54% of the odd non squares below 10^8 are discarded without
% calling numerl:isqrt/1
is_square(N) when N < 0 -> false;
is_square(N) when N < 2 -> {true, N};
is_square(N) -> is_square(N, N band 3).

% @doc
% a fast test. Try avoiding computing the cube root if not needed. Returns false
% if N is not the cube of an integer and {true, icubrt(N)} when N is a cube.
is_cube(N) when N < 0 -> is_neg_cube(is_cube(-N));
is_cube(N) when N < 2 -> {true, N};
is_cube(N) when N < 8_000 ->
	is_small_cube(
		lists:member(
			N,
			[8, 27, 64, 125, 216, 343, 512, 729, 1_000, 1_331, 1_728, 2_197,
				2_744, 3_375, 4_096, 4_913, 5_832, 6_859]),
			N);
is_cube(N) -> is_cube_parity(N, N band 7).

%
% Implementation
%

% fast Fibonacci, returns {F_n, F_{n+1}}
% fast_fib(0) -> {0, 1};
fast_fib(1) -> {1, 1};
fast_fib(2) -> {1, 2};
fast_fib(N) ->
	{A, B} = fast_fib(N bsr 1),
	A2 = A * A,
	B2 = B * B,
	P = A * B bsl 1,
	fast_fib(A2, B2, P, N band 1).

fast_fib(A2, B2, P, 0) -> {P - A2, A2 + B2};
fast_fib(A2, B2, P, 1) -> {A2 + B2, P + B2}.

fast_fibm(A2, B2, P, 0, M) when P >= A2 -> {(P - A2) rem M, (A2 + B2) rem M};
fast_fibm(A2, B2, P, 0, M) -> {(P - A2) rem M + M, (A2 + B2) rem M};
fast_fibm(A2, B2, P, 1, M) -> {(A2 + B2) rem M, (P + B2) rem M}.

fact(V, 0, P, Acc) -> list_mul([P * V | Acc], []);
fact(V, I, P, Acc) when P < ?N64 -> fact(V + I, I - 2, P * V, Acc);
fact(V, I, P, Acc) -> fact(V, I, 1, [P | Acc]).

list_mul([F, S | T], Acc) -> list_mul(T, [F * S | Acc]);
list_mul([R], []) -> R;
list_mul([], Acc) -> list_mul(Acc, []);
list_mul([R], Acc) -> list_mul([R | Acc], []).

is_square(N, 0) -> is_even_square(N, num_util:p2(N));
is_square(N, 1) -> is_odd_square(N, N band 7);
is_square(_, _) -> false.

is_even_square(_, P2) when P2 band 1 =:= 1 -> false;
is_even_square(N, P2) ->
	N_odd = N bsr P2,
	is_even_square_(is_odd_square(N_odd, N_odd band 7), P2).

is_even_square_(false, _) -> false;
is_even_square_({true, Root}, P2) -> {true, Root bsl (P2 bsr 1)}.

is_odd_square(N, 1) -> square_mod_test(N);
is_odd_square(_, _) -> false.

square_mod_test(N) ->
	% 3 051 123 075 = 225 * 247 * 253 * 217
	T = N rem 3_051_123_075,
	is_square_(
		square_225_test(T) andalso % 3 * 3 * 5 * 5
		square_247_test(T) andalso % 13 * 19
		square_253_test(T) andalso % 11 * 23
		square_217_test(T),        % 7 * 31
		N).

square_225_test(T) ->
%	lists:member(T rem 225,
%		[0, 1, 4, 9, 16, 19, 25, 31, 34, 36, 46, 49, 54, 61, 64, 76, 79, 81, 91,
%			94, 99, 100, 106, 109, 121, 124, 126, 136, 139, 144, 151, 154, 166,
%			169, 171, 175, 181, 184, 189, 196, 199, 211, 214, 216]).
%
%		        1 0100 1000 0000 0000 1001 0000 % 196, 199, 211, 214, 216
%		0010 0001 0010 0000 1000 1010 0100 0000 % 166, 169, 171, 175, 181, 184, 189
%		0000 0100 1000 0001 0000 1001 0000 0000 % 136, 139, 144, 151, 154
%		0101 0010 0000 0000 0010 0100 0001 1000 % 99, 100, 106, 109, 121, 124, 126
%		0100 1000 0000 0010 1001 0000 0000 0001 % 64, 76, 79, 81, 91, 94
%		0010 0000 0100 0010 0100 0000 0001 0100 % 34, 36, 46, 49, 54, 61
%		1000 0010 0000 1001 0000 0010 0001 0011 % 0, 1, 4, 9, 16, 19, 25, 31
		(1 bsl (T rem 225)) band
			16#1480090_21208A40_04810900_52002418_48029001_20424014_82090213
			=/= 0.

square_247_test(T) ->
	lists:member(T rem 247,
		[0, 1, 4, 9, 16, 17, 23, 25, 26, 30, 35, 36, 38, 39, 42, 43, 49, 55, 61,
			62, 64, 66, 68, 74, 77, 81, 82, 87, 92, 95, 100, 101, 104, 114, 118,
			120, 121, 130, 131, 133, 134, 139, 140, 142, 144, 152, 153, 156,
			157, 159, 168, 169, 172, 178, 182, 191, 194, 195, 196, 199, 207,
			209, 218, 220, 225, 233, 234, 235, 237, 244]).

square_253_test(T) ->
	lists:member(T rem 253,
		[0, 1, 3, 4, 9, 12, 16, 23, 25, 26, 27, 31, 36, 47, 48, 49, 55, 58, 59,
			64, 69, 70, 71, 75, 77, 78, 81, 82, 92, 93, 100, 104, 108, 110, 115,
			119, 121, 124, 133, 141, 144, 146, 147, 154, 163, 165, 169, 170,
			174, 177, 179, 185, 187, 188, 190, 192, 196, 202, 207, 209, 210,
			213, 220, 223, 225, 231, 232, 234, 236, 242, 243, 246]).

square_217_test(T) ->
	lists:member(T rem 217,
		[0, 1, 2, 4, 7, 8, 9, 14, 16, 18, 25, 28, 32, 35, 36, 39, 49, 50, 51,
			56, 63, 64, 67, 70, 71, 72, 78, 81, 93, 95, 98, 100, 102, 107, 109,
			112, 113, 121, 126, 128, 133, 134, 140, 142, 144, 149, 155, 156,
			162, 163, 165, 169, 175, 183, 186, 190, 191, 193, 196, 200, 204,
			205, 211, 214]).

is_square_(false, _) -> false;
is_square_(true, N) ->
	S = numerl:isqrt(N),
	is_root(N, S, S * S).

is_root(N, Root, N) -> {true, Root};
is_root(_, _, _) -> false.

is_neg_cube(false) -> false;
is_neg_cube({_, Root}) -> {true, -Root}.

is_small_cube(false, _) -> false;
is_small_cube(true, N) -> {true, numerl:icubrt(N)}.

is_cube_parity(N, Odd) when Odd band 1 =:= 1 -> cube_mod_test(N);
is_cube_parity(N, 0) -> is_cube_even(N, num_util:p2(N));
is_cube_parity(_, _) -> false.

is_cube_even(N, P2) when P2 rem 3 =:= 0 -> cube_mod_test(N);
is_cube_even(_, _) -> false.

cube_mod_test(N) ->
	% 6 411 132 = 252 * 247 * 103
	T = N rem 6_411_132,
	is_cube_(
		cube_252_test(T) andalso % 2 * 2 * 3 * 3 * 7
		cube_247_test(T) andalso % 13 * 19
		cube_103_test(T),        % 103 is prime
		N).

cube_252_test(T) ->
	test(T, 252, [0, 1, 8, 27, 28, 35, 36, 55, 63, 64, 71, 91, 99, 125]).

cube_247_test(T) ->
	test(T, 247,
		[0, 1, 8, 12, 18, 26, 27, 31, 38, 39, 57, 64, 65, 77, 83, 96, 103,
			122]).

cube_103_test(T) ->
	test(T, 103,
		[0, 1, 3, 8, 9, 10, 13, 14, 22, 23, 24, 27, 30, 31, 34, 37, 39, 42]).

is_cube_(false, _) -> false;
is_cube_(true, N) ->
	C = numerl:icubrt(N),
	is_root(N, C, C * C * C).

% when M is suitable we just need to test up to M/2.
% when testing for square we need M - 1 to be an even square
% for cubes I got them experimentally, no clear patterns
test(T, M, L) ->
	R = T rem M,
	V = min(R, M - R),
	lists:member(V, L).
