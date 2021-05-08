%% This module regroups small utility functions which are of limited interest by
%& themselve but are needed as building blocks of more useful functions.

-module(num_lib).

-export([fib/1, fibm/2, fast_fibm/2]).
-export([lucas/1, lucas/3, lucasm/2, lucasm/4]).
-export([fact/1]).
-export([is_square/1, is_cube/1]).

-define(N64, 18446744073709551616).

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
	case N band 1 of
		0 ->
			{(P - A2) rem M + case P >= A2 of true -> 0; _ -> M end,
			(A2 + B2) rem M};
		1 -> {(A2 + B2) rem M, (P + B2) rem M}
	end.

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
% example: 99.77% of the odd non squares below 10^8 are discarded without
% isqrt/1 and the ratio assymptotally grow above 99.96%
is_square(N) when N < 0 -> false;
is_square(N) when N < 2 -> {true, N};
is_square(N) ->
	case N band 3 of
		0 -> is_even_square(N, num_util:p2(N));
		1 -> is_odd_square(N, N band 7);
		_ -> false
	end.

% @doc
% a fast test. Try avoiding computing the cube root if not needed. Returns false
% if N is not the cube of an integer and {true, icubrt(N)} when N is a cube.
is_cube(N) when N < 0 ->
	case is_cube(-N) of
		false -> false;
		{_, C} -> {true, -C}
	end;
is_cube(N) when N < 2 -> {true, N};
is_cube(N) when N < 8_000 ->
	case lists:member(
			N,
			[8, 27, 64, 125, 216, 343, 512, 729, 1_000, 1_331, 1_728, 2_197,
				2_744, 3_375, 4_096, 4_913, 5_832, 6_859]) of
		false -> false;
		true -> {true, numerl:icubrt(N)}
	end;
is_cube(N) ->
	case N band 1 of
		0 -> is_cube(N, num_util:p2(N));
		1 -> cube_mod_test(N)
	end.

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
	P = A * B,
	case N band 1 of
		0 -> {P bsl 1 - A2, A2 + B2};
		1 -> {A2 + B2, P bsl 1 + B2}
	end.

fact(V, 0, P, Acc) -> list_mul([P * V | Acc], []);
fact(V, I, P, Acc) when P < ?N64 -> fact(V + I, I - 2, P * V, Acc);
fact(V, I, P, Acc) -> fact(V, I, 1, [P | Acc]).

list_mul([F, S | T], Acc) -> list_mul(T, [F * S | Acc]);
list_mul([R], []) -> R;
list_mul([], Acc) -> list_mul(Acc, []);
list_mul([R], Acc) -> list_mul([R | Acc], []).

is_even_square(_, P2) when P2 band 1 =:= 1 -> false;
is_even_square(N, P2) ->
	N_odd = N bsr P2,
	case is_odd_square(N_odd, N_odd band 7) of
		false -> false;
		{_, Root} -> {true, Root bsl (P2 bsr 1)}
	end.

is_odd_square(N, 1) -> square_mod_test(N);
is_odd_square(_, _) -> false.

square_mod_test(N) ->
	% 438 918 480 = 208 * 231 * 63 * 145
	T = N rem 438_918_480,
	case 
		square_208_test(T) andalso
		square_231_test(T) andalso
		square_63_test(T) andalso
		square_145_test(T) of
		false -> false;
		true -> is_square_(N)
	end.

% as this test is applied only on odd values, we can skip the even remainders
% and thus make the list twice as short
square_208_test(T) ->
	lists:member(T rem 208, [1,9,17,25,49,65,81,105,113,121,129,153,169,185]).

square_231_test(T) ->
	lists:member(T rem 231,
		[0, 1, 4, 9, 15, 16, 22, 25, 36, 37, 42, 49, 58, 60, 64, 67, 70, 78, 81,
			88, 91 ,93, 99, 100, 102, 114, 121, 126, 130, 133, 135, 141, 144,
			147, 148, 154, 163, 165, 168, 169, 177, 190, 196, 198, 207, 210,
			214, 225]).

square_63_test(T) ->
	lists:member(T rem 63, [0,1,4,7,9,16,18,22,25,28,36,37,43,46,49,58]).

square_145_test(T) ->
	test(T, 145,
		[0,1,4,5,6,9,16,20,24,25,29,30,34,35,36,45,49,51,54,59,64,65,71]).

is_square_(N) ->
	S = numerl:isqrt(N),
	case S * S of
		N -> {true, S};
		_ -> false
	end.

is_cube(N, P2) -> case P2 rem 3 of 0 -> cube_mod_test(N); _ -> false end.

cube_mod_test(N) ->
	% 6 411 132 = 252 * 247 * 103
	T = N rem 6_411_132,
	case
		cube_252_test(T) andalso 
		cube_247_test(T) andalso 
		cube_103_test(T) of 
		false -> false;
		true -> is_cube_(N)
	end.

cube_252_test(T) ->
	test(T, 252, [0, 1, 8, 27, 28, 35, 36, 55, 63, 64, 71, 91, 99, 125]).

cube_247_test(T) ->
	test(T, 247, [0,1,8,12,18,26,27,31,38,39,57,64,65,77,83,96,103,122]).

cube_103_test(T) ->
	test(T, 103, [0,1,3,8,9,10,13,14,22,23,24,27,30,31,34,37,39,42]).

is_cube_(N) ->
	C = numerl:icubrt(N),
	case C * C * C of
		N -> {true, C};
		_ -> false
	end.

% when M is suitable we just need to test up to M/2.
% when testing for square we need M - 1 to be an even square
% for cubes I got them experimentally, no clear patterns
test(T, M, L) ->
	R = T rem M,
	V = min(R, M - R),
	lists:member(V, L).
