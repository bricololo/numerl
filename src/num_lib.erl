-module(num_lib).

%-compile(export_all).
-export([is_square/1, is_cube/1]).

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
	% 4 382 217 840 = 208 * 231 * 145 * 37 * 17
	T = N rem 4_382_217_840,
	case 
		square_208_test(T) andalso
		square_231_test(T) andalso
		square_145_test(T) andalso
		square_37_test(T) andalso
		square_17_test(T) of
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

square_145_test(T) ->
	test(T, 145,
		[0,1,4,5,6,9,16,20,24,25,29,30,34,35,36,45,49,51,54,59,64,65,71]).

square_37_test(T) -> test(T, 37, [0, 1, 3, 4, 7, 9, 10, 11, 12, 16]).

square_17_test(T) -> test(T, 17, [0, 1, 2, 4, 8]).

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
