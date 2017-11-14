-module(numerl).

%% core number theory functions.

-export([gcd/2, egcd/2, is_square/1, isqrt/1, icubrt/1, iroot/2]).
-export([ipow/2, ipowm/3, jacobi/2]).

-define(N64, 16#ffffffffffffffff).

%% API

% greatest common divisor using euclid algorithm
gcd(A, B) ->
	AA = abs(A),
	AB = abs(B),
	case AA > AB of
		false -> abs(euclid(AB, AA));
		true -> abs(euclid(AA, AB))
	end.

% extended GCD
egcd(0, B) -> {B, 0, 1};
egcd(A, 0) -> {A, 1, 0};
egcd(A, B) -> egcd(A, B, A, 1, 0, B).

% a fast test. Try avoiding computing the square root
is_square(N) when N < 0 -> false;
is_square(N) when N < 2 -> {true, N};
is_square(N) when N band 2 =:= 2 -> false;
is_square(N) when N band 1 =:= 1 -> is_square_(N, N band 7);
is_square(N) -> is_square(N, num_util:p2(N)).

% integer square root using Newton method
isqrt(N) when N < 0 -> undefined;
isqrt(N) -> isqrt(N, isqrt_candidate(N)).

% integer cube root using Newton method
icubrt(N) -> icubrt(N, icubrt_candidate(N)).

% integer root using Newton method
iroot(N, P) -> iroot(N, P, iroot_candidate(N, P)).

% fast exponentiation
ipow(0, 0) -> undefined;
ipow(0, _) -> 0;
ipow(_, 0) -> 1;
ipow(1, _) -> 1;
ipow(N, 1) -> N;
ipow(N, 2) -> N * N;
ipow(-1, N) when N band 1 =:= 0 -> 1;
ipow(-1, _) -> -1;
ipow(2, N) -> 1 bsl N;
ipow(N, P) -> ipow(N, P, 1).

% fast modular exponentiation
ipowm(0, 0, _) -> undefined;
ipowm(0, _, _) -> 0;
ipowm(_, 0, _) -> 1;
ipowm(1, _, _) -> 1;
ipowm(2, P, M) when P > ?N64, M > ?N64 -> i2powm(binary:encode_unsigned(P), M, 1);
ipowm(N, P, M) -> ipowm(N, P, M, 1).

% Jacobi-Legendre symbol (M is supposed to be odd
jacobi(A, M) -> jacobi(abs(A rem M), M, 1).

%%%
%%% Internals
%%%
%%%
euclid(A, 0) -> A;
euclid(A, B) -> euclid(B, A rem B).

egcd(A, B, D, U, _, 0) -> {D, U, (D - A * U) div B};
egcd(A, B, D, U, V, R) ->
	Q = D div R,
	egcd(A, B, R, V, U - Q * V, D rem R).

ipow(_, 0, R) -> R;
ipow(N, P, R) when P band 1 =:= 1 -> ipow(N, P - 1, R * N);
ipow(N, P, R) -> ipow(N * N, P bsr 1, R).

i2powm(<<0:8, P/binary>>, M, R) ->
	i2powm(P, M, sqrm(R, M, 8));
i2powm(<<Y:8>>, M, R) ->
	C = num_util:p2(Y),
	F = (1 bsl (Y bsr C)) rem M,
	(R * sqrm(F, M, C)) rem M;
i2powm(<<Y:8, P/binary>>, M, R) ->
	C = num_util:p2(Y),
	F = (1 bsl (Y bsr C)) rem M,
	i2powm(P, M, sqrm((R * sqrm(F, M, C)) rem M, M, 8));
i2powm(<<>>, _, R) -> R.

sqrm(R, _, 0) -> R;
sqrm(R, M, N) -> sqrm((R * R) rem M, M, N - 1).

ipowm(_, 0, _, R) -> R;
ipowm(N, P, M, R) when P band 1 =:= 1 -> ipowm(N, P - 1, M, (R * N) rem M);
ipowm(N, P, M, R) -> ipowm((N * N) rem M, P bsr 1, M, R).

is_square(_, P2) when P2 band 1 =:= 1 -> false;
is_square(N, P2) when P2 < 57 ->
	is_square_(N, ((N band 16#fffffffffffffff) bsr P2) band 7);
is_square(N, P2) -> is_square_(N, (N bsr P2) band 7).

is_square_(N, 1) -> square_mod_test(N);
is_square_(_, _) -> false.

square_mod_test(N) ->
	T = N rem 45045,
	case square_63_test(T) of
		false -> false;
		true -> square_mod_test2(N, T)
	end.

square_mod_test2(N, T) ->
	case square_65_test(T) of
		false -> false;
		true -> square_mod_test3(N, T)
	end.

square_mod_test3(N, T) ->
	case lists:member(T rem 11, [0,1,3,4,5,9]) of
		false -> false;
		true -> is_square_(N)
	end.

square_63_test(T) ->
	lists:member(T rem 63, [0,1,4,7,9,16,18,22,25,28,36,37,43,46,49,58]).

square_65_test(T) ->
	lists:member(
		T rem 65,
		[0,1,4,9,10,14,16,25,26,29,30,35,36,39,40,49,51,55,56,61,64]).

is_square_(N) ->
	S = isqrt(N),
	case S * S of
		N -> {true, S};
		_ -> false
	end.

isqrt(N, A) -> isqrt(N, A, (A + N div A) div 2).

isqrt_candidate(N) -> 1 bsl (num_util:log2_est(N) bsr 1).

isqrt(N, A, B) when abs(A - B) =< 1 ->
	case B * B of
		P when P > N -> B - 1;
		_ -> B
	end;
isqrt(N, _, A) -> isqrt(N, A, (A + N div A) div 2).

icubrt(N, A) -> icubrt(N, A, (N div (A * A) + (A bsl 1)) div 3).

icubrt_candidate(N) -> 1 bsl (num_util:log2_est(N) div 3).

icubrt(N, A, B) when abs(A - B) =< 1 ->
	case ipow(A, 3) of
		P when P > N -> B - 1;
		_ -> B
	end;
icubrt(N, _, A) -> icubrt(N, A, (N div (A * A) + (A bsl 1)) div 3).

iroot(N, P, A) ->
	iroot(N, P, A, ((N div ipow(A, P - 1)) + (A * (P - 1))) div P).

iroot(N, P, A, B) when abs(A - B) =< 1 ->
	case ipow(A, P) of
		R when R > N -> B - 1;
		_ -> B
	end;
iroot(N, P, _, A) ->
	iroot(N, P, A, ((N div ipow(A, P - 1)) + (A * (P - 1))) div P).

iroot_candidate(N, P) -> 1 bsl (num_util:log2_est(N) div P).


jacobi(0, 1, T) -> T;
jacobi(0, _, _) -> 0;
jacobi(A, M, T) ->
	P2 = num_util:p2(A),
	M1 =
		case {P2 band 1, M band 7} of
			{1, 3} -> -1;
			{1, 5} -> -1;
			_ -> 1
		end,
	Ar = A bsr P2,
	M2 = case {Ar band 3, M band 3} of {3, 3} -> -1; _ -> 1 end,
	jacobi(M rem Ar, Ar, T * M1 * M2).
