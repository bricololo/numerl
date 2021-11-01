-module(numerl).

-export([gcd/2, egcd/2, isqrt/1, icubrt/1, iroot/2, ipow/2, ipowm/3, jacobi/2]).
-export([sqrt_m/2]).

-define(N64, 16#ffffffffffffffff).

-spec gcd(A :: integer(), B :: integer()) -> integer().
% @doc
% greatest common divisor using euclid algorithm
gcd(A, B) when B >= 0, A >= B -> euclid(A, B);
gcd(A, B) when B >= 0 -> gcd(B, A);
gcd(A, B) -> gcd(A, abs(B)).

-spec egcd(A :: integer(), B :: integer()) -> {integer(), integer(), integer()}.
% @doc
% egcd(A, B) returns {D, U, V} such as: A * U + B * V = D = gcd(A, B)
egcd(0, B) -> {B, 0, 1};
egcd(A, 0) -> {A, 1, 0};
egcd(A, B) -> egcd(A, B, A, 1, 0, B).

-spec isqrt(N :: integer()) -> atom() | integer().
% @doc
% integer square root using Newton method. returns the largest integer R such
% that R * R < N + 1
isqrt(N) when N < 0 -> undefined;
isqrt(N) when N < 2 -> N;
isqrt(N) -> isqrt(N, isqrt_candidate(N)).

-spec icubrt(N :: integer()) -> integer().
% @doc
% integer cube root using Newton method. returns the largest integer R such
% that R * R * R < N + 1
icubrt(-1) -> -1;
icubrt(N) when N > - 9, N < -1 -> -2;
icubrt(N) when N > -2, N < 2 -> N;
icubrt(N) when N > 1, N < 8 -> 1;
icubrt(N) -> icubrt(N, icubrt_candidate(N)).

-spec iroot(N :: integer(), P :: integer()) -> integer().
% @doc
% integer root using Newton method. assuming N > -1 when P is even but no
% test is done, caller has to ensure it. return the largest integer R such that
% ipow(R, P) < N + 1
iroot(N, P) -> iroot(N, P, iroot_candidate(N, P)).

-spec ipow(N :: number(), P :: integer()) -> integer() | float().
% @doc
% fast exponentiation. Gives exact result when N is an integer and an
% approximation when N is a float. Note that the function can overflow if P is
% large and N is a float larger than 1.
ipow(0, 0) -> undefined;
ipow(0, _) -> 0;
ipow(_, 0) -> 1;
ipow(1, _) -> 1;
ipow(N, 1) -> N;
ipow(N, 2) -> N * N;
ipow(-1, N) when N band 1 =:= 0 -> 1;
ipow(-1, _) -> -1;
ipow(2, N) -> 1 bsl N;
ipow(Even, N) when Even band 1 =:= 0 ->
	Z = num_util:p2(Even),
	ipow(Even bsr Z, N, 1) bsl (Z * N);
ipow(N, P) -> ipow(N, P, 1).

-spec ipowm(N :: number(), P :: integer(), M :: integer()) -> integer().
% @doc
% fast modular exponentiation, returns ipow(N, P) rem M
ipowm(0, 0, _) -> undefined;
ipowm(0, _, _) -> 0;
ipowm(_, 0, _) -> 1;
ipowm(1, _, _) -> 1;
ipowm(2, P, M) when P > ?N64, M > ?N64 -> i2powm(binary:encode_unsigned(P), M, 1);
ipowm(N, P, M) -> ipowm(N, P, M, 1).

-spec jacobi(A :: integer(), M :: integer()) -> -1 | 0 | 1.
% @doc
% Jacobi-Legendre symbol (M is supposed to be odd, caller has to ensure that)
% jacobi(A, M) = 0 <=> gcd(A, M) =/= 1,
% jacobi(A, M) = 1 => A is a square mod M when M is prime
% jacobi(A, M) = -1 <=> A is not a square mod M
jacobi(A, M) -> jacobi(abs(A rem M), M, 1).

% find X such that X * X = A (P) given that P is prime and jacobi(A, P) = 1 by
% using Tonelli algorithm.
sqrt_m(V, P) ->
	A = V rem P,
	case P band 7 of
		1 -> sqrt_m_1(A, P);
		5 ->
			X = numerl:ipowm(A, P bsr 3 + 1, P),
			case X * X rem P of
				A -> X;
				_ -> X * numerl:ipowm(2, P bsr 2, P) rem P
			end;
		_ -> numerl:ipowm(A, P bsr 2 + 1, P)
	end.

%%%
%%% Internals
%%%
%%%
euclid(A, 0) -> A;
euclid(A, B) -> euclid(B, A rem B).

egcd(A, B, D, U, _, 0) when D >= 0 -> {D, U, (D - A * U) div B};
egcd(A, B, D, U, _, 0) -> {-D, -U, (A * U - D) div B};
egcd(A, B, D, U, V, R) ->
	Q = D div R,
	egcd(A, B, R, V, U - Q * V, D rem R).

ipow(N, P, R) when P band 1 =:= 1 -> ipow(N, P - 1, R * N);
ipow(_, 0, R) -> R;
ipow(N, P, R) -> ipow(N * N, P bsr 1, R).

i2powm(<<0:8, P/binary>>, M, R) -> i2powm(P, M, sqrm(R, M, 8));
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

isqrt(N, A) -> isqrt(N, A, (A + N div A) div 2).

isqrt_candidate(N) -> 1 bsl (num_util:log2_est(N) bsr 1).

isqrt(N, A, B) when abs(A - B) =< 1 ->
	case B * B of P when P > N -> B - 1; _ -> B end;
isqrt(N, _, A) -> isqrt(N, A, (A + N div A) div 2).

icubrt(N, A) -> icubrt(N, A, (N div (A * A) + (A bsl 1)) div 3).

icubrt_candidate(N) -> 1 bsl (num_util:log2_est(N) div 3).

icubrt(N, A, B) when abs(A - B) =< 1 ->
	L = max(A, B),
	case ipow(L, 3) of R when R > N -> L - 1; _ -> L end;
icubrt(N, _, A) -> icubrt(N, A, (N div (A * A) + (A bsl 1)) div 3).

iroot(N, P, A) ->
	iroot(N, P, A, ((N div ipow(A, P - 1)) + (A * (P - 1))) div P).

iroot(N, P, A, B) when abs(A - B) =< 1 ->
	L = max(A, B),
	case ipow(L, P) of R when R > N -> L - 1; _ -> L end;
iroot(N, P, _, A) ->
	iroot(N, P, A, ((N div ipow(A, P - 1)) + (A * (P - 1))) div P).

iroot_candidate(N, P) -> 1 bsl (num_util:log2_est(N) div P).

jacobi(0, 1, T) -> T;
jacobi(0, _, _) -> 0;
jacobi(A, M, T) ->
	P2 = num_util:p2(A),
	M1 = case {P2 band 1, M band 7} of {1, 3} -> -1; {1, 5} -> -1; _ -> 1 end,
	Ar = A bsr P2,
	M2 = case {Ar band 3, M band 3} of {3, 3} -> -1; _ -> 1 end,
	jacobi(M rem Ar, Ar, T * M1 * M2).

sqrt_m_1(A, P) ->
	S = num_util:p2(P - 1),
	T = P bsr S,
	D = numerl:ipowm(non_square(2, P bsr 1, P), T, P),
	M = m(1, S, 0, numerl:ipowm(A, T, P), D, P),
	(numerl:ipowm(A, (T + 1) bsr 1, P) * numerl:ipowm(D, M bsr 1, P)) rem P.


m(I, S, M, _, _, _) when I >= S -> M;
m(I, S, M, A, D, P) ->
	R = (A * numerl:ipowm(D, M, P)) rem P,
	R1 = numerl:ipowm(R, 1 bsl (S - I - 1), P),
	N = case R1 of V when V =:= P - 1 -> M + numerl:ipow(2, I); _ -> M end,
	m(I + 1, S, N, A, D, P).

non_square(Y, Lim, _) when Y > Lim -> fail;
non_square(Y, Lim, P) ->
	case numerl:jacobi(Y, P) of -1 -> Y; _ -> non_square(Y + 1, Lim, P) end.
