-module(factor).

-export([naive/1, naive/2, fermat/1, lehman/1, rho/1, rho/2]).
-export([naive_list/1, naive_list/2, naive_list/3]).

%%%
%%% exported functions
%%%

% simply try all the prime numbers up to min(sqrt(N), 500000)
naive(2) -> [2];
naive(3) -> [3];
naive(N) -> naive(N, 500000).

% simply try all the prime numbers up to min(sqrt(N), Limit)
naive(N, Limit) -> naive_list(N, min(Limit, numerl:isqrt(N) +1)).

% equivalent to naive/1
naive_list(N) -> naive_list(N, 500000).

% equivalent to naive/2
naive_list(N, Limit) -> naive_list(N, Limit, primes:list(Limit)).

% used both as backend of naive/1 and naive/2 but can also be used to check
% against a factor base.
naive_list(N, Limit, Primes) -> naive_list(N, Limit, Primes, []).

% just for curiosity, lehman is usually faster
fermat(N) ->
	case numerl:is_square(N) of
		false ->
			R = numerl:isqrt(N),
			fermat(N, R + 1);
		{true, S} -> [S, S]
	end.

lehman(N) ->
	B = numerl:icubrt(N),
	case naive_list(N, B, primes:list(B)) of
		{B, N, []} -> lehman2(N, B, 1);
		{_, _, [H | _]} -> H;
		[H | _] -> H
	end.

% Pollard rho algorithm
rho(N) -> rho(N, -1).

rho(N, B) ->
	F = fun (X) -> (X * X + B) rem N end,
	Y = F(2),
	rho2(N, F, 2, Y, numerl:gcd(N, Y - 2)).

%%%
%%% implementation
%%%

naive_list(N, Limit, [H | _], Acc) when H > Limit -> final(N, Limit, H, Acc);
naive_list(N, Limit, [H | T], Acc) when N rem H =:= 0 ->
	{N2, Facts} = reduce(N div H, [H]),
	naive_list(N2, Limit, T, Facts ++ Acc);
naive_list(N, Limit, [_ | T], Acc) -> naive_list(N, Limit, T, Acc);
naive_list(1, _, [], Acc) -> lists:reverse(Acc);
naive_list(N, Limit, [], Acc) when N < Limit * Limit -> lists:reverse([N | Acc]);
naive_list(N, Limit, [], Acc) -> {Limit, N, lists:reverse(Acc)}.

final(1, _, _, Acc) -> lists:reverse(Acc);
final(N, Limit, Factor, Acc) ->
	case Factor * Factor of
		P when P > N, N =/= 1 -> lists:reverse([N | Acc]);
		_ -> {Limit, N, lists:reverse(Acc)}
	end.

reduce(N, [F | T]) when N rem F =:= 0 -> reduce(N div F, [F, F | T]);
reduce(N, L) -> {N, L}.

fermat(N, R) ->
	case numerl:is_square(R * R - N) of
		false -> fermat(N, R + 1);
		{true, S} -> [R + S, R - S]
	end.

lehman2(_, B, K) when K > B -> prime;
lehman2(N, B, K) ->
	{R, M} = case K band 1 of 0 -> {1, 2}; _ -> {K + N, 4} end,
	lehman3(N, B, K, R, M).

lehman3(N, B, K, R, M) ->
	T = 4 * K * N,
	lehman4(N, B, K, R, M, numerl:isqrt(T) + 1, T, T + B * B).

lehman4(N, B, K, _, _, A, _, Lim) when A * A > Lim -> lehman2(N, B, K + 1);
lehman4(N, B, K, R, M, A, T, Lim) when A rem M =:= R ->
	C = A * A - T,
	case numerl:is_square(C) of
		{true, S} -> numerl:gcd(N, A + S);
		false -> lehman4(N, B, K, R, M, A + M, T, Lim)
	end;
lehman4(N, B, K, R, M, A, T, Lim) -> lehman4(N, B, K, R, M, A + 1, T, Lim).

rho(N, F, X, Y, 1) ->
	U = F(X),
	V = F(F(Y)),
	rho(N, F, U, V, numerl:gcd(N, U - V));
rho(N, _, _, _, N) -> fail;
rho(_, _, _, _, G) -> G.

% rho but grouping 50 iterations before taking a GCD to improve speed sligthly
rho2(N, F, X, Y, G) ->
	case rho2(N, F, X, Y, G, 50) of
		{1, U, V} -> rho2(N, F, U, V, 1, 50);
		{D, U, V} ->
			case numerl:gcd(N, D) of
				1 -> rho2(N, F, U, V, 1);
				_ -> rho(N, F, X, Y, 1)
			end
	end.

rho2(_, _, X, Y, A, 0) -> {A, X, Y};
rho2(N, F, X, Y, A, C) ->
	U = F(X),
	V = F(F(Y)),
	rho2(N, F, U, V, (A * abs(U - V)) rem N, C - 1).
