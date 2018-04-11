-module(factor).

-export([naive/1, naive/2, lehman/1, pollard/1, pollard/2, fermat/1]).
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
pollard(N) -> pollard(N, 1).

pollard(N, B) ->
	pollard(N, B, 2, 2, 2, 1, 1, 1, 0).

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

pollard(N, B, Y, X, X1, K, L, P, 19) ->
	NX = (X * X + B) rem N,
	NP = (P * (X1 - NX)) rem N,
	case numerl:gcd(N, NP) of
		1 -> pollard2(N, B, NX, NX, X1, K, L, NP, 0);
		_ -> pollard_end(N, B, Y, X1)
	end;
pollard(N, B, Y, X, X1, K, L, P, C) ->
	NX = (X * X + B) rem N,
	NP = (P * (X1 - NX)) rem N,
	pollard2(N, B, Y, NX, X1, K, L, NP, C + 1).

pollard2(N, B, Y, X, X1, 1, L, P, _) ->
 	G = numerl:gcd(N, P),
	case G of
		1 -> pollard_adv(N, B, X, X, P, L, L bsl 1);
		_ -> pollard_end(N, B, Y, X1)
	end;
pollard2(N, B, Y, X, X1, K, L, P, C) ->
	pollard(N, B, Y, X, X1, K - 1, L, P, C).

pollard_adv(N, B, X, X1, P, K, L) ->
	NX = pollard_loop(N, B, X, K),
	pollard(N, B, NX, NX, X1, K, L, P, 0).

pollard_loop(_, _, X, 0) -> X;
pollard_loop(N, B, X, K) ->
	pollard_loop(N, B, (X * X + B) rem N, K - 1).

pollard_end(N, B, Y, X1) ->
	NY = (Y * Y + B) rem N,
	case numerl:gcd(abs(X1 - Y), N) of
		1 -> pollard_end(N, B, NY, X1);
		G when G < N -> G;
		_ -> failed
	end.

fermat(N, R) ->
	case numerl:is_square(R * R - N) of
		false -> fermat(N, R + 1);
		{true, S} -> [R + S, R - S]
	end.
