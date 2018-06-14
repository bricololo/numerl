-module(factor).

-export([naive/1, naive/2, fermat/1, lehman/1, rho/1, rho/2, pollard/2]).
-export([pollard/3, naive_list/1, naive_list/2, naive_list/3]).

%%%
%%% exported functions
%%%

% simply try all the prime numbers up to min(sqrt(N), 500000)
naive(2) -> [2];
naive(3) -> [3];
naive(N) -> naive(N, 500000).

% simply try all the prime numbers up to min(sqrt(N), Limit)
naive(N, Limit) -> naive_list(N, min(Limit, numerl:isqrt(N) + 1)).

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
		{B, N, []} -> lehman(N, B + 1, 1);
		{_, _, [H | _]} -> H;
		[H | _] -> H
	end.

% Pollard rho algorithm
rho(N) -> rho(N, 1).

rho(N, B) ->
	F = fun (X) -> (X * X + B) rem N end,
	Y = F(2),
	rho2(N, F, 2, Y, numerl:gcd(N, Y - 2)).

% Pollard p-1 first stage algorithm
% TODO: what is a right default value for B ?
pollard(N, B) -> pollard(N, B, 2).

pollard(N, B, S) -> pollard(N, B, S, eratos:sieve(B), 1).

pollard(N, B, S, [H | P], L) when L * H > B -> pollard(N, B, S, P, 1);
pollard(N, B, S, [H | _] = P, L) ->
	NS = numerl:ipowm(S, H, N),
	case numerl:gcd(NS - 1, N) of
		1 -> pollard(N, B, NS, P, L * H);
		N -> fail; % stage2 needed
		G -> G
	end;
pollard(N, B, S, [], _) -> p_stage_2(N, B, B * 100, S).

% TODO:
%  - right default value for B2?
%  - implement stage2
p_stage_2(N, B1, B2, S) ->
io:format("stage 2 needed:~n~p ~p ~p ~p~n", [N, B1, B2, S]).

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

lehman(_, B, K) when K > B -> prime;
lehman(N, B, K) ->
	L = 2 * numerl:isqrt(K * N),
	H = L + (numerl:iroot(N, 6) + 1) div (4 * numerl:isqrt(K)),
	case lehman_sq(N, 4 * K * N, L, H) of
		nope -> lehman(N, B, K + 1);
		G -> G
	end.

lehman_sq(_, _, A, H) when A > H -> nope;
lehman_sq(N, Cst, A, H) ->
	case numerl:is_square(A * A - Cst) of
		{true, B} -> numerl:gcd(A + B, N);
		_ -> lehman_sq(N, Cst, A + 1, H)
	end.

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
