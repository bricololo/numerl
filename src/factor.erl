-module(factor).

-export([naive/1, naive/2, fermat/1, hart/1, lehman/1, rho/1, rho/2, rho/3]).
-export([brent/1, brent/2, brent/3, pollard/2, pollard/3]).
-export([naive_list/1, naive_list/2, naive_list/3]).

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
			T = R + 1,
			element(1, split:fermat(N, T * T, 2 * T + 1));
		{true, S} -> [S, S]
	end.

hart(N) ->
	case numerl:is_square(N) of
		false ->
			B = numerl:icubrt(N),
			case naive_list(N, B, eratos:sieve(B)) of
				{B, N, []} -> split:hart(N, B);
				{_, _, [H | _]} -> H;
				[H | _] -> H
			end;
		{true, Square_root} -> Square_root
	end.

% some test values from Lehman article:
% 1123877887715932507 -> 299155897 (k=23220)
% 1129367102454866881 -> 25869889 (k=6750)
% 29742315699406748437 -> 372173423 (k=25982)
lehman(N) ->
	B = numerl:icubrt(N),
	case naive_list(N, B, primes:list(B)) of
		{B, N, []} -> lehman_odd(N, B, 1);
		{_, _, [H | _]} -> H;
		[H | _] -> H
	end.

% Pollard rho algorithm
rho(N) -> rho(N, -1).

rho(N, B) -> rho(N, B, 2).

rho(N, B, Start) ->
	Fun = fun(X) -> (X * X + B) rem N end,
	Y = Fun(Start),
	split:rho(N, Fun, Start, Y, numerl:gcd(N, abs(Y - Start))).

brent(N) -> brent(N, -1).

brent(N, B) -> brent(N, B, 2).

brent(N, B, Start) ->
	Fun = fun(X) -> (X * X + B) rem N end,
	split:brent(N, Fun, Start, 2, 1, 2).

% Pollard p-1 first stage algorithm
% TODO: what is a right default value for B ?
pollard(N, B) -> pollard(N, B, 2).

pollard(N, B, S) -> pollard(N, B, S, eratos:sieve(B), 1).


pollard(N, B, S, [H | P], L) when L * H > B -> pollard(N, B, S, P, 1);
pollard(N, B, S, [H|_] = P, L) -> pollard(N, B, numerl:ipowm(S,H,N), P, L*H);
pollard(N, B, S, [], _) ->
	case numerl:gcd(S - 1, N) of
		1 -> p_stage_2(N, B, B * 100, S);
		N -> fail;
		G -> G
	end.

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
	naive_list(N2, min(Limit, numerl:isqrt(N2)), T, Facts ++ Acc);
naive_list(N, Limit, [_ | T], Acc) -> naive_list(N, Limit, T, Acc);
naive_list(1, _, [], Acc) -> lists:reverse(Acc);
naive_list(N, Limit, [], Acc) when N < Limit * Limit -> lists:reverse([N | Acc]);
naive_list(N, Limit, [], Acc) -> {Limit, N, lists:reverse(Acc)}.

final(1, _, _, Acc) -> lists:reverse(Acc);
final(N, Limit, Factor, Acc) ->
	case Factor * Factor of
		P when P > N -> lists:reverse([N | Acc]);
		_ -> {Limit, N, lists:reverse(Acc)}
	end.

reduce(N, [F | T]) when N rem F =:= 0 -> reduce(N div F, [F, F | T]);
reduce(N, L) -> {N, L}.

lehman_odd(_, B, K) when K > B -> prime;
lehman_odd(N, B, K) ->
	Cst = 4 * K * N,
	L = numerl:isqrt(Cst),
	R = (K + N) band 3,
	case lehman_a(start_a(L, R, 4), Cst + B * B, 4, Cst) of
		nope -> lehman_even(N, B, K + 1);
		G -> numerl:gcd(N, G)
	end.

lehman_even(_, B, K) when K > B -> prime;
lehman_even(N, B, K) ->
	Cst = 4 * K * N,
	L = numerl:isqrt(Cst),
	case lehman_a(start_a(L, 1, 2), Cst + B * B, 2, Cst) of
		nope -> lehman_odd(N, B, K + 1);
		G -> numerl:gcd(N, G)
	end.

lehman_a(A, Max, _, _) when A * A > Max -> nope;
lehman_a(A, Max, Inc, Cst) ->
	case numerl:is_square(A * A - Cst) of
		false -> lehman_a(A + Inc, Max, Inc, Cst);
		{true, B} -> A + B
	end.

start_a(S, R, M) ->
	case S rem M of
		R -> S;
		V -> S + R - V
	end.
