-module(eratos).

-export([sieve/1, sieve/2, foldl/3]).

% compute the list of all primes up to N
sieve(N) -> sieve(N, [7, 5, 3, 2]).

sieve(N, Primes) ->
	P_lim = numerl:isqrt(N),
	W = wheel:init([3, 5, 7]),
	case is_list(Primes) of
		true -> ok;
		_ -> ets:insert(Primes, [{2, y}, {3, y}, {5, y}, {7, y}])
	end,
	sieve(pq:new(fun cur/1, fun next/1), 11, P_lim, N, Primes, W).

foldl(N, Fun, Acc) ->
	P_lim = numerl:isqrt(N),
	W = wheel:init([3, 5, 7]),
	N_acc = lists:foldl(Fun, Acc, [2, 3, 5, 7]),
	sieve(pq:new(fun cur/1, fun next/1), 11, P_lim, N, {Fun, N_acc}, W).

% Comp is a priority queue of known composites
% N is a prime candidate
% P_lim is the maximum prime to verify
% Lim is the target number
% Primes is the (growing) list/ets table of primes found or the accumulated
% result of the provided function so far in case of foldl
% W is a wheel of deltas

% we don't need to add lists of composites anymore
sieve(Comp, N, P_lim, Lim, Primes, W) when N > P_lim ->
	sieve(Comp, N, Lim, Primes, W);
% sieving out the next composite
sieve(Comp, N, P_lim, Lim, Primes, W) ->
	{Inc, W2} = wheel:next(W),
	case pq:val(Comp) of
		% N is composite
		N -> sieve(pq:bump(Comp, N), N + Inc, P_lim, Lim, Primes, W2);
		% N is indeed prime we need to add the list of its multiple to Comp
		_ -> sieve(pq:add(Comp,np(N,W)), N+Inc, P_lim, Lim, ins(N,Primes), W2)
	end.

% sieving out the composites until we reach the target
sieve(_, N, Lim, {_, Acc}, _) when N > Lim -> Acc;
sieve(_, N, Lim, Primes, _) when N > Lim, is_list(Primes) ->
	lists:reverse(Primes);
sieve(_, N, Lim, Primes, _) when N > Lim -> Primes;
sieve(Comp, N, Lim, Primes, W) ->
	{Inc, W2} = wheel:next(W),
	case pq:val(Comp) of
		N -> sieve(pq:bump(Comp, N), N + Inc, Lim, Primes, W2);
		_ -> sieve(Comp, N + Inc, Lim, ins(N, Primes), W2)
	end.

ins(N, {Fun, Acc}) -> {Fun, Fun(N, Acc)};
ins(N, Primes) when is_list(Primes) -> [N | Primes];
ins(N, Primes) ->
	ets:insert(Primes, {N, y}),
	Primes.

% initialize the list of composites multiple of Prime as a lazy list
np(Prime, Wheel) -> {Prime, Wheel, Prime * Prime}.

% value of the lazy list
cur({_, _, V}) -> V.

% give the next element of the lazy list
next({Prime, Wheel, M}) ->
	{Inc, W2} = wheel:next(Wheel),
	{Prime, W2, Prime * Inc + M}.
