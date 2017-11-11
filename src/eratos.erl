-module(eratos).

-export([sieve/1, sieve/2]).

% compute the list of all primes up to N
sieve(N) -> sieve(N, [7, 5, 3, 2]).

sieve(N, Primes) ->
	P_lim = numerl:isqrt(N),
	W = wheel:init([3, 5, 7]),
	case is_list(Primes) of
		true -> ok;
		_ -> ets:insert(Primes, [{2, y}, {3, y}, {5, y}, {7, y}])
	end,
	sieve(new_pq(), 11, P_lim, N, Primes, W).

% Comp is a priority queue of known composites
% N is a prime candidate
% P_lim is the maximum prime to verify
% Lim is the target number
% Primes is the (growing) list of primes found
% W is a wheel of deltas

% we don't need to add lists of composites anymore
sieve(Comp, N, P_lim, Lim, Primes, W) when N > P_lim ->
	sieve(Comp, N, Lim, Primes, W);
% sieving out the next composite
sieve(Comp, N, P_lim, Lim, Primes, W) ->
	{Inc, W2} = wheel:next(W),
	case val(Comp) of
		N -> % N is composite
			sieve(bump(Comp, N), N + Inc, P_lim, Lim, Primes, W2);
		_ -> % N is indeed prime we need to add the list of its multiple to Comp
			sieve(add(Comp, N, W), N + Inc, P_lim, Lim, ins(N, Primes), W2)
	end.

% sieving out the composites until we reach the target
sieve(_, N, Lim, Primes, _) when N > Lim, is_list(Primes) ->
	lists:reverse(Primes);
sieve(_, N, Lim, Primes, _) when N > Lim -> Primes;
sieve(Comp, N, Lim, Primes,  W) ->
	{Inc, W2} = wheel:next(W),
	case val(Comp) of
		N -> sieve(bump(Comp, N), N + Inc, Lim, Primes, W2);
		_ -> sieve(Comp, N + Inc, Lim, ins(N, Primes), W2)
	end.

new_pq() -> empty.

leaf(P) -> {P, nil, nil}.

path(N) -> path(N, []).

path(1, L) -> L;
path(N, L) -> path(N bsr 1, [N band 1 | L]).

% add a new list of composites (all multiples of Prime >= Prime squared) to the
% priority queue.
% The priority queue is managed as a heap, so:
% Creating the root of the heap
add(empty, Prime, Wheel) -> {1, leaf(np(Prime, Wheel))};
% adding the Sz + 1 element to the tree T at the position given by path/1
add({Sz, T}, Prime, Wheel) -> {Sz + 1, add(T, Prime, Wheel, path(Sz + 1))}.

% adding an element by creating an empty subtree as a leaf
add(nil, Prime, Wheel, []) -> leaf(np(Prime, Wheel));
% going through tre Left (resp. Right) subtree until reaching the missing leaf
add({P, L, R}, Prime, Wheel, [0 | Path]) -> {P, add(L, Prime, Wheel, Path), R};
add({P, L, R}, Prime, Wheel, [1 | Path]) -> {P, L, add(R, Prime, Wheel, Path)}.

val(empty) -> nothing;
val({P, _, _}) -> cur(P);
val({_, {P, _, _}}) -> cur(P).

% initialize the list of composites multiple of Prime as a lazy list
np(Prime, Wheel) -> {Prime, Wheel, Prime * Prime}.

% give the next element of the lazy list
next({Prime, Wheel, M}) ->
	{Inc, W2} = wheel:next(Wheel),
	{Prime, W2, Prime  * Inc + M}.

% value of the lazy list
cur({_, _, V}) -> V.

% remove the composite N from the tree T of size Sz, the size of the tree will
% not change as a composite removed is replaced by a bigger composite.
bump({Sz, T}, N) -> {Sz, bumpt(T, N)}.

% a composite might be present several times (for example 2431 will be present
% 3 times, as it is multiple of 11, 13 and 17). So we remove V from T until the
% smallest composite in T is larger than V.
bumpt({P, L, R} = T, V) ->
	case cur(P) of
		Vp when Vp =< V -> bumpt(fix({next(P), L, R}), V);
		_ -> T
	end.

% adjust heap by swapping primes down until it's a heap again
fix({_, nil, nil} = T) -> T;
fix({P, {Pl, nil, nil}, nil} = T) ->
	case {cur(P), cur(Pl)} of
		{V, Vl} when V < Vl -> T;
		_ -> {Pl, {P, nil, nil}, nil}
	end;
fix({P, {Pl, Ll, Rl} = L, {Pr, Lr, Rr} = R} = T) ->
	case {cur(P), cur(Pl), cur(Pr)} of
		{V, Vl, Vr} when V < Vl, V < Vr -> T;
		{_, Vl, Vr} when Vl < Vr -> {Pl, fix({P, Ll, Rl}), R};
		_ -> {Pr, L, fix({P, Lr, Rr})}
	end.

ins(N, Primes) when is_list(Primes) -> [N | Primes];
ins(N, Primes) ->
	ets:insert(Primes, {N, y}),
	Primes.
