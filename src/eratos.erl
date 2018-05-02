-module(eratos).

-export_type([tab/0, tid/0]).

-type tab() :: atom() | tid().
-opaque tid() :: reference().

-export([sieve/1, sieve/2, foldl/3]).

-spec sieve(N :: integer()) -> [integer()].
% @doc
% compute the list of all primes up to N
sieve(N) -> sieve(N, [7, 5, 3, 2]).

-spec sieve(N :: integer, P :: [integer()] | tab()) -> [integer()] | tab().
% @doc
% returns all the primes up to N either as a list or as an ets table according
% to the type of the second argument.
sieve(N, Primes) ->
	P_lim = numerl:isqrt(N),
	W = wheel:init([3, 5, 7]),
	case is_list(Primes) of
		true -> ok;
		_ -> ets:insert(Primes, [{2, y}, {3, y}, {5, y}, {7, y}])
	end,
	sieve(pq:new(fun cur/1, fun next/1), 11, P_lim, N, Primes, W).

-spec foldl(N :: pos_integer(), Fun :: function, Acc :: term()) -> term().
% @doc
% equivalent to lists:foldl(Fun, Acc, sieve(N)) but the list of primes is not
% built
foldl(N, Fun, Acc) ->
	P_lim = numerl:isqrt(N),
	W = wheel:init([3, 5, 7]),
	N_acc = lists:foldl(Fun, Acc, [2, 3, 5, 7]),
	sieve(pq:new(fun cur/1, fun next/1), 11, P_lim, N, {Fun, N_acc}, W).

%%%
%%% internals
%%%

% Comp is a sorted heap of known composites
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

% lazy list of composites multiple of Prime
np(Prime, Wheel) -> {Prime, Wheel, Prime * Prime}.

% head of the lazy list
cur({_, _, V}) -> V.

% tail of the lazy list
next({Prime, Wheel, M}) ->
	{Inc, W2} = wheel:next(Wheel),
	{Prime, W2, Prime * Inc + M}.
