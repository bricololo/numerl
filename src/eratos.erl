-module(eratos).

% A specialised sieve dealing only with primes, it relies on tricks (such as
% using large wheels) to be slightly faster than the implementation of the sieve
% module.

-export_type([tab/0, tid/0]).

-type tab() :: atom() | tid().
-opaque tid() :: reference().

-export([sieve/1, sieve/2, foldl/3]).

-spec sieve(N :: integer()) -> [integer()].
% @doc
% compute the list of all primes up to N
sieve(N) -> sieve(N, primes_list(N, N)).

-spec sieve(N :: integer(), P :: [integer()] | tab()) -> [integer()] | tab().
% @doc
% returns all the primes up to N either as a list or as an ets table according
% to the type of the second argument.
sieve(N, Primes) ->
	P_lim = numerl:isqrt(N),
	P_list = primes_list(Primes, N),
	W = wheel:init(tl(lists:reverse(P_list))),
	init(Primes, P_list),
	sieve(pq_heap:new(fun next/1), first(P_list), P_lim, N, Primes, W).

-spec foldl(N :: pos_integer(), Fun :: fun((_, _) -> term()), Acc :: term()) -> term().
% @doc
% equivalent to lists:foldl(Fun, Acc, sieve(N)) but the list of primes is not
% built
foldl(N, Fun, Acc) ->
	P_lim = numerl:isqrt(N),
	W = wheel:init([3, 5, 7]),
	N_acc = lists:foldl(Fun, Acc, [2, 3, 5, 7]),
	sieve(pq_heap:new(fun next/1), 11, P_lim, N, {Fun, N_acc}, W).

%%%
%%% internals
%%%

primes_list(List, _) when is_list(List) -> List;
primes_list(_, N) when N =< 1_000 -> [7, 5, 3, 2];
primes_list(_, N) when N =< 10_000 -> [11, 7, 5, 3, 2];
%primes_list(_, N) when N =< 100_000 -> [13, 11, 7, 5, 3, 2];
primes_list(_, N) when N =< 1_000_000 -> [13, 11, 7, 5, 3, 2];
primes_list(_, N) when N =< 10_000_000 -> [17, 13, 11, 7, 5, 3, 2];
primes_list(_, N) when N =< 100_000_000 -> [19, 17, 13, 11, 7, 5, 3, 2];
primes_list(_, _) -> [7, 5, 3, 2]. % small wheel to limit swapping

init(Primes, _) when is_list(Primes) -> ok;
init(Primes, P_list) -> ets:insert(Primes, [{P} || P <- P_list]).

first([23 | _]) -> 29;
first([H | _]) when H < 31 -> H+5-(H rem 6+1) div 2.

% Comp is a priority queue of known composites
% N is a prime candidate
% P_lim is the maximum prime to verify
% Lim is the target number
% Primes is the (growing) list/ets table of primes found or the accumulated
% result of the provided function so far in case of foldl
% Wheel is a wheel of deltas

% we don't need to add lists of composites anymore
sieve(Comp, N, P_lim, Lim, Primes, Wheel) when N > P_lim ->
	sieve(Comp, N, Lim, Primes, Wheel);
% sieving out the next composite
sieve(Comp, N, P_lim, Lim, Primes, Wheel) ->
	{N_comp, N_primes} = if_prime(Comp, N, Primes, Wheel),
	{Inc, Wheel2} = wheel:next(Wheel),
	sieve(N_comp, N+Inc, P_lim, Lim, N_primes, Wheel2).

if_prime(Comp, N, Primes, Wheel) ->
	if_prime(Comp, N, Primes, Wheel, pq_heap:val(Comp)).

if_prime(Comp, N, Primes, _, N) -> % N is composite
	{pq_heap:bump(Comp, N), Primes};
if_prime(Comp, N, Primes, Wheel, _) -> % N is prime
	{pq_heap:add(Comp, np(N, Wheel)), ins(N, Primes)}.

% sieving out the composites until we reach the target
sieve(Comp, N, Lim, Primes, Wheel) when N =< Lim ->
	{N_comp, N_primes} = if_prime(Comp, N, Primes, Wheel),
	{Inc, Wheel2} = wheel:next(Wheel),
	sieve(N_comp, N+Inc, Lim, N_primes, Wheel2);
sieve(_, _, _, {_, Acc}, _) -> Acc;
sieve(_, _, _, Primes, _) when is_list(Primes) -> lists:reverse(Primes);
sieve(_, _, _, Primes, _) -> Primes.

ins(N, {Fun, Acc}) -> {Fun, Fun(N, Acc)};
ins(N, Primes) when is_list(Primes) -> [N | Primes];
ins(N, Primes) ->
	ets:insert(Primes, {N}),
	Primes.

% lazy list of composites multiple of Prime
np(Prime, Wheel) -> {Prime*Prime, Wheel, Prime}.

% tail of the lazy list
next({Mult, Wheel, Prime}) ->
	{Inc, W2} = wheel:next(Wheel),
	{Prime*Inc+Mult, W2, Prime}.
