% This module is a backend of the sieve module

-module(prime).

-export([init/2, new_acc/2, result/1]).
-export([candidate/1, next_candidate/1]).
-export([bad/1, next_bad/2]).

-export([fast_next/2, fast_bump/3]).
-export([start_from/2, init_stream_pq/0, update_pq/2]).

%
% sieve callback functions
%

init(N, {Fun, Acc, From}) ->
	Lim = numerl:isqrt(N),
	Wheel = wheel:init([3, 5, 7]),
	N_Acc = lists:foldl(Fun, Acc, [P || P <- [2, 3, 5, 7], P =< From]),
	sieve(pq_skew:new(fun next/1), 11, Lim, {Fun, N_Acc, From}, Wheel);
init(N, Primes) ->
	Lim = numerl:isqrt(N),
	Wheel = wheel:init([3, 5, 7]),
	sieve(pq_skew:new(fun next/1), 11, Lim, Primes, Wheel).

bad(Bad) -> pq_skew:val(Bad).

next_bad(Bad, Value) -> pq_skew:bump(Bad, Value).

candidate({V, _}) -> V.

next_candidate({V, Wheel}) ->
	{Inc, W2} = wheel:next(Wheel),
	{V + Inc, W2}.

new_acc(Acc, N) -> ins(N, Acc).

result({_, Acc, _}) -> Acc;
result(Primes) when is_list(Primes) -> lists:reverse(Primes);
result(Primes) -> Primes.

fast_next({Mult, Prime, Wheel} = E, From) when Mult < From ->
	Lim = 210 * Prime,
	case From - Mult of
		Small when Small < Lim -> fast_next(next(E), From);
		Large -> fast_next({Mult + Lim * (Large div Lim), Prime, Wheel}, From)
	end;
fast_next(E, _) -> E.

fast_bump(Tree, Value, Next) -> pq_skew:bumpt(Tree, Value, Next).

%
% extra functions
%

start_from({Val, Wheel} = Lazy, From) ->
	case From - Val of
		Small when Small < 210 -> start(Lazy, From);
		Large -> start_from({Val + 210 * (Large div 210), Wheel}, From)
	end.

init_stream_pq() -> pq_skew:new(fun next/1).

update_pq(Pq, {Prime, Wheel}) ->
	pq_skew:add(Pq, np(Prime, Wheel)).

%
% Internals
%

% In the case of primes, init is by itself a sieve...
sieve(Comp, N, Lim, Primes, Wheel) when N > Lim -> {Comp, {N, Wheel}, Primes};
sieve(Comp, N, Lim, Primes, Wheel) ->
	{Inc, Wheel2} = wheel:next(Wheel),
	case pq_skew:val(Comp) of
		% N is composite
		N -> sieve(pq_skew:bump(Comp, N), N + Inc, Lim, Primes, Wheel2);
		% N is indeed prime we need to add the list of its multiple to Comp
		_ -> sieve(pq_skew:add(Comp,np(N,Wheel)),N+Inc,Lim,ins(N,Primes),Wheel2)
	end.

% lazy list of composite multiples of Prime
np(Prime, Wheel) -> {Prime * Prime, Prime, Wheel}.

% tail of the lazy list
next({Mult, Prime, Wheel}) ->
	{Inc, Wheel2} = wheel:next(Wheel),
	{Mult + Inc * Prime, Prime, Wheel2}.

% Acc handling
ins(N, {_, _, From} = Result) when N < From -> Result;
ins(N, {Fun, Acc, From}) -> {Fun, Fun(N, Acc), From};
ins(N, Primes) when is_list(Primes) -> [N | Primes];
ins(N, Primes) ->
	ets:insert(Primes, {N}),
	Primes.

start({Val, _} = Start, Lim) when Val >= Lim -> Start;
start({Val, Wheel}, Lim) ->
	{Inc, Wheel2} = wheel:next(Wheel),
	start({Val + Inc, Wheel2}, Lim).
