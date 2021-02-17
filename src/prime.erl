% This module is a backend of the sieve module

-module(prime).

-export([init_pq/2, candidate/1, result/1, next_candidate/1, new_acc/2]).

-export([fast_next/2]).

-export([start_from/2, init_stream_pq/0, update_pq/2]).

%
% sieve callback functions
%

init_pq(N, {Fun, Acc, From}) ->
	Lim = numerl:isqrt(N),
	Wheel = wheel:init([3, 5, 7]),
	N_Acc = lists:foldl(Fun, Acc, [P || P <- [2, 3, 5, 7], P =< From]),
	sieve(pq:new(fun cur/1, fun next/1), 11, Lim, {Fun, N_Acc, From}, Wheel);
init_pq(N, Primes) ->
	Lim = numerl:isqrt(N),
	Wheel = wheel:init([3, 5, 7]),
	sieve(pq:new(fun cur/1, fun next/1), 11, Lim, Primes, Wheel).

candidate({V, _}) -> V.

result({_, Acc, _}) -> Acc;
result(Primes) when is_list(Primes) -> lists:reverse(Primes);
result(Primes) -> Primes.

next_candidate({V, Wheel}) ->
	{Inc, W2} = wheel:next(Wheel),
	{V + Inc, W2}.

new_acc(Acc, N) -> ins(N, Acc).

fast_next({Prime, Wheel, M} = E, From) when M < From ->
	Lim = 210 * Prime,
	case From - M of
		Small when Small < Lim -> fast_next(next(E), From);
		Large -> fast_next({Prime, Wheel, M + Lim * (Large div Lim)}, From)
	end;
fast_next(E, _) -> E.

%
% extra functions
%

start_from({Val, Wheel} = Lazy, From) ->
	case From - Val of
		Small when Small < 210 -> start(Lazy, From);
		Large -> start_from({Val + 210 * (Large div 210), Wheel}, From)
	end.

init_stream_pq() -> pq:new(fun cur/1, fun next/1).

update_pq(Pq, {Prime, Wheel}) ->
	pq:add(Pq, np(Prime, Wheel)).

%
% Internals
%

% In the case of primes, init_pq is by itself a sieve...
sieve(Comp, N, Lim, Primes, Wheel) when N > Lim -> {Comp, {N, Wheel}, Primes};
sieve(Comp, N, Lim, Primes, Wheel) ->
	{Inc, Wheel2} = wheel:next(Wheel),
	case pq:val(Comp) of
		% N is composite
		N -> sieve(pq:bump(Comp, N), N + Inc, Lim, Primes, Wheel2);
		% N is indeed prime we need to add the list of its multiple to Comp
		_ -> sieve(pq:add(Comp,np(N,Wheel)), N+Inc, Lim, ins(N,Primes), Wheel2)
	end.

% lazy list of composite multiples of Prime
np(Prime, Wheel) -> {Prime, Wheel, Prime * Prime}.

% head of the lazy list
cur({_, _, V}) -> V.

% tail of the lazy list
next({Prime, Wheel, M}) ->
	{Inc, W2} = wheel:next(Wheel),
	{Prime, W2, Prime * Inc + M}.

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
