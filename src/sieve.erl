-module(sieve).

% This module implement a generic sieving using a lazy list of candidates and a
% priority queue of exceptionnal values that have to be sieved out.

% a callback module needs to implement and export the following functions:
%	init_pq/2 to initialise the priority queue of rejected numbers as a heap of
%		lawy lists;
%	candidate/1 to get the current value of the lazy list;
%	result/1 to extract the final result from the sieve;
%   next_candidate/1 to get the next state of the lazy list of candidates;
%	new_acc/2 to add an element to the accumulator.
% moreover, in order to be able to start the sieving from an arbitrary point the
% two following callback functions are also needed:
%	start_from/2 to initialize candidates to a suitable value
%	fast_next/2 to initialize the heap to a suitable state without having to do
%		a gazillion rebalancing

% note also that the init_pq/2 callback function will need to provide the Cur
% and Next functions to pq:new/2, as they are passed as a closure they do not
% require to be exported by the callback module.

-export([
	up_to/2, from_to/3,
	b_up_to/3, b_from_to/4,
	b1b2_up_to/4, b1b2_from_to/5,
	ets/3, ets/4, %ets/5, ets/6,
	foldl/4, foldl/5,
	stream/1, stream/3
	]).

% all prime numbers up to Lim
up_to(prime = Module, Lim) ->
	{Comp, Start, Acc} = Module:init_pq(Lim, [7, 5, 3, 2]),
	sieve(Module, Comp, Start, Lim, Acc);
% all squarefree numbers up to Lim
up_to(squarefree = Module, Lim) ->
	{Bad, Start, Acc} = Module:init_pq(Lim),
	sieve(Module, Bad, Start, Lim, Acc).

% all prime numbers between From and To
from_to(prime, To, From) when From < To -> from_to(prime, From, To);
from_to(prime = Module, From, To) ->
	{Temp, Start, Acc} = Module:init_pq(To, [7, 5, 3, 2]),
	Start_from = Module:start_from(Start, From),
	Comp = fast_bump(Module, Temp, element(1, Start_from)),
	sieve(Module, Comp, Start_from, To,
		lists:takewhile(fun(X) -> X >= From end, Acc)).

% all B-smooth numbers up to Lim
b_up_to(smooth = Module, B, Lim) ->
	{Bad, Start, Acc} = Module:init_pq(B, Lim),
	sieve(Module, Bad, Start, Lim, Acc);
% all B-powersmooth numbers up to Lim with B1=B2=B
b_up_to(power_smooth, B, Lim) -> b_from_to(power_smooth, B, B, Lim).

% B-smooth numbers between From and To
b_from_to(Module, B, From, To) when
		Module =:= smooth orelse Module =:= power_smooth ->
	{Tmp, Start, Acc} = Module:init_pq(B, {From, To}),
	Bad = fast_bump(Module, Tmp, From),
	sieve(Module, Bad, Start, To, Acc).

% B1,B2-powersmooth numbers up to Lim
b1b2_up_to(power_smooth = Module, B1, B2, Lim) ->
	{Bad, Start, Acc} = Module:init_pq({B1, B2}, Lim),
	sieve(Module, Bad, Start, Lim, Acc).

% B1,B2-powersmooth numbers between From and To
b1b2_from_to(power_smooth = Module, B1, B2, From, To) ->
	{Tmp, _, Acc} = Module:init_pq({B1, B2}, To),
	Bad = fast_bump(Module, Tmp, From),
	sieve(Module, Bad, From, To, Acc).

% all primes up to Lim in a ets table
% TODO: store something more useful than {P}...
ets(prime = Module, Lim, Tid) ->
	{Comp, Start, Acc} = Module:init_pq(Lim, Tid),
	ets:insert(Acc, [{2}, {3}, {5}, {7}]),
	sieve(Module, Comp, Start, Lim, Acc).

% primes between From and To in a ets table
ets(prime = Module, From, To, Tid) ->
% unfortunately, the primes below sqrt(To) are inserted in the table :-(
%	{Temp, Start, Acc} = prime:init_pq(To, Tid),
%	Start_from = prime:start_from(Start, From),
%	Comp = fast_bump(prime, Temp, element(1, Start_from)),
%	[ets:insert(Acc, {P}) || P <- [2, 3, 5, 7], P >= From],
%	sieve(prime, Comp, Start_from, To, Acc).
	Fun =
		fun
			(P, Acc) when P < From -> Acc;
			(P, Acc) ->
				ets:insert(Acc, {P}),
				Acc
		end,
	foldl(Module, From, To, Fun, Tid).

foldl(prime, Lim, Fun, []) -> foldl(prime, 1, Lim, Fun, [7, 5, 3, 2]);
foldl(prime, Lim, Fun, Acc) -> foldl(prime, 1, Lim, Fun, Acc).

foldl(prime = Module, From, To, Fun, Acc) ->
	{Comp, Start, N_Acc} = Module:init_pq(To, {Fun, Acc, From}),
	sieve(Module, Comp, Start, To, N_Acc).

stream(prime = Module) ->
	Bad = Module:init_stream_pq(),
	Wheel = wheel:init([3, 5, 7]),
	stream(Module, Bad, {11, Wheel}).

sieve(Module, Bad, V, Lim, Acc) ->
	Candidate = Module:candidate(V),
	case Candidate =< Lim of
		false -> Module:result(Acc);
		true ->
			N_Cand = Module:next_candidate(V),
			case pq:val(Bad) of
				Candidate ->
					sieve(Module, pq:bump(Bad, Candidate), N_Cand, Lim, Acc);
				_ ->
					N_Acc = Module:new_acc(Acc, Candidate),
					sieve(Module, Bad, N_Cand, Lim, N_Acc)
			end
	end.

fast_bump(Module, {Cur, _, _, Heap} = Bad, From) ->
	case pq:val(Bad) of
		Large when Large >= From -> Bad;
		Small ->
			Fast_next = fun(X) -> Module:fast_next(X, From) end,
			N_bad = setelement(4, Bad, pq:bumpt(Heap, Small, Cur, Fast_next)),
			fast_bump(Module, N_bad, From)
	end.

stream(prime = Module, Bad, V) ->
	Candidate = Module:candidate(V),
	N_Cand = Module:next_candidate(V),
	case pq:val(Bad) of
		Candidate -> stream(Module, pq:bump(Bad, Candidate), N_Cand);
		_ -> {Candidate, Module, Module:update_pq(Bad, V), N_Cand}
	end.
