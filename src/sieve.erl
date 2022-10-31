-module(sieve).

% This module implement a generic sieving using a lazy list of candidates and a
% priority queue of exceptionnal values that have to be sieved out.

% a callback module needs to implement and export the following functions:
%	init/2 to initialise the lazy generator of rejected numbers, the lazy
%		generator of candidates and the accumulator that will store the result;
%	candidate/1 to get the current value of the candidates generator;
%   next_candidate/1 to get the next state of the candidates generator;
%	bad/1 to get the current value of the rejected generator;
%	next_bad/2 to get the next state of the rejected generator;
%	new_acc/2 to add an element to the accumulator.
%	result/1 to extract the final result from the sieve;
%
% moreover, in order to be able to start the sieving from an arbitrary point the
% two following callback functions are also needed:
%	start_from/2 to initialize candidates to a suitable value
%	fast_next/2 to initialize the heap to a suitable state without having to do
%		a gazillion rebalancing

% note also that the init/2 callback function will need to provide the Next
% function to pq_*:new/1, as it is passed as a closure it does not require to
% be exported by the callback module.

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
	{Composites, Start, Acc} = Module:init(Lim, [7, 5, 3, 2]),
	sieve(Module, Composites, Start, Lim, Acc);
% all squarefree numbers up to Lim
up_to(squarefree = Module, Lim) ->
	{Bad, Start, Acc} = Module:init(Lim),
	sieve(Module, Bad, Start, Lim, Acc).

% all prime numbers between From and To
from_to(prime, To, From) when From < To -> from_to(prime, From, To);
from_to(prime = Module, From, To) ->
	{Temp, Start, Acc} = Module:init(To, [7, 5, 3, 2]),
	Start_from = Module:start_from(Start, From),
	Composites = fast_bump(Module, Temp, element(1, Start_from)),
	sieve(Module, Composites, Start_from, To,
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
	{Comp, Start, Acc} = Module:init(Lim, Tid),
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
	{Comp, Start, N_Acc} = Module:init(To, {Fun, Acc, From}),
	sieve(Module, Comp, Start, To, N_Acc).

stream(prime = Module) ->
	Bad = Module:init_stream_pq(),
	Wheel = wheel:init([3, 5, 7]),
	stream(Module, Bad, {11, Wheel}).

stream(prime = Module, Bad, V) ->
	Candidate = Module:candidate(V),
	N_Cand = Module:next_candidate(V),
	case Module:bad(Bad) of
		Candidate -> stream(Module, Module:next_bad(Bad, Candidate), N_Cand);
		_ -> {Candidate, Module, Module:update_pq(Bad, V), N_Cand}
	end.

%%
%% Implementation
%%

sieve(Module, Bad, V, Lim, Acc) ->
	Candidate = Module:candidate(V),
	candidate(Module, Bad, V, Lim, Candidate, Acc).

fast_bump(Module, {_, Heap} = Bad, From) ->
	case Module:bad(Bad) of
		Large when Large >= From -> Bad;
		Small ->
			Fast_next = fun(X) -> Module:fast_next(X, From) end,
			N_bad = setelement(2, Bad, Module:fast_bump(Heap, Small, Fast_next)),
			fast_bump(Module, N_bad, From)
	end.

candidate(Module, Bad, V, Lim, Cand, Acc) when Cand =< Lim ->
	N_Cand = Module:next_candidate(V),
	{N_Bad, N_Candidate, N_Acc} =
		next_step_vals(Module, Bad, V, Cand, N_Cand, Module:bad(Bad), Acc),
	sieve(Module, N_Bad, N_Candidate,  Lim, N_Acc);
candidate(Module, _, _, _, _, Acc) -> Module:result(Acc).

next_step_vals(Module, Bad, _, Cand, N_Cand, Bad_Val, Acc) when Bad_Val > Cand ->
	N_Acc = Module:new_acc(Acc, Cand),
	{Bad, N_Cand, N_Acc};
next_step_vals(Module, Bad, _, Cand, N_Cand, Bad_Val, Acc) when Bad_Val =:= Cand ->
	{Module:next_bad(Bad, Cand), N_Cand, Acc};
next_step_vals(Module, Bad, V, Cand, _, _, Acc) ->
	{Module:next_bad(Bad, Cand - 1), V, Acc}.
