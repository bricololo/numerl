-module(sieve).

-export([
	up_to/2, from_to/3,
	b_up_to/3, b_from_to/4,
	b1b2_from_to/5,
	ets/3, ets/4, %ets/5, ets/6,
	foldl/4, foldl/5
	]).

-export([fast_bump/3]).

% all prime numbers up to Lim
up_to(prime, Lim) ->
	{Comp, Start, Acc} = prime:init_pq(Lim, [7, 5, 3, 2]),
	sieve(prime, Comp, Start, Lim, Acc);
% all squarefree numbers up to Lim
up_to(squarefree, Lim) ->
	{Bad, Start, Acc} = squarefree:init_pq(Lim),
	sieve(squarefree, Bad, Start, Lim, Acc).

% all prime numbers between From and To
from_to(prime, To, From) when From < To -> from_to(prime, From, To);
from_to(prime, From, To) ->
	{Temp, Start, Acc} = prime:init_pq(To, [7, 5, 3, 2]),
	Start_from = prime:start_from(Start, From),
	Comp = fast_bump(prime, Temp, element(1, Start_from)),
	sieve(prime, Comp, Start_from, To,
		lists:takewhile(fun(X) -> X >= From end, Acc)).

% all B-smooth numbers up to Lim
b_up_to(smooth, B, Lim) ->
	{Bad, Start, Acc} = smooth:init_pq(B, Lim),
	sieve(smooth, Bad, Start, Lim, Acc);
% all B-powersmooth numbers up to Lim with B1=B2=B
b_up_to(power_smooth, B, Lim) -> b_from_to(power_smooth, B, B, Lim).

b_from_to(smooth, B, From, To) ->
	{Tmp, Start, Acc} = smooth:init_pq(B, {From, To}),
	Bad = fast_bump(smooth, Tmp, From),
	sieve(smooth, Bad, Start, To, Acc);
b_from_to(power_smooth, B1, B2, Lim) ->
	{Bad, Start, Acc} = power_smooth:init_pq({B1, B2}, Lim),
	sieve(power_smooth, Bad, Start, Lim, Acc).


b1b2_from_to(power_smooth, B1, B2, From, To) ->
	{Tmp, _, Acc} = power_smooth:init_pq({B1, B2}, To),
	Bad = fast_bump(power_smooth, Tmp, From),
	sieve(power_smooth, Bad, From, To, Acc).

ets(prime, Lim, Tid) ->
	{Comp, Start, Acc} = prime:init_pq(Lim, Tid),
	ets:insert(Acc, [{2, y}, {3, y}, {5, y}, {7, y}]),
	sieve(prime, Comp, Start, Lim, Acc).

ets(prime, From, To, Tid) ->
	{Temp, Start, Acc} = prime:init_pq(To, Tid),
	Start_from = prime:start_from(Start, From),
	Comp = fast_bump(prime, Temp, element(1, Start_from)),
	[ets:ins(Acc, {P, y}) || P <- [2, 3, 5, 7], P =< From],
	sieve(prime, Comp, Start_from, To,
		lists:dropwhile(fun(X) -> X < From end, Acc)).

foldl(prime, Lim, Fun, Acc) -> foldl(prime, 1, Lim, Fun, Acc).
foldl(prime, From, To, Fun, Acc) ->
	{Comp, Start, N_Acc} = prime:init_pq(To, {Fun, Acc, From}),
	sieve(prime, Comp, Start, To, N_Acc).

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
