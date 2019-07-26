-module(sieve).

-export([
	list/2, list/3, list/4, list/5,
	ets/3, ets/4, %ets/5, ets/6,
	foldl/4, foldl/5
	]).

list(prime, Lim) ->
	{Comp, Start, Acc} = prime:init_pq(Lim, [7, 5, 3, 2]),
	sieve(prime, Comp, Start, Lim, Acc).

list(prime, From, To) ->
	{Temp, Start, Acc} = prime:init_pq(To, [7, 5, 3, 2]),
	Start_from = prime:start_from(Start, From),
	Comp = prime:fast_bump(Temp, element(1, Start_from)),
	sieve(prime, Comp, Start_from, To,
		lists:dropwhile(fun(X) -> X < From end, Acc));
list(smooth, B, Lim) ->
	{Bad, Start, Acc} = smooth:init_pq(B, Lim),
	sieve(smooth, Bad, Start, Lim, Acc);
list(power_smooth, B, Lim) -> list(power_smooth, B, B, Lim).

list(smooth, B, From, To) ->
	{Bad, Start, Acc} = smooth:init_pq(B, {From, To}),
	sieve(smooth, Bad, Start, To, Acc);
list(power_smooth, B1, B2, Lim) ->
	{Bad, Start, Acc} = power_smooth:init_pq({B1, B2}, Lim),
	sieve(power_smooth, Bad, Start, Lim, Acc).


%list(smooth, B, B2, From, To) ->
list(power_smooth, B1, B2, From, To) ->
	{Bad, _, Acc} = power_smooth:init_pq({B1, B2}, To),
	sieve(power_smooth, Bad, From, To, Acc).

ets(prime, Lim, Tid) ->
	{Comp, Start, Acc} = prime:init_pq(Lim, Tid),
	ets:insert(Acc, [{2, y}, {3, y}, {5, y}, {7, y}]),
	sieve(prime, Comp, Start, Lim, Acc).

ets(prime, From, To, Tid) ->
	{Temp, Start, Acc} = prime:init_pq(To, Tid),
	Start_from = prime:start_from(Start, From),
	Comp = prime:fast_bump(Temp, element(1, Start_from)),
	[ets:ins(Acc, {P, y}) || P <- [2, 3, 5, 7], P =< From],
	sieve(prime, Comp, Start_from, To,
		lists:dropwhile(fun(X) -> X < From end, Acc)).
%ets(smooth, B, Lim, Tid) -> ets(smooth, B, B, Lim);
%ets(power_smooth, B, Lim, Tid) -> ets(power_smooth, B, B, Lim);

%ets(smooth, B, B2, Lim, Tid) ->
%ets(power_smooth, B, B2, Lim, Tid) ->

%ets(smooth, B, B2, From, To, Tid) ->
%ets(power_smooth, B, B2, From, To, Tid) ->

foldl(prime, Lim, Fun, Acc) -> foldl(prime, 1, Lim, Fun, Acc).
%foldl(smooth, Lim, Fun, Acc) ->
%foldl(power_smooth, Lim, Fun, Acc) ->
foldl(prime, From, To, Fun, Acc) ->
	{Comp, Start, N_Acc} = prime:init_pq(To, {Fun, Acc, From}),
	sieve(prime, Comp, Start, To, N_Acc).
%foldl(smooth, From, To, Fun, Acc) ->
%foldl(power_smooth, From, To, Fun, Acc) ->

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
