-module(smooth).


-export([init_pq/2, candidate/1, result/1, next_candidate/1, new_acc/2]).
-export([fast_bump/2]).

init_pq(B, {From, To}) when B < To ->
	Primes = sieve:list(prime, B, To),
	init(pq:new(fun cur/1, fun next/1), Primes, From);
init_pq(B, Lim) when B < Lim ->
	Primes = sieve:list(prime, B, Lim),
	init(pq:new(fun cur/1, fun next/1), Primes, 2).

candidate(V) -> V.

result(L) when is_list(L) -> lists:reverse(L).

next_candidate(N) -> N + 1.

new_acc(Acc, V) when is_list(Acc) -> [V | Acc].

fast_bump({Cur, _, _, Heap} = Bad, From) ->
	case pq:val(Bad) of
		Large when Large >= From -> Bad;
		Small ->
			Fast_next = fun(X) -> fast_next(X, From) end,
			N_bad = setelement(4, Bad, pq:bumpt(Heap, Small, Cur, Fast_next)),
			fast_bump(N_bad, From)
	end.

%
% internals
%

cur({V, _}) -> V.

next({V, Inc}) -> {V + Inc, Inc}.

fast_next({V, Inc}, From) when V < From ->
	case From - V of
		Small when Small < Inc -> {V + Inc, Inc};
		Large -> {V + Inc * (Large div Inc), Inc}
	end;
fast_next(E, _) -> E.

init(Bad, [H | T], B) -> init(pq:add(Bad, {H, H}), T, B);
init(Bad, [], B) -> {Bad, B, []}.
