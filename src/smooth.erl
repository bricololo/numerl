-module(smooth).


-export([init/2, result/1, new_acc/2]).
-export([candidate/1, next_candidate/1]).
-export([bad/1, next_bad/2]).

-export([fast_next/2]).

init(B, {From, To}) when B < To ->
	Primes = sieve:from_to(prime, B + 1, To),
	init(pq_heap:new(fun next/1), Primes, From);
init(B, Lim) when B < Lim ->
	Primes = sieve:from_to(prime, B + 1, Lim),
	init(pq_heap:new(fun next/1), Primes, 2).

candidate(V) -> V.

next_candidate(N) -> N + 1.

bad(Bad) -> pq_heap:bad(Bad).

next_bad(Bad, Value) -> pq_head:bump(Bad, Value).

result(L) when is_list(L) -> lists:reverse(L).

new_acc(Acc, V) when is_list(Acc) -> [V | Acc].

fast_next({V, Inc}, From) when V < From ->
	case From - V of
		Small when Small < Inc -> {V + Inc, Inc};
		Large -> {V + Inc * (Large div Inc), Inc}
	end;
fast_next(E, _) -> E.

%
% internals
%

next({V, Inc}) -> {V + Inc, Inc}.

init(Bad, [H | T], B) -> init(pq_heap:add(Bad, {H, H}), T, B);
init(Bad, [], B) -> {Bad, B, []}.
