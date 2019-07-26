-module(smooth).


-export([init_pq/2, candidate/1, result/1, next_candidate/1, new_acc/2]).

init_pq(B, {From, To}) when B * B < To ->
	Primes = sieve:list(prime, B, To),
	init(pq:new(fun cur/1, fun next/1), Primes, max(B,From));
init_pq(B, Lim) when B * B < Lim ->
	Primes = sieve:list(prime, B, Lim),
	init(pq:new(fun cur/1, fun next/1), Primes, B).

candidate(V) -> V.

result(L) when is_list(L) -> lists:reverse(L).

next_candidate(N) -> N + 1.

new_acc(Acc, V) when is_list(Acc) -> [V | Acc].

%
% internals
%

cur({V, _}) -> V.

next({V, Inc}) -> {V + Inc, Inc}.

init(Bad, [H | T], B) -> init(pq:add(Bad, {H, H}), T, B);
init(Bad, [], B) -> {Bad, B, []}.
