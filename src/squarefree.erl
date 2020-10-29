-module(squarefree).

-export([init_pq/1, init_pq/2, candidate/1, result/1, next_candidate/1,
	new_acc/2]).

init_pq(Lim) ->
	Primes = sieve:up_to(prime, numerl:isqrt(Lim)),
	init(pq:new(fun cur/1, fun next/1), Primes, 2).

init_pq(From, To) ->
	Primes = sieve:up_to(prime, numerl:isqrt(To)),
	init(pq:new(fun cur/1, fun next/1), Primes, From).

candidate(V) -> V.

result(L) when is_list(L) -> lists:reverse(L).

next_candidate(N) -> N + 1.

new_acc(Acc, V) when is_list(Acc) -> [V | Acc].

%
% internals
%

cur({V, _}) -> V.

next({V, Inc}) -> {V + Inc, Inc}.

init(Bad, [H | T], B) -> init(pq:add(Bad, {H * H, H * H}), T, B);
init(Bad, [], B) -> {Bad, B, []}.
