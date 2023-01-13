-module(squarefree).

-export([init/1, init/2, candidate/1, result/1, next_candidate/1,
	new_acc/2, bad/1, next_bad/2]).

init(Lim) ->
	Primes = sieve:up_to(prime, numerl:isqrt(Lim)),
	init(pq_skew:new(fun next/1), Primes, 2).

init(From, To) ->
	Primes = sieve:up_to(prime, numerl:isqrt(To)),
	init(pq_skew:new(fun next/1), Primes, From).

candidate(V) -> V.

result(L) when is_list(L) -> lists:reverse(L).

next_candidate(N) -> N + 1.

new_acc(Acc, V) when is_list(Acc) -> [V | Acc].

bad(Bad) -> pq_skew:val(Bad).
next_bad(Bad, Value) -> pq_skew:bump(Bad, Value).

%
% internals
%

next({V, Inc}) -> {V + Inc, Inc}.

init(Bad, [H | T], B) -> init(pq_skew:add(Bad, {H * H, H * H}), T, B);
init(Bad, [], B) -> {Bad, B, [1]}.
