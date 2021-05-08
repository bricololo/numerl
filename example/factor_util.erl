-module(factor_util).

-export([l/1, factor_base/2, reduce/2]).

l(N) ->
	Ln = math:log(N),
	math:exp(math:sqrt(Ln * math:log(Ln))).

factor_base(B, Filter) ->
	Candidates = tl(eratos:sieve(B)),
	[P || P <- Candidates, Filter(P)].

reduce(N, [F | T]) when N rem F =:= 0 -> reduce(N div F, [F, F | T]);
reduce(N, L) -> {N, L}.
