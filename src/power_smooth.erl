-module(power_smooth).

% A callback module of sieve to get power smooth numbers

-export([init_pq/2, candidate/1, result/1, next_candidate/1, new_acc/2]).
-export([fast_next/2]).

init_pq({B1, B2}, {_, To}) ->
	init(pq:new(fun cur/1, fun next/1), filter(B1, B2, To));
init_pq({B1, B2}, Lim) ->
	init(pq:new(fun cur/1, fun next/1), filter(B1, B2, Lim)).


candidate(V) -> V.

result(L) when is_list(L) -> lists:reverse(L).

next_candidate(N) -> N + 1.

new_acc(Acc, V) -> [V | Acc].

fast_next({V, Inc}, From) when V < From ->
	case From - V of
		Small when Small < Inc -> {V + Inc, Inc};
		Large -> {V + Inc * (Large div Inc), Inc}
	end;
fast_next(E, _) -> E.

%
% internals
%

cur({V, _}) -> V.

next({V, Inc}) -> {V + Inc, Inc}.

init(Bad, [H | T]) -> init(pq:add(Bad, {H, H}), T);
init(Bad, []) -> {Bad, 2, []}.

filter(B1, B2, Lim) ->
	Primes = sieve:up_to(prime, Lim),
	{Small, Big} = lists:splitwith(fun(X) -> X =< B1 end, Primes),
	{Medium, Large} = lists:splitwith(fun(X) -> X =< B2 end, Big),
	lists:merge3(
		filters(lists:reverse(Small), B1),
		[X * X || X <- Medium],
		Large).

filters(Primes, B) ->
	filter1(Primes, B, numerl:isqrt(B), []).

filter1([H | T], B, Lim, Acc) when H > Lim -> filter1(T, B, Lim, [H * H | Acc]);
filter1(P, B, _, Acc) -> filter2(P, B, numerl:icubrt(B), Acc).

filter2([H | T], B, Lim, Acc) when H > Lim ->
	filter2(T, B, Lim, [H * H * H | Acc]);
filter2(P, B, _, Acc) -> filter3(P, B, Acc).

filter3([H | T], B, Acc) -> filter3(T, B, [pump(H, B, H * H * H) | Acc]);
filter3(_, _, Acc) -> lists:sort(Acc).

pump(P, B, V) ->
	case V * V of
		Small when Small < B -> pump(P, B, Small);
		_ -> 
			case P * V of
				Medium when Medium < B -> pump(P, B, Medium);
				Large -> Large
			end
	end.
