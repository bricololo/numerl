-module(wheel2).

-export([init/1, init2/1, next/1]).


init(Divs) ->
	Count = lists:foldl(fun(E, P) -> (E - 1) * P end, 1, Divs),
	Seed = seed(Divs, Count + 1),
	{delta(Seed), []}.

init2(Divs) ->
	P = lists:foldl(fun(E, A) -> E * A end, 1, Divs),
	{delta(filter(Divs, P)), []}.

next({[H | T], Acc}) -> {H, {T, [H | Acc]}};
next({[], Acc}) -> next({lists:reverse(Acc), []}).

%%%
%%% Implementation
%%%

seed(Divs, Count) ->
	Start = lists:max(Divs) + 2,
	seed(Divs, Start, 0, Count, []).


seed(_, _, Count, Count, Acc) -> lists:reverse(Acc);
seed(Divs, C, Length, Count, Acc) ->
	case check(Divs, C) of
		C -> seed(Divs, C + 2, Length + 1, Count, [C | Acc]);
		_ -> seed(Divs, C + 2, Length, Count, Acc)
	end.


check([], C) -> C;
check([H | _], C) when (C rem H) =:= 0 -> 0;
check([_ | T], C) -> check(T, C).


delta(L) ->
	[B - A || {A, B} <- lists:zip(lists:sublist(L, length(L) -1), tl(L))].

filter(L, P) ->
	M = lists:max(L) + 2,
	C = lists:seq(M, M + P - 1, 2),
	F = fun(Pr, Acc) -> [X || X <- Acc, X rem Pr =/=0] end,
	lists:foldl(F, C, tl(L)).
