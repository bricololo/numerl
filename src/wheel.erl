-module(wheel).

-export([init/1, next/1]).


init([3, 5, 7 | Divs]) ->
	Length = lists:foldl(fun(E, L) -> E * L end, 210, Divs),
	Notches =
		lists:merge(
			[filter(P, Length, Divs) ||
				P <- [11, 13, 17, 19, 23, 29, 31, 37]]),
	{delta(Notches ++ [hd(Notches) + Length]), []};
init(Divs) ->
	Count = lists:foldl(fun(E, P) -> (E - 1) * P end, 1, Divs),
	Seed = seed(Divs, Count + 1),
	{delta(Seed), []}.


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


check([H | _], C) when (C rem H) =:= 0 -> 0;
check([_ | T], C) -> check(T, C);
check([], C) -> C.


delta(L) when length(L) < 2 -> undefined;
delta(L) -> delta(L, []).

delta([F, S | _] = L, Acc) -> delta(tl(L), [S - F | Acc]);
delta([_], Acc) -> lists:reverse(Acc).


filter(P, L, T) ->
	[X || X <- lists:seq(P, P - 1 + L, 30), X rem 7 =/= 0, check(T, X) =/= 0].
