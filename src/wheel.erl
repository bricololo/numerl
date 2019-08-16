-module(wheel).

-export([init/1, next/1]).


init([3, 5 | [7 | Divs] = T]) ->
	Length = lists:foldl(fun(E, L) -> E * L end, 210, Divs),
	Notches =
		lists:merge([
			[Y || Y <- lists:seq(11, 10 + Length, 30), check(T, Y) =/= 0],
			[Y || Y <- lists:seq(13, 12 + Length, 30), check(T, Y) =/= 0],
			[Y || Y <- lists:seq(17, 16 + Length, 30), check(T, Y) =/= 0],
			[Y || Y <- lists:seq(19, 18 + Length, 30), check(T, Y) =/= 0],
			[Y || Y <- lists:seq(23, 22 + Length, 30), check(T, Y) =/= 0],
			[Y || Y <- lists:seq(29, 28 + Length, 30), check(T, Y) =/= 0],
			[Y || Y <- lists:seq(31, 30 + Length, 30), check(T, Y) =/= 0],
			[Y || Y <- lists:seq(37, 36 + Length, 30), check(T, Y) =/= 0]]),
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


delta(L) ->
	[B - A || {A, B} <- lists:zip(lists:sublist(L, length(L) -1), tl(L))].
