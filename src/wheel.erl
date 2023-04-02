-module(wheel).

-export([init/1, next/1, sync/3, sync/4]).

init([2]) -> {[2], []};
init([2 | Divs]) -> init(Divs);
init([3]) -> {[2, 4], []};
init([3 | Divs]) ->
	{Notches, Len} = filter(Divs, [5, 7], 6),
	{delta(Notches++[hd(Notches)+Len]), []}.

next({[H | T], Acc}) -> {H, {T, [H | Acc]}};
next({[], Acc}) -> next({lists:reverse(Acc), []}).

sync(Start, Goal, {A, B}) ->
	sync(Start, Goal, {A, B}, lists:sum(A)+lists:sum(B)).

sync(Start, Goal, Wheel, Length) ->
	Miss = (Goal-Start) rem Length,
	sync(Miss, Wheel).

%%%
%%% Implementation
%%%

delta(L) when length(L) < 2 -> undefined;
delta(L) -> delta(L, []).

delta([F, S | _] = L, Acc) -> delta(tl(L), [S-F | Acc]);
delta([_], Acc) -> lists:reverse(Acc).

filter([], List, Inc) -> {List, Inc};
filter([P | Primes], List, Inc) ->
	filter(Primes,
		[V || I <- lists:seq(0, P-1), R <- List, V <- [I*Inc+R], V rem P =/= 0],
		Inc*P).

sync(0, Wheel) -> {0, Wheel};
sync(Miss, {[Inc | _], _} = Wheel) when Miss < Inc -> {-Miss, Wheel};
sync(Miss, {[], Acc}) -> sync(Miss, {lists:reverse(Acc), []});
sync(Miss, Wheel) ->
	{Inc, Wheel2} = next(Wheel),
	sync(Miss - Inc, Wheel2).
