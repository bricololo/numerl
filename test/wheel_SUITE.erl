-module(wheel_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [{group, exported}].

groups() ->
	[
		{exported,
			[shuffle],
			[init, next, sync]
		},
		{internal, [shuffle], []}
	].

init(_) ->
	Init = fun(List) -> wheel:init(List) end,

	{[2], []} = Init([2]),
	{[2, 4], []} = Init([2, 3]),
	{[2, 4], []} = Init([3]),

	Wheel = fun(List) -> element(1, Init(List)) end,
	[4, 2, 4, 2, 4, 6, 2, 6] = Wheel([3, 5]),

	Wlength = fun(List) -> lists:sum(List) end,
	Wsize = fun(List) -> lists:foldl(fun(E, A) -> A*(E-1) end, 1, List) end,

	length(Wheel([3, 5, 7])) =:= Wsize(Wheel([3, 5, 7])), % 48
	210 = Wlength(Wheel([3, 5, 7])),

	length(Wheel([3, 5, 7, 11, 13, 17])) =:=
		Wsize(Wheel([3, 5, 7, 11, 13, 17])), % 92 160
	510510 = Wlength(Wheel([3, 5, 7, 11, 13, 17])),

	ok.

next(_) ->
	Next = fun(Wheel) -> wheel:next(Wheel) end,

	{2, {[4], [2]}} = Next(wheel:init([3])),

	Use =
		fun(_, {[H | _] = Acc, Wheel}) ->
			{Inc, Wheel2} = Next(Wheel),
			{[H+Inc | Acc], Wheel2}
		end,

	Wheel = wheel:init([3, 5]),
	W = lists:reverse(element(1, Wheel)),
	{[37, 31, 29, 23, 19, 17, 13, 11, 7], {[], W}} =
		lists:foldl(Use, {[7], Wheel}, lists:seq(1, 8)),

	ok.

sync(_) ->
	Sync = fun(S, G, W, L) -> wheel:sync(S, G, W, L) end,

	Wheel = wheel:init([3, 5]),

	{0, Wheel} = Sync(7, 7, Wheel, 30),
	{0, Wheel} = Sync(7, 37, Wheel, 30),
	{0, Wheel} = Sync(7, 67, Wheel, 30),

	W = element(1, Wheel),

	{0, {W2, [4]}} = Sync(7, 11, Wheel, 30), W2 = tl(W),

	{-3, Wheel} = Sync(7, 10, Wheel, 30),
	{-3, Wheel} = Sync(7, 1_000_000, Wheel, 30),

	{0, {_, [2, 4]}} = Sync(7, 13, Wheel, 30),

	% let's try with a fully rotated wheel
	W3 = lists:reverse(W),
	Wheel2 = {[], W3},
	{0, Wheel2} = Sync(37, 37, Wheel2, 30),
	{-1, Wheel} = Sync(37, 38, Wheel2, 30),

	ok.
