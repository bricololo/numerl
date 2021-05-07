-module(num_lib_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [{group, exported}].

groups() ->
	[{exported, [shuffle], [is_square]},
		{internal, [shuffle], []}].

is_square(_) ->
	Isq = fun(N) -> num_lib:is_square(N) end,

	false = Isq(-5),
	{true, 0} = Isq(0),
	{true, 1} = Isq(1),
	false = Isq(2),

	{true, 2} = Isq(4),
	false = Isq(8),
	{true, _} = Isq(9 bsl 56),
	false = Isq(11 bsl 56),
	{true, _} = Isq(9 bsl 100),
	false = Isq(17 bsl 100),

	L = [X * X || X <- lists:seq(0, 1_000)],
	L = [X || X <- lists:seq(0, 1_000_000), Isq(X) =/= false],

	false = Isq(8), % ok mod 4 but an odd number of trailing 0
	false = Isq(48), % ok mod 4, even number of trailing 0 but not a square
	false = Isq(33), % ok mod 8 but not mod 208
	false = Isq(17), % ok mod 8 and 208 but not mod 231
	false = Isq(273), % ok mod 8, 208 and 231 but not mod 145
	false = Isq(1401), % ok mod 8, 208, 231 and 145 but not mod 37
	false = Isq(7401), % ok mod 8, 208, 231, 145 and 37 but not mod 17
	false = Isq(1785), % ok mod 8, 208, 231 and 145, 37 and 17 but not square
	ok.
