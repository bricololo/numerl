-module(primes_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [{group, exported}].

groups() ->
	[
		{
			exported,
			[shuffle],
			[
				fermat_test,
				strong_test,
				fibonacci_test,
				%lucas_test, % primes:lucas test/4 is broken
				no_small_div_test%,
				%rabin_miller_test
			]
		},
		{internal, [shuffle], []}
	].

fermat_test(_) ->
	F = fun(N) -> primes:fermat_test(N) end,
	F2 = fun(N, B) -> primes:fermat_test(N, B) end,

	% primes are indeed passing the test
	true = F(1009),
	true = F2(1009, 3),

	% some famous counterexamples
	true = F(341),
	{false, 3} = F2(341, 3),
	true = F(1 bsl 32 + 1),
	{false, 3} = F2(1 bsl 32 + 1, 3),
	true = F(1 bsl 128 + 1),
	{false, 3} = F2(1 bsl 128 + 1, 3),

	% Carmichaels numbers
	true = F(561),
	true = F(1729),
	true = F(8719309),
	ok.

strong_test(_) ->
	F = fun(N) -> primes:strong_test(N) end,
	F2 = fun(N, B) -> primes:strong_test(N, B) end,

	% primes are indeed passing the test
	true = F(1009),
	true = F2(1009, 3),

	% what happened to our previous counter examples:
	{false, 2} = F(341),
	true = F(1 bsl 32 + 1),
	{false, 3} = F2(1 bsl 32 + 1, 3),
	true = F(1 bsl 128 + 1),
	{false, 3} = F2(1 bsl 128 + 1, 3),

	{false, 2} = F(561),
	{false, 2} = F(1729),
	{false, 2} = F(8719309),
	ok.

fibonacci_test(_) ->
	F = fun(N) -> primes:fibonacci_test(N) end,

	% primes are indeed passing the test
	true = F(1009),

	% what happened to our previous counter examples:
	% TODO: any fibonacci pseudo primes ?
	false = F(341),
	false = F(1 bsl 32 + 1),

	false = F(561),
	false = F(1729),
	false = F(8719309),
	ok.

lucas_test(_) ->
	F = fun(N) -> primes:lucas_test(N) end,
	F3 = fun(N, A, B) -> primes:lucas_test(N, A, B) end,

	% primes are indeed passing the test
	true = F(1009),
	true = F3(1009, 1, 3),
	{wrong_choice_of_parameters, 5, 4} = F3(1009, 5, 4),

	% TODO: any lucas pseudo primes ?
	false = F(341),
	false = F(1 bsl 32 + 1),

	false = F(561),
	false = F(1729),
	false = F(8719309),
	ok.

no_small_div_test(_) ->
	F = fun(N) -> primes:no_small_div_test(N) end,

	% a fast but weak and limited test

	% primes larger than 89 are indeed passing the test
	true = F(1009),
	% but
	{false, 83} = F(83),

	% what happened to our previous counter examples:
	{false, 341} = F(341),
	% but
	true = F(1 bsl 32 + 1), % because the smallest div is > 89

	% 100% exact between 90 and 9408
	{false, 30} = F(90),
	true = F(97),
	true = F(9403),
	{false, 23} = F(9407),
	{false, 42} = F(9408),
	% but
	true = F(97 * 97),
	ok.
