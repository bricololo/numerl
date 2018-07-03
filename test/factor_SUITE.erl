-module(factor_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [{group, exported}].

groups() ->
	[{exported,
	  	[shuffle],
		[naive_1,
		 naive_2,
		 fermat,
		 %lehman,
		 rho,
		 brent,
		 pollard_2,
		 pollard_3]%,
		 %naive_list_1,
		 %naive_list_2,
		 %naive_list_3]
	},
	{internal, [shuffle], []}].

naive_1(_) ->
	F = fun(N) -> factor:naive(N) end,

	% working cases,
	[2] = F(2),
	[5] = F(5),
	[101, 103, 107, 109] = F(101 * 103 * 107 * 109),

	% repeated prime factors
	[101, 101, 103, 103, 103, 107, 109] =
       		F(101 * 101 * 103 * 103 * 103 * 107 * 109),

	% no factorisation as smallest divisor is larger than the default limit
	{500000, 500009 * 500029, []} = F(500009 * 500029),

	% partial factorisation because several prime factors are larger than default limit
	{500000, 500009 * 500029, [101, 103, 107, 109]} =
       		F(101 * 103 * 107 * 109 * 500009 * 500029),

	% complete factorisation as only one factor is larger than the default limit
	[101, 103, 500009] = F(101* 103 * 500009),
	ok.
	
naive_2(_) ->
	% fix a limit smaller than sqrt(N)
	[2, 3, 5, 7] = factor:naive(210, 8),
	ok.

fermat(_) ->
	F = fun(N) -> factor:fermat(N) end,

	% note that the factors are not sorted.
	[1999, 1009] = F(1009 * 1999),

	% note that fermat returns two, not necessarily prime, factors
	[103 * 107, 101 * 109] = F(101 * 103 * 107 * 109),

	% detecting squares
	[101, 101] = F(101 * 101),
	[101 * 109, 101 * 109] = F(101 * 101 * 109 * 109),
	ok.

rho(_) ->
	274177 = factor:rho(1 bsl 64 + 1),
	
	% TODO: find a value of N that fails to be factored
	ok.

brent(_) ->
	274177 = factor:rho(1 bsl 64 + 1),
	ok.

pollard_2(_) ->
	F = fun(N, B) -> factor:pollard(N, B) end,

	Rep_97 = (numerl:ipow(10, 97) - 1) div 9,
	12004721 = F(Rep_97, 100),

	Mers_101 = 1 bsl 101 - 1,
	fail = F(Mers_101, 500000),
	ok.

pollard_3(_) ->
	% with a starting point of 3 instead of the default 2, we can now split
	% Mersenne_101 :)
	Mers_101 = 1 bsl 101 - 1,
	7432339208719 = factor:pollard(Mers_101, 280000, 3),
	ok.