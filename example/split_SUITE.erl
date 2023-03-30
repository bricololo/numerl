-module(split_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [{group, exported}].

groups() ->
	[{exported,
		[shuffle],
		[odd, naive, hart, lehman]
	},
	{internal, [shuffle], []}].

odd(_) ->
	N = 500_009*500_029,
	Lim = 500_000,

	{odd, fail, N, Lim, _} = split:odd(N, Lim, 5, 2),
	{odd, ok, [500_009], Cont} = split:odd(N, 600_000, 5, 2),
	{odd, ok, [500_029], Cont2} = split:odd_cont(N div 500_009, Cont),
	{odd, ok, [500_029], Cont2} = split:odd_cont(N, Cont),

	% note that odd does not detect squares factors
	N2 = N*N,
	{odd, ok, [500_009], Cont} = split:odd(N2, 600_000, 5, 2),
	{odd, ok, [500_029], Cont2} = split:odd_cont(N2 div 500_009, Cont),

	% note also that odd does not cope with multiples of 3
	N3 = numerl:ipow(3, 40),
	{odd, fail, N3, Lim, _} = split:odd(N3, Lim, 5, 2),

	ok.

naive(_) ->
	P600 = eratos:sieve(600_000),
	P500 = lists:takewhile(fun(P) -> P < 500_000 end, P600),
	N = 500_009*500_029,
	Lim = 500_000,

	% None of the primes divides N
	{naive, fail, N, Lim} = split:naive(N, Lim, P500),
	% the limit has been reached but there is still some primes to test.
	{naive, fail, N, Lim, 500009} = split:naive(N, Lim, P600),

	{naive, ok, [500009], Cont} = split:naive(N, 600_000, P600),
	{naive, fail, N, _, 500029} = split:naive_cont(N, Cont),
	{naive, fail, 500029, _, 500029} = split:naive_cont(N div 500009, Cont),

	N_2 = 500_009*500_029*500_041,
	{naive, ok, [500009], Cont} = split:naive(N_2, 600_000, P600),
	{naive, ok, [500029], Cont2} = split:naive_cont(N_2, Cont),
	true = Cont =/= Cont2,
	{naive, ok, [500029], Cont3} = split:naive_cont(N_2 div 500_009, Cont),
	true = Cont =/= Cont3,
	true = Cont2 =/= Cont3,
	{naive, ok, [500041], _} = split:naive_cont(N_2, Cont2),
	ok.

hart(_) ->
	F = fun(M, Lim) -> split:hart(M, Lim) end,
	N = 500_009*500_029,
	N2 = 500_009*500_029*500_041,

	% N close to a square, a very small limit is enough
	{hart, ok, [500009]} = F(N, 5),

	% When the composite is not close to a square a larger limit is needed
	{hart, fail, {N2, 10, 11}} = F(N2, 10),
	{hart, fail, {N2, _, _}} = F(N2, 499994),
	{hart, ok, [500041]} = F(N2, 499995),
	ok.

lehman(_) ->
	F = fun(N) -> split:lehman(N, numerl:icubrt(N), 1) end,

	N1 = 1123877887715932507,
	{lehman, ok, [299155897], 23220} = F(N1),

	N2 = 1129367102454866881, % we're not always getting the smallest div
	{lehman, ok, [43655660929], 6750} = F(N2),

	N3 = 29742315699406748437,
	% according to lehman article, K should be 25982 but I get 825406 :-(
	{lehman, ok, [372173423], 825406} = F(N3),
	ok.
