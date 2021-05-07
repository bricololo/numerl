-module(sieve_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [{group, exported}].

groups() ->
	[{exported,
		[shuffle],
		[prime_small,
		 prime_small_ets,
		 prime_small_foldl,
		 prime_from_to,
		 prime_from_to_large,
		 prime_ets_from_to,
		 prime_1_000,
		 prime_10_000,
		 prime_100_000,
		 prime_1_000_000]
	 },
	 {internal, [shuffle],[]}].

prime_small(_) ->
	% any value of N < 121 will not use the priority queue at all
	Goal =
		[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67,
		 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113],
	Result = sieve:up_to(prime, 120),
	30 = length(Result),
	113 = lists:last(Result),
	Goal = Result,
	ok.

prime_small_ets(_) ->
	Tid = ets:new(primes, []),
	P = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67,
		 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113],
	Goal = [{X} || X <- P],
	Tid = sieve:ets(prime, 120, Tid),
	Goal = lists:sort(ets:tab2list(Tid)),
	ok.

prime_small_foldl(_) ->
	F = fun(E, Acc) -> [E | Acc] end,
	Result = lists:reverse(sieve:foldl(prime, 120, F, [])),
	Result = sieve:up_to(prime, 120),
	ok.

prime_1_000(_) ->
	Result = sieve:up_to(prime, 1_000),
	[2 | _] = Result,
	Length = 168,
	Last = 997,
	Length = length(Result),
	Last = lists:last(Result),
	% testing that each number in Result is indeed prime.
	Divs = [2 | lists:seq(3, 31, 2)],
	Goal = lists:duplicate(Length, true),
	Goal = lists:map(fun(N) -> check_prime(N, Divs) end, Result),
	ok.

prime_from_to(_) ->
	Result = sieve:from_to(prime, 120, 1_000),
	Result = sieve:from_to(prime, 1_000, 120),
	Result = [P || P <- sieve:up_to(prime, 1_000), P >= 120],
	ok.

prime_from_to_large(_) ->
	Result = sieve:from_to(prime, 8_000, 10_000),
	Result = [P || P <- sieve:up_to(prime, 10_000), P >= 8_000],
	ok.

prime_ets_from_to(_) ->
	Tid = ets:new(primes, []),
	Goal = [{P} || P <- sieve:from_to(prime, 5_000, 10_000)],
	Tid = sieve:ets(prime, 5_000, 10_000, Tid),
	Goal = lists:sort(ets:tab2list(Tid)),
	ok.

check_prime(N, [N | _]) -> true;
check_prime(N, [H | T]) when N rem H =/= 0 -> check_prime(N, T);
check_prime(_, []) -> true;
check_prime(_, _) -> false.

% got the values of length and last from pari/gp
prime_10_000(Conf) ->
	sieve([{value, 10_000}, {length, 1229}, {last, 9973} | Conf]).
prime_100_000(Conf) ->
	sieve([{value, 100_000}, {length, 9592}, {last, 99991} | Conf]).
prime_1_000_000(Conf) ->
	sieve([{value, 1_000_000}, {length, 78498}, {last, 999983} | Conf]).

sieve(Conf) ->
	Result = sieve:up_to(prime, proplists:get_value(value, Conf)),
	Length = proplists:get_value(length, Conf),
	Length = length(Result),
	Last = proplists:get_value(last, Conf),
	Last = lists:last(Result),
	ok.
