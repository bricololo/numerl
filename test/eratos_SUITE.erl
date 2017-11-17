-module(eratos_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [{group, exported}].

groups() ->
	[{exported,
		[shuffle],
		[sieve_small,
		 sieve_small_ets,
		 sieve_1000,
		 sieve_10000,
		 sieve_100000,
		 sieve_1000000]
	 },
	 {internal, [shuffle],[]}].

sieve_small(_) ->
	% any value of N < 121 will not use the priority queue at all
	Goal =
		[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67,
		 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113],
	Result = eratos:sieve(120),
	30 = length(Result),
	113 = lists:last(Result),
	Goal = Result,
	ok.

sieve_small_ets(_) ->
	Tid = ets:new(primes, []),
	P = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67,
		 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113],
	Goal = [{X, y} || X <- P],
	Tid = eratos:sieve(120, Tid),
	Goal = lists:sort(ets:tab2list(Tid)),
	ok.

% got the value of length and last from pari/gp
sieve_1000(Conf) ->
	sieve([{value, 1000}, {length, 168}, {last, 997} | Conf]).
sieve_10000(Conf) ->
	sieve([{value, 10000}, {length, 1229}, {last, 9973} | Conf]).
sieve_100000(Conf) ->
	sieve([{value, 100000}, {length, 9592}, {last, 99991} | Conf]).
sieve_1000000(Conf) ->
	sieve([{value, 1000000}, {length, 78498}, {last, 999983} | Conf]).

sieve(Conf) ->
	Result = eratos:sieve(proplists:get_value(value, Conf)),
	Length = proplists:get_value(length, Conf),
	Length = length(Result),
	Last = proplists:get_value(last, Conf),
	Last = lists:last(Result),
	ok.
