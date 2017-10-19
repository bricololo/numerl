-module(primes).

-define(N60, 1152921504606846976). % 1 bsl 60

-export([full/1, list/1, pi/1, sieve/3, is_prime/1]).
-export([rabin_miller/1, rabin_miller/2, lucas/1, lucas/3]).

% -export([next/1, prev/1]).

% give {number of primes, product of all primes, list of primes} up to N
full(2) -> {1, 2, [2]};
full(3) -> {2, 6, [2, 3]};
full(N) ->
	S = max(3, numerl:isqrt(N)),
	{Pi, Pr, L} = full(S),
	L2 = extend(L, Pr, N),
	{Pi + length(L2), lists:foldl(fun(X, Acc) -> X * Acc end, Pr, L2), L ++ L2}.

% list all primes up to N
list(N) when N < 11 -> [P || P <- [2, 3, 5, 7], P =< N];
list(N) when N < 500 -> eratos:sieve(N);
list(N) when N < 5000 -> sieve(N);
list(N) -> eratos:sieve(N).

% very inefficient number of primes up to N
pi(N) -> length(list(N)).

sieve(N, P, F) ->
	sieve(numerl:isqrt(N), [P], lists:reverse(F), tl(init_list(F, N))).

% a probalistic test.
is_prime(N) when N < 101 ->
	lists:member(N,
		[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67,
			71, 73, 79, 83, 89, 97]);
is_prime(N) ->
	case numerl:gcd(2305567963945518424753102147331756070, N) of
		1 when N < 10201 -> true;
		1 -> possible_prime(N);
		_ -> false
	end.

% rabin miller primality testing
rabin_miller(N) -> rabin_miller(N, 20).

% rabin miller primality testing
rabin_miller(N, C) ->
	T = num_util:p2(N - 1),
	Q = N bsr T,
	r_m(N, Q, T, C).

% Nth lucas number
lucas(N) -> lucas(2, 1, N).

% Nth lucas number of parameter A, B
lucas(A, _, 0) -> A;
lucas(_, B, 1) -> B;
lucas(A, B, N) ->
	{F1, F2} = misc:fib(N - 1),
	A * F1 + B * F2.


%%
%% Implementation
%%

extend(L, Pr, Lim) ->
	Max = lists:last(L) + 2,
	G = numerl:gcd(Max, Pr),
	T = case G of 1 -> Pr * Max; _ -> Pr end,
	R = [X || X <- lists:seq(Max + 2, Lim, 2), numerl:gcd(T, X) =:= 1],
	case G of 1 -> [Max | R]; _ -> R end.	

sieve(N) -> sieve(N, 11, [2, 3, 5, 7]).

sieve(Max, Primes, Filter, Unknown) ->
	case hd(Primes) of
		Large when Large > Max ->
			lists:append([lists:reverse(Filter), Primes, Unknown]);
		Prime ->
			{N_primes, N_unknown} = split(Unknown, Prime * Prime, []),
			sieve(
				Max,
				lists:append(tl(Primes), N_primes),
				[Prime | Filter],
				[N || N <- N_unknown, N rem Prime =/= 0]
			)
	end.

split([H | T], M, Acc) when H < M -> split(T, M, [H | Acc]);
split(L, _, Acc) -> {lists:reverse(Acc), L}.

init_list([2], N) -> lists:seq(3, N, 2);
init_list([2, 3], N) -> lists:merge(lists:seq(5, N, 6), lists:seq(7, N, 6));
init_list(L, N) ->
	P = lists:foldl(fun(E, A) -> E * A end, 1, L),
	lists:merge([lists:seq(X, N, P) || X <- filter(L, P)]).

filter(L, P) ->
	M = lists:max(L) + 2,
	C = lists:seq(M, M + P - 1, 2),
	F = fun(Pr, Acc) -> [X || X <- Acc, X rem Pr =/=0] end,
	lists:foldl(F, C, tl(L)).

% at this point N has no divisor less than 100
possible_prime(N) ->
	case {numerl:ipowm(2, N - 1, N), N rem 5} of
		{1, 2} -> misc:fibm(N + 1, N) =:= 0;
		{1, 3} -> misc:fibm(N + 1, N) =:= 0;
		{1, _} -> rabin_miller(N);
		{_, _} -> false
	end.

r_m(_, _, _, 0) -> maybe;
r_m(N, Q, T, C) ->
	A = rand:uniform(min(N -1,?N60)) + 1,
	case numerl:ipowm(A, Q, N) of
		1 -> r_m(N, Q, T, C - 1);
		B ->
			case r_m2(B, N, 0, T - 1) of
				false -> false;
				ok -> r_m(N, Q, T, C - 1)
			end
	end.

r_m2(1, _, _, _) -> false;
r_m2(B, N, _, _) when B =:= N - 1 -> ok;
r_m2(B, N, E, T) when E < T -> r_m2(numerl:ipowm(B, 2, N), N, E + 1, T);
r_m2(_, _, _, _) -> false.
