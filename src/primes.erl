-module(primes).

-define(N60, 1152921504606846976). % 1 bsl 60

-export([list/1, sieve/1, sieve/3, fermat_test/1, fermat_test/2]).
-export([strong_test/1, strong_test/2, fibonacci_test/1, lucas_test/1]).
-export([lucas_test/3, no_small_div_test/1, trial_div_test/1]).
-export([trial_div_test/2, rabin_miller_test/1, rabin_miller_test/2]).
-export([is_prime/1, is_prime/2, possible_prime/1, possible_prime/2]).

% TODO:
% -export([lucas_strong_test, baillie_PSW_test/1, next/1, prev/1, pi/1]).

% list all primes up to N
list(N) when N < 11 -> [P || P <- [2, 3, 5, 7], P =< N];
list(N) when N < 500 -> eratos:sieve(N);
list(N) when N < 5000 -> sieve(N);
list(N) -> eratos:sieve(N).

sieve(N) -> sieve(N, 11, [2, 3, 5, 7]).

sieve(N, P, F) ->
	sieve(numerl:isqrt(N), [P], lists:reverse(F), tl(init_list(F, N))).

% true just means that N is passing the test, not that N is prime...
fermat_test(N) -> fermat_test(N, 2).

fermat_test(N, B) ->
	case numerl:ipowm(B, N - 1, N) of
		1 -> true;
		_ -> {false, B}
	end.

% true just means that N is passing the test, not that N is prime...
% correspond to 1 iteration of miller_rabin
strong_test(N) -> strong_test(N, 2).

strong_test(N, B) ->
	S = num_util:p2(N - 1),
	T = N bsr S,
	case numerl:ipown(B, T, N) of
		1 -> true;
		N1 when N1 =:= N - 1 -> true;
		A ->
			case strong_test(A, S - 1, N) of
				true -> true;
				false -> {false, B}
			end
	end.

% true just means that N is passing the test, not that N is prime...
fibonacci_test(N) ->
	case misc:fibm(N - numerl:jacobi(N, 5), N) of
		0 -> true;
		_ -> false
	end.

% true just means that N is passing the test, not that N is prime...
lucas_test(N) -> lucas_test(N, 1, 2).

lucas_test(N, A, B) ->
	D = A * A - 4 * B,
	case numerl:is_square(D) of
		{true, _} -> {wrong_choice_of_parameters, A, B};
		false ->
			case numerl:gcd(N, 2 * B * D) of
				1 -> lucas_test(N, A, B, D);
				_ -> false
			end
	end.

% true just means that N is passing the test, not that N is prime...
no_small_div_test(N) ->
	case numerl:gcd(614889782588491410, N) of % up to 47
		1 ->
			case numerl:gcd(38655288426304091, N) of % up to 89
				1 -> true;
				D -> {false, D}
			end;
		D -> {false, D}
	end.

% true just means that N is passing the test, not that N is prime...
trial_div_test(N) ->
	case no_small_div_test(N) of
		true ->
			trial_div_test(N, numerl:isqrt(N),
				lists:dropwhile(fun(P) -> P < 97 end, list(100000)));
		Else -> Else
	end.

trial_div_test(N, List) -> trial_div_test(N, numerl:isqrt(N), List).

% rabin miller primality testing
rabin_miller_test(N) -> rabin_miller_test(N, 20).

% rabin miller primality testing
rabin_miller_test(N, C) ->
	T = num_util:p2(N - 1),
	Q = N bsr T,
	r_m(N, Q, T, C).

% a probalistic test.
is_prime(N) -> is_prime(N, 20).

is_prime(N, _) when N < 97 ->
	lists:member(N,
		[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67,
			71, 73, 79, 83, 89]);
is_prime(N, K) ->
	case no_small_div_test(N) of
		true when N < 9409 -> true;
		true -> possible_prime(N, K);
		_ -> false
	end.

%%
%% Implementation
%%

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

% at this point N has no divisor less than 97
possible_prime(N) -> possible_prime(N, 20).
possible_prime(N, C) ->
	case N rem 5 of
		2 -> fib_and_fermat(N);
		3 -> fib_and_fermat(N);
		_ -> rabin_miller_test(N, C)
	end.

fib_and_fermat(N) ->
	case fermat_test(N, 2) of
		true -> misc:fibm(N + 1, N) =:= 0; % bypassing the unneeded jacobi
		Else -> Else
	end.

r_m(_, _, _, 0) -> true;
r_m(N, Q, T, C) ->
	A = rand:uniform(min(N - 1, ?N60)) + 1,
	case numerl:ipowm(A, Q, N) of
		1 -> r_m(N, Q, T, C - 1);
		B ->
			case r_m2(B, N, 0, T - 1) of
				false -> {false, A};
				ok -> r_m(N, Q, T, C - 1)
			end
	end.

r_m2(1, _, _, _) -> false;
r_m2(B, N, _, _) when B =:= N - 1 -> ok;
r_m2(B, N, E, T) when E < T -> r_m2(numerl:ipowm(B, 2, N), N, E + 1, T);
r_m2(_, _, _, _) -> false.

strong_test(_, 0, _) -> false;
strong_test(1, _, _) -> false;
strong_test(B, S, N) ->
	case B * B rem N of
		N1 when N1 =:= N - 1 -> true;
		E -> strong_test(E, S - 1, N)
	end.

lucas_test(N, A, B, D) ->
	E = numerl:jacobi(D, N),
	{F1, F2} = misc:fast_fibm(N - E),
	case (A * F1 + B * F2) rem N of
		0 -> true;
		_ -> false
	end.

trial_div_test(_, Lim, [H | _]) when H > Lim -> true;
trial_div_test(N, Lim, [H | T]) when N rem H =/= 0 -> trial_div_test(N, Lim, T);
trial_div_test(_, _, []) -> true;
trial_div_test(_, _, [H | _]) -> {false, H}.
