-module(primes).

-define(N64, 18_446_744_073_709_551_615). % 1 bsl 64 - 1

-export([list/1, fermat_test/1, fermat_test/2, strong_test/1, strong_test/2]).
-export([fibonacci_test/1, lucas_test/1, lucas_test/3, no_small_div_test/1]).
-export([trial_div_test/1, trial_div_test/2, rabin_miller_test/1]).
-export([rabin_miller_test/2, is_prime/1, is_prime/2, possible_prime/1]).
-export([possible_prime/2]).

% TODO:
% -export([lucas_strong_test, baillie_PSW_test/1, next/1, prev/1]).

% list all primes up to N
list(N) when N < 11 -> [P || P <- [2, 3, 5, 7], P =< N];
list(N) -> eratos:sieve(N).

fermat_test(N) -> fermat_test(N, 2).
fermat_test(N, B) -> witness_num(numerl:ipowm(B, N - 1, N), B).

strong_test(N) -> strong_test(N, 2).
strong_test(N, B) ->
	S = num_util:p2(N - 1),
	T = N bsr S,
	strong(numerl:ipowm(B, T, N), N, S, B).

fibonacci_test(N) -> fibonacci(num_lib:fibm(N - numerl:jacobi(N, 5), N)).

lucas_test(N) -> lucas_test(N, 1, 3).
lucas_test(N, A, B) ->
	D = A * A - 4 * B,
	case is_power:square(D) of
		{true, _} -> {wrong_choice_of_parameters, A, B};
		false ->
			case numerl:gcd(N, 2 * B * D) of
				1 -> lucas_test(N, A, B, D);
				_ -> false
			end
	end.

% do not use this test as a standalone one, but see it as a fast way to seed
% out most of the composite numbers
no_small_div_test(N) ->
	no_small_div(numerl:gcd(16_294579_238595_022365, N), N). % 3 to 53

trial_div_test(N) -> trial_div(no_small_div_test(N), N).
trial_div_test(N, List) -> trial_div_test(N, numerl:isqrt(N), List).

% rabin miller primality testing
rabin_miller_test(N) -> rabin_miller_test(N, 20).
rabin_miller_test(N, C) ->
	S = num_util:p2(N - 1),
	T = N bsr S,
	r_m(N, T, S, C).

% a probalistic test.
is_prime(N) -> is_prime(N, 20).
is_prime(N, _) when N < 103 ->
	lists:member(N,
		[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67,
			71, 73, 79, 83, 89, 97, 101]);
is_prime(N, _) when N < 10609 -> no_small_div_test(N);
is_prime(N, K) -> possible_prime(no_small_div_test(N), N, K).

%%
%% Implementation
%%

witness_num(1, _) -> true;
witness_num(_, B) -> {false, B}.

witness_bool(true, _) -> true;
witness_bool(false, B) -> {false, B}.

strong(1, _, _, _) -> true;
strong(Minus_1, N, _, _) when N - 1 =:= Minus_1 -> true;
strong(A, N, S, B) -> witness_bool(strong(A, S - 1, N), B).

fibonacci(0) -> true;
fibonacci(_) -> false.

no_small_div(1, N) ->
	no_small_div2(numerl:gcd(14_290787_196698_157718, N)); % 2 and 59 to 101
no_small_div(D, _) -> {false, D}.

no_small_div2(1) -> true;
no_small_div2(D) -> {false, D}.

trial_div(true, N) ->
	trial_div_test(N, numerl:isqrt(N),
		lists:dropwhile(fun(P) -> P < 103 end, list(100000)));
trial_div(Else, _) -> Else.

possible_prime(true, N, K) -> possible_prime(N, K);
possible_prime(_, _, _) -> false.

% at this point N has no divisor less than 103
possible_prime(N) -> possible_prime(N, 20).

possible_prime(N, C) -> possible_prime_(N, C, N rem 5).

possible_prime_(N, _, R) when R =:= 2; R =:= 3 -> fib_and_fermat(N);
possible_prime_(N, C, _) -> rabin_miller_test(N, C).

fib_and_fermat(N) -> fib_and_fermat(fermat_test(N, 2), N).

fib_and_fermat(true, N) -> num_lib:fibm(N + 1, N) =:= 0;
fib_and_fermat(Else, _) -> Else.

% TODO: replace r_m2/4 by strong/3
r_m(_, _, _, 0) -> true;
r_m(N, Q, T, C) ->
	A = rand:uniform(min(N - 1, ?N64)) + 1,
	r_m(numerl:ipowm(A, Q, N), N, Q, T, C, A).

r_m(1, N, Q, T, C, _) -> r_m(N, Q, T, C - 1);
r_m(B, N, Q, T, C, A) -> r_m_(r_m2(B, N, 0, T - 1), N, Q, T, C, A).

r_m_(false, _, _, _, _, A) -> {false, A};
r_m_(ok, N, Q, T, C, _) -> r_m(N, Q, T, C - 1).

r_m2(1, _, _, _) -> false;
r_m2(B, N, _, _) when B =:= N - 1 -> ok;
r_m2(B, N, E, T) when E < T -> r_m2(numerl:ipowm(B, 2, N), N, E + 1, T);
r_m2(_, _, _, _) -> false.

strong(_, 0, _) -> false;
strong(1, _, _) -> false;
strong(B, S, N) -> strong_(B * B rem N, S, N).

strong_(B2, _, N) when B2 =:= N - 1 -> true;
strong_(B2, S, N) -> strong(B2, S - 1, N).

% Something wrong with my implementation of this test: it seems to always
% returns false
% TODO : fix it.
% indeed the Lucas test relies on the Lucas sequence V_n and not on Lucas
% numbers L_n hence the meaningless results...
lucas_test(N, A, B, D) ->
	E = numerl:jacobi(D, N),
	case num_lib:lucasm(A, B, N - E, N) of
		0 -> true;
		_ -> {false, A, B}
	end.

trial_div_test(_, Lim, [H | _]) when H > Lim -> true;
trial_div_test(N, Lim, [H | T]) when N rem H =/= 0 -> trial_div_test(N, Lim, T);
trial_div_test(_, _, []) -> true;
trial_div_test(_, _, [H | _]) -> {false, H}.
