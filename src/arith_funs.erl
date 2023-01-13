-module(arith_funs).

-export([phi/1, tau/1, sigma/1, mu/1, omega/1]).

% All these functions are multiplicative or additive, so they are easy to
% compute if the full factorization of N is known. The full factorization of N
% is represented by a list of {P_i, E_i} such that P_i is prime, P_i =/= P_j
% when i =/= j, E_i >= 1 and N = lists:foldl(P_i ^ E_i * Acc, 1, [{P_i, E_i}]).

% This module does not check if the input is a valid full factorization and will
% provide wrong answers when the input is not as expected

% Euler phi/totient function gives the amount of numbers lower than N that are
% coprime with N
phi(L) when is_list(L) ->
	mult(L, fun({P, E}) -> numerl:ipow(P, E - 1) * (P - 1) end).

% number of positive divisors
tau(L) when is_list(L) ->
	mult(L, fun({_, E}) -> 1 + E end).

% sum of positive divisors
sigma(L) when is_list(L) ->
	mult(L, fun({P, E}) -> (numerl:ipow(P, E + 1) - 1) div (P - 1) end).

% mu(N) is 0 if N is a muttiple of a square, and otherwise 1 if N has an even
% number of prime factors and -1 if N has an odd number of primes factors.
mu(L) when is_list(L) ->
	% mult(L, fun({_, 1}) -> -1; (_) -> 0 end). would be correct but slower
	moebius(L, 1).

% number of distinct primes that divides N
omega(L) when is_list(L) ->
	% add(L, fun({_, _}) -> 1 end). would be correct but slower
	length(L).

mult(L, Fun) -> lists:foldl(fun(E, Acc) -> Acc * Fun(E) end, 1, L).

moebius([{_, 1} | T], Acc) -> moebius(T, -Acc);
moebius([], Acc) -> Acc;
moebius(_, _) -> 0.
