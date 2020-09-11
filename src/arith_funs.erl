-module(arith_funs).

-export([phi/1, d/1, sigma/1, moebius/1, omega/1]).

% All these functions are multiplicative or additive, so they are easy to
% compute if the full factorization of N is known. The full factorization of N
% is represented by a list of {P_i, E_i} such that P_i is prime, P_i =/= P_j
% when i =/= j, E_i >= 1 and N = lists:foldl(P_i ^ E_i * Acc, 1, [{P_i, E_i}]).

% This module does not check if the input is a valid full factorization and will
% provide wrong answers when the input is not as expected

phi(L) when is_list(L) ->
	mult(L, fun({P, E}) -> numerl:ipow(P, E - 1) * (P - 1) end).

d(L) when is_list(L) ->
	mult(L, fun({_, E}) -> 1 + E end).

sigma(L) when is_list(L) ->
	mult(L, fun({P, E}) -> (numerl:ipow(P, E + 1) - 1) div (P - 1) end).

moebius(L) when is_list(L) ->
	% mult(L, fun({_, 1}) -> -1; (_) -> 0 end). would be correct but slower
	moebius(L, 1).

omega(L) when is_list(L) ->
	% add(L, fun({_, _}) -> 1 end). would be correct but slower
	length(L).

mult(L, Fun) -> lists:foldl(fun(E, Acc) -> Acc * Fun(E) end, 1, L).

moebius([{_, 1} | T], Acc) -> moebius(T, -Acc);
moebius([], Acc) -> Acc;
moebius(_, _) -> 0.
