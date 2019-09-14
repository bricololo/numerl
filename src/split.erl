-module(split).

-export([odd/4, odd_cont/2]).
-export([fermat/3, fermat_cont/2]).
-export([hart/2]).
-export([lehman_odd/3, lehman_even/3]).
-export([rho/5, rho_cont/2]).
-export([brent/6, brent_cont/2]).

-define(RHO_ITER, 50).

% this module regroups functions that find a list (often reduce to 1 element) of
% proper divisors of a given integer. No effort in this module are made to
% ensure that the input is indeed composite or that the proper divisors found
% (if any) are indeed prime. When possible/needed the split function returns a
% continuation.

odd_cont(N, {Lim, Factor, Inc}) -> odd(N, Lim, Factor, Inc).

% try to divide N by all the p such that p mod 6 = ± 1.
% Factor and Inc have to be set so that both Factor and Factor + Inc are both
% ± 1 (6), Inc = 4 or 2
odd(N, Lim, Factor, Inc) when Factor < Lim ->
	case N rem Factor of
		0 -> {[Factor],{min(Lim,numerl:isqrt(N div Factor)),Factor+Inc,6-Inc}};
		_ -> odd(N, Lim, Factor + Inc, 6 - Inc)
	end;
odd(N, Lim, _, _) -> {odd, fail, Lim, N}.


fermat_cont(N, {Square, Inc}) -> fermat(N, Square, Inc).

fermat(N, Square, Inc) ->
	Next_square = Square + Inc,
	Next_inc = Inc + 2,
	case numerl:is_square(Next_square - N) of
		false -> fermat(N, Next_square, Next_inc);
		{true, Square_root} ->
			%V = numerl:isqrt(Next_square), but it is faster to derive it from
			% Next_inc
			V = Next_inc bsr 1,
			{[V - Square_root, V + Square_root], {Next_square, Next_inc}}
	end.


hart(N, Lim) -> hart(N, Lim, 1).

lehman_odd(N, B, K) ->
	Cst = 4 * K * N,
	L = numerl:isqrt(Cst),
	R = (K + N) band 3,
	case lehman_a(start_a(L, R, 4), Cst + B * B, 4, Cst) of
		nope -> lehman_even(N, B, K + 1);
		G -> {[numerl:gcd(N, G)], K}
	end.

lehman_even(N, B, K) ->
	Cst = 4 * K * N,
	L = numerl:isqrt(Cst),
	case lehman_a(start_a(L, 1, 2), Cst + B * B, 2, Cst) of
		nope -> lehman_odd(N, B, K + 1);
		G -> {[numerl:gcd(N, G)], K}
	end.

brent_cont(N, {Fun,Start,Y,Iter,Power}) -> brent(N, Fun, Start, Y, Iter, Power).

brent(N, Fun, X, _, Iter, Iter) -> brent(N, Fun, X, X, Iter, Iter bsl 1);
brent(N, Fun, X, Y, Iter, Power) ->
	Lim = min(Power - Iter, ?RHO_ITER),
	case brent_(N, Fun, X, Y, 1, Lim) of
		{1, Next_x} -> brent(N, Fun, Next_x, Y, Iter + Lim, Power);
		{0, _} -> brent_bt(N, Fun, X, Y, Iter, Power);
		{Prod, Next_x} ->
			case numerl:gcd(N, Prod) of
				1 -> brent(N, Fun, Next_x, Y, Iter + Lim, Power);
				G -> {[G], {Fun, Next_x, Y, Iter + Lim, Power}}
			end
	end.

%
% Implementation
%

hart(N, Lim, I) when I =< Lim ->
	G = N * I,
	Tmp = numerl:isqrt(G),
	S = case Tmp * Tmp of G -> Tmp; _ -> Tmp + 1 end,
	M = (S * S) rem N,
	case numerl:is_square(M) of
		{true, T} -> {[numerl:gcd(N, abs(S - T))], {I, Lim}};
		_ -> hart(N, Lim, I + 1)
	end;
hart(_, _, _) -> fail.

lehman_a(A, Max, _, _) when A * A > Max -> nope;
lehman_a(A, Max, Inc, Cst) ->
	case numerl:is_square(A * A - Cst) of
		false -> lehman_a(A + Inc, Max, Inc, Cst);
		{true, B} -> A + B
	end.

start_a(S, R, M) ->
	case S rem M of
		R -> S;
		V -> S + R - V
	end.

rho_cont(N, {Fun, X, Y, Gcd}) -> rho(N, Fun, X, Y, Gcd).

rho(N, Fun, X, Y, Gcd) ->
	case rho(N, Fun, X, Y, Gcd, ?RHO_ITER) of
		{1, U, V} -> rho(N, Fun, U, V, 1, ?RHO_ITER);
		{D, U, V} ->
			case numerl:gcd(N, D) of
				1 -> rho(N, Fun, U, V, 1);
				_ -> rho_bt(N, Fun, X, Y, 1)
			end
	end.
rho(_, _, X, Y, Gcd, 0) -> {Gcd, X, Y};
rho(N, Fun, X, Y, Gcd, Count) ->
	U = Fun(X),
	V = Fun(Fun(Y)),
	rho(N, Fun, U, V, (Gcd * abs(U - V)) rem N, Count - 1).

% bactracking GCD computations
rho_bt(N, Fun, X, Y, 1) ->
	U = Fun(X),
	V = Fun(Fun(Y)),
	rho_bt(N, Fun, U, V, numerl:gcd(N, abs(U - V)));
rho_bt(N, _, _, _, N) -> fail;
rho_bt(_, Fun, X, Y, Gcd) -> {[Gcd], {Fun, X, Y, Gcd}}.

brent_(_, _, X, _, Prod, 0) -> {Prod, X};
brent_(N, Fun, X, Y, Prod, Count) ->
	Next_x = Fun(X),
	brent_(N, Fun, Next_x, Y, (Prod * (Y - Next_x)) rem N, Count - 1).

% backtracking GCD computations
brent_bt(N, Fun, X, _, Iter, Iter) -> brent_bt(N, Fun, X, X, Iter, Iter bsl 1);
brent_bt(N, Fun, X, Y, Iter, Power) ->
	Next_x = Fun(X),
	case numerl:gcd(N, Y - Next_x) of
		1 -> brent(N, Fun, Next_x, Y, Iter + 1, Power);
		N -> brent(N, Fun, Next_x, Y, Iter + 1, Power);
		G -> G
	end.
