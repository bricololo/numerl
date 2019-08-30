-module(split).

-export([fermat/3, fermat_cont/2]).
-export([rho/5, rho_cont/2]).
-export([brent/6, brent_cont/2]).

-define(RHO_ITER, 50).

% this module regroups functions that find a list (often reduce to 1 element) of
% proper divisors of a given integer. No effort in this module are made to
% ensure that the input is indeed composite or that the proper divisors found
% (if any) are indeed prime. When possible/needed the split function returns a
% continuation.

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
