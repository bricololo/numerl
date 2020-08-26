-module(factor).

-export([odd/1, odd/2, naive/1, naive/2, fermat/1, hart/1, lehman/1]).
-export([rho/1, rho/2, rho/3, brent/1, brent/2, brent/3, pollard/2, pollard/3]).
-export([naive_list/3]).

-define(DEFAULT_LIMIT, 500000).

%%%
%%% exported functions
%%%

odd(N) -> odd(N, ?DEFAULT_LIMIT).

odd(N, Lim) when N band 1 =:= 1 -> odd_3(N, Lim, []);
odd(N, Lim) -> odd(N bsr 1, Lim, [2]).

% simply try all the prime numbers up to min(sqrt(N), 500000)
naive(2) -> [2];
naive(3) -> [3];
naive(N) -> naive(N, ?DEFAULT_LIMIT).

% simply try all the prime numbers up to min(sqrt(N), Limit)
naive(N, Limit) -> naive_list(N, min(Limit, numerl:isqrt(N) + 1)).

% used both as backend of naive/1 and naive/2 but can also be used to check
% against a factor base.
naive_list(N, Lim, Primes) ->
	case split:naive(N, Lim, Primes) of
		{naive, ok, [Factor], Cont} ->
			{N_N, Div} = reduce(N div Factor, [Factor]),
			naive_list_(N_N, Cont, Div);
		_ -> {fail, N, Lim, []}
	end.


% just for curiosity, lehman is usually faster
fermat(N) ->
	case numerl:is_square(N) of
		false ->
			R = numerl:isqrt(N),
			T = R + 1,
			{fermat, ok, Factors} = split:fermat(N, T * T, 2 * T + 1),
			Factors;
		{true, Square_root} -> [Square_root, Square_root]
	end.

% a quite recent algorithm (2012)
hart(N) ->
	case numerl:is_square(N) of
		false ->
			B = numerl:icubrt(N),
			case naive_list(N, B, eratos:sieve(B)) of
				{B, N, []} -> split:hart(N, min(B, ?DEFAULT_LIMIT));
				{_, _, [H | _]} -> H;
				[H | _] -> H
			end;
		{true, Square_root} -> [Square_root, Square_root]
	end.

% some test values from Lehman article:
% 1123877887715932507 -> 299155897 (k=23220)
% 1129367102454866881 -> 25869889 (k=6750)
% 29742315699406748437 -> 372173423 (k=25982)
lehman(N) ->
	B = numerl:icubrt(N),
	case naive_list(N, B, eratos:sieve(B)) of
		{B, N, []} -> split:lehman_odd(N, B, 1);
		{_, _, [H | _]} -> H;
		[H | _] -> H
	end.

% Pollard rho algorithm
rho(N) -> rho(N, -1).

rho(N, B) -> rho(N, B, 2).

rho(N, B, Start) ->
	Fun = fun(X) -> (X * X + B) rem N end,
	Y = Fun(Start),
	split:rho(N, Fun, Start, Y, numerl:gcd(N, abs(Y - Start))).

brent(N) -> brent(N, -1).

brent(N, B) -> brent(N, B, 2).

brent(N, B, Start) ->
	Fun = fun(X) -> (X * X + B) rem N end,
	split:brent(N, Fun, Start, 2, 1, 2).

% Pollard p-1 first stage algorithm
% TODO: what is a right default value for B ?
pollard(N, B) -> pollard(N, B, 2).

pollard(N, B, S) -> pollard(N, B, S, eratos:sieve(B), 1).


pollard(N, B, S, [H | P], L) when L * H > B -> pollard(N, B, S, P, 1);
pollard(N, B, S, [H|_] = P, L) -> pollard(N, B, numerl:ipowm(S,H,N), P, L*H);
pollard(N, B, S, [], _) ->
	case numerl:gcd(S - 1, N) of
		1 -> {stage2, B, S};
		N -> fail;
		G -> G
	end.

%%%
%%% implementation
%%%

odd(N, Lim, Div) when N band 1 =:= 1 -> odd_3(N, Lim, Div);
odd(N, Lim, Div) -> odd(N bsr 1, Lim, [2 | Div]).

odd_3(N, Lim, Div) when N rem 3 =:= 0 -> odd_3(N div 3, Lim, [3 | Div]);
odd_3(N, Lim, Div) ->
	case split:odd(N, Lim, 5, 2) of
		{odd, ok, [Factor], Cont} ->
			{N_N, N_Div} = reduce(N div Factor, [Factor | Div]),
			odd_(N_N, Cont, N_Div);
		{odd, fail, N, Lim, _} -> {partial, Lim, N, Div}
	end.

odd_(N, Cont, Div) ->
	case split:odd_cont(N, Cont) of
		{odd, ok, [Factor], N_Cont} ->
			{N_N, N_Div} = reduce(N div Factor, [Factor | Div]),
			odd_(N_N, N_Cont, N_Div);
		{odd, fail, N, Lim, _} -> {partial, Lim, N, Div}
	end.

naive_list(N, Lim) when N =< 16#FFFFFFFFFFFFFFFF -> odd(N, Lim);
naive_list(N, Lim) -> naive_list(N, Lim, eratos:sieve(Lim)).

naive_list_(N, Cont, Div) ->
	case split:naive_cont(N, Cont) of
		{naive, ok, [Factor], N_Cont} ->
			{N_N, N_Div} = reduce(N div Factor, [Factor | Div]),
			naive_list_(N_N, N_Cont, N_Div);
		{naive, fail, N, Lim, _} -> {fail, Lim, N, lists:reverse(Div)};
		{naive, fail, N, Lim} -> final(N, Lim, hd(Div), Div) % Todo: get last P
	end.

final(1, _, _, Acc) -> lists:reverse(Acc);
final(N, Limit, Factor, Acc) ->
	case Factor * Factor of
		P when P > N -> lists:reverse([N | Acc]);
		_ -> {fail, Limit, N, lists:reverse(Acc)}
	end.

reduce(N, [F | T]) when N rem F =:= 0 -> reduce(N div F, [F, F | T]);
reduce(N, L) -> {N, L}.
