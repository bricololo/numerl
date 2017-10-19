-module(misc).

-export([fib/1, fibm/2]).

% Fibonacci
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> element(1, fast_fib(N)).

% modular Fibonacci
fibm(0, _) -> 0;
fibm(1, _) -> 1;
fibm(N, M) -> element(1, fast_fibm(N, M)).

%%%
%%% implementation
%%%

% fast Fibonacci, returns {F_n, F_{n+1}}
% fast_fib(0) -> {0, 1};
fast_fib(1) -> {1, 1};
fast_fib(2) -> {1, 2};
fast_fib(N) ->
	{A, B} = fast_fib(N bsr 1),
	A2 = A * A,
	B2 = B * B,
	P = A * B,
	case N band 1 of
		0 -> {P bsl 1 - A2, A2 + B2};
		1 -> {A2 + B2, P bsl 1 + B2}
	end.

fast_fibm(1, _) -> {1, 1};
fast_fibm(2, _) -> {1, 2};
fast_fibm(N, M) ->
	{A, B} = fast_fibm(N bsr 1, M),
	A2 = A * A,
	B2 = B * B,
	P = A * B,
	case N band 1 of
		0 -> {pos_rem(P bsl 1 - A2, M), pos_rem(A2 + B2, M)};
		1 -> {pos_rem(A2 + B2, M), pos_rem(P bsl 1 + B2, M)}
	end.

pos_rem(X, M) ->
	case X rem M of
		R when R < 0 -> R + M;
		R -> R
	end.
