-module(misc).

-export([fib/1, fibm/2, fast_fibm/2, lucas/1, lucas/3, lucasm/2, lucasm/4]).
-export([fact/1]).

-define(N60, 1152921504606846976).

% Fibonacci
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> element(1, fast_fib(N)).

% modular Fibonacci
fibm(0, _) -> 0;
fibm(1, _) -> 1;
fibm(N, M) -> element(1, fast_fibm(N, M)).

% modular fast Fibonacci
fast_fibm(1, _) -> {1, 1};
fast_fibm(2, _) -> {1, 2};
fast_fibm(N, M) ->
	{A, B} = fast_fibm(N bsr 1, M),
	A2 = A * A rem M,
	B2 = B * B,
	P = (A * B) bsl 1,
	case N band 1 of
		0 ->
			{(P - A2) rem M + case P >= A2 of true -> 0; _ -> M end,
			(A2 + B2) rem M};
		1 -> {(A2 + B2) rem M, (P + B2) rem M}
	end.

% Nth lucas number
lucas(N) -> lucas(2, 1, N).

% Nth lucas number of parameter A, B
lucas(A, _, 0) -> A;
lucas(_, B, 1) -> B;
lucas(A, B, N) ->
	{F1, F2} = fast_fib(N - 1),
	A * F1 + B * F2.

lucasm(N, M) -> lucasm(2, 1, N, M).

lucasm(A, _, 0, M) -> A rem M;
lucasm(_, B, 1, M) -> B rem M;
lucasm(A, B, N, M) ->
	{F1, F2} = fast_fibm(N - 1, M),
	(A * F1 + B * F2) rem M.

% Factorial
fact(0) -> 1;
fact(1) -> 1;
fact(N) when N band 1 =:= 1 -> N * fact(N - 1);
fact(N) -> fact(N, N - 2, 1, []).

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

fact(V, 0, P, Acc) -> fact([P * V | Acc], []);
fact(V, I, P, Acc) when P < ?N60 -> fact(V + I, I - 2, P * V, Acc);
fact(V, I, P, Acc) -> fact(V, I, 1, [P | Acc]).

fact([F, S | T], Acc) -> fact(T, [F * S | Acc]);
fact([R], []) -> R;
fact([], Acc) -> fact(Acc, []);
fact([R], Acc) -> fact([R | Acc], []).
