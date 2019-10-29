-module(factor_util).

-export([l/1, factor_base/2, sqrt_m/2 ]).

l(N) ->
	Ln = math:log(N),
	math:exp(math:sqrt(Ln * math:log(Ln))).

factor_base(B, Filter) -> [P || P <- tl(eratos:sieve(B)), Filter(P)].

% find X such that X * X = A (P) given that P is prime and jacobi(A, P) = 1 by
% using Tonelli algorithm.
sqrt_m(V, P) ->
	A = V rem P,
	case P band 7 of
		1 -> sqrt_m_1(A, P);
		5 ->
			X = numerl:ipowm(A, P bsr 3 + 1, P),
			case X * X rem P of
				A -> X;
				_ -> X * numerl:ipowm(2, P bsr 2, P) rem P
			end;
		_ -> numerl:ipowm(A, P bsr 2 + 1, P)
	end.

sqrt_m_1(A, P) ->
	S = num_util:p2(P - 1),
	T = P bsr S,
	Y = non_square(2, P bsr 1, P),
	D = numerl:ipowm(Y, T, P),
	M = m(1, S, 0, numerl:ipowm(A, T, P), D, P),
	(numerl:ipowm(A, (T + 1) bsr 1, P) * numerl:ipowm(D, M bsr 1, P)) rem P.


m(I, S, M, _, _, _) when I >= S -> M;
m(I, S, M, A, D, P) ->
	R = (A * numerl:ipowm(D, M, P)) rem P,
	R1 = numerl:ipowm(R, 1 bsl (S - I - 1), P),
	N = case R1 of V when V =:= P - 1 -> M + numerl:ipow(2, I); _ -> M end,
	m(I + 1, S, N, A, D, P).

non_square(Y, Lim, _) when Y > Lim -> fail;
non_square(Y, Lim, P) -> 
	case numerl:jacobi(Y, P) of
		-1 -> Y;
		_ -> non_square(Y + 1, Lim, P)
	end.
