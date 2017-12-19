-module(pq).

-export([new/2, val/1, add/2, bump/2]).

new(Cur, Next) -> {Cur, Next, empty}.

leaf(P) -> {P, nil, nil}.

path(N) -> path(N, []).

path(1, L) -> L;
path(N, L) -> path(N bsr 1, [N band 1 | L]).

% add a new element to the priority queue. The priority queue is managed as a
% heap, so:
% Creating the root of the heap
add({Cur, Next, empty}, Elem) -> {Cur, Next, 1, leaf(Elem)};
% adding the Sz + 1 element to the tree T at the position given by path/1
add({Cur,Next,Size,T}, Elem) -> {Cur, Next, Size+1, add(T,Elem,path(Size+1))}.

% adding an element by creating an empty subtree as a leaf
add(nil, Elem, []) -> leaf(Elem);
% going through tre Left (resp. Right) subtree until reaching the missing leaf
add({P, L, R}, Elem, [0 | Path]) -> {P, add(L, Elem, Path), R};
add({P, L, R}, Elem, [1 | Path]) -> {P, L, add(R, Elem, Path)}.

val({_, _, empty}) -> nothing;
val({Cur, _, _, {P, _, _}}) -> Cur(P).

% remove the composite N from the tree T of size Sz, the size of the tree will
% not change as a composite removed is replaced by a bigger composite.
bump({Cur, Next, Sz, T}, N) -> {Cur, Next, Sz, bumpt(T, N, Cur, Next)}.

% a composite might be present several times (for example 2431 will be present
% 3 times, as it is multiple of 11, 13 and 17). So we remove V from T until the
% smallest composite in T is larger than V.
bumpt({P, L, R} = T, V, Cur, Next) ->
	case Cur(P) of
		Vp when Vp =< V -> bumpt(fix({Next(P), L, R}, Cur), V, Cur, Next);
		_ -> T
	end.

% adjust heap by swapping primes down until it's a heap again
fix({_, nil, nil} = T, _) -> T;
fix({P, {Pl, nil, nil}, nil} = T, Cur) ->
	case {Cur(P), Cur(Pl)} of
		{V, Vl} when V < Vl -> T;
		_ -> {Pl, {P, nil, nil}, nil}
	end;
fix({P, {Pl, Ll, Rl} = L, {Pr, Lr, Rr} = R} = T, Cur) ->
	case {Cur(P), Cur(Pl), Cur(Pr)} of
		{V, Vl, Vr} when V < Vl, V < Vr -> T;
		{_, Vl, Vr} when Vl < Vr -> {Pl, fix({P, Ll, Rl}, Cur), R};
		_ -> {Pr, L, fix({P, Lr, Rr}, Cur)}
	end.
