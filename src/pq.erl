%% This module implement a priority queue as a heap
%% being aimed at being used by the callback modules of sieve it present some
%% specificities :
%%  - Getting the priority of a given element is done via the Cur/1 function
%%  - the top priority is the least value
%%  - insertions are assumed to be done in decreasing priority order
%%
%% The Cur function could be avoided by using tuples of fixed size with the
%% priority in first position, that would probably speed up the fix function
%% when rebalancing the tree.

-module(pq).

-export([new/2, val/1, add/2, bump/1, bump/2, bumpt/4]).

new(Cur, Next) -> {Cur, Next, empty}.

val({_, _, empty}) -> nothing;
val({Cur, _, _, {P, _, _}}) -> Cur(P).

% add a new element to the priority queue. The priority queue is managed as a
% heap tree, so:
% Creating the root of the heap
add({Cur, Next, empty}, Elem) -> {Cur, Next, 1, leaf(Elem)};
% adding the Size + 1 element to the heap Heap at the position given by path/1
add({Cur, Next, Size, Heap}, Elem) ->
	{Cur, Next, Size + 1, add(Heap, Elem, path(Size + 1))}.

% remove N from the heap Heap of size Size, the size of the heap will not change
% as an element removed is replaced by a bigger one
bump({Cur, Next, Size, Heap}, N) ->
	{Cur, Next, Size, bumpt(Heap, N, Cur, Next)}.

bump({Cur, Next, Size, {Head, Left, Right}}) ->
	{Cur, Next, Size, fix({Next(Head), Left, Right}, Cur)}.

% an element might be present several times So we replace Head by its next value
% until the smallest element in Heap is larger than V.
bumpt({Head, _, _} = Heap, V, Cur, Next) ->
	case Cur(Head) of
		Vp when Vp =< V ->
			bumpt(fix(setelement(1, Heap, Next(Head)), Cur), V, Cur, Next);
		_ -> Heap
	end.

%%
%% implementation
%%

leaf(Elem) -> {Elem, nil, nil}.

path(N) -> path(N, []).

path(1, L) -> L;
path(N, L) -> path(N bsr 1, [N band 1 | L]).

add(nil, Elem, []) -> leaf(Elem);
add({_, Left, _} = H, Elem, [0 | Path]) ->
	setelement(2, H, add(Left, Elem, Path));
add({_, _, Right} = H, Elem, [1 | Path]) ->
	setelement(3, H, add(Right, Elem, Path)).

% adjust heap by swapping elements down until it's a heap again
fix({_, nil, nil} = T, _) -> T;
fix({Head, {Left, nil, nil}, nil} = Heap, Cur) ->
	case {Cur(Head), Cur(Left)} of
		{V, Vl} when V < Vl -> Heap;
		_ -> {Left, {Head, nil, nil}, nil}
	end;
fix({Head, {Hl, Ll, Rl} = Left, {Hr, Lr, Rr} = Right} = Heap, Cur) ->
	case {Cur(Head), Cur(Hl), Cur(Hr)} of
		{V, Vl, Vr} when V < Vl, V < Vr -> Heap;
		{_, Vl, Vr} when Vl < Vr -> {Hl, fix({Head, Ll, Rl}, Cur), Right};
		_ -> {Hr, Left, fix({Head, Lr, Rr}, Cur)}
	end.
