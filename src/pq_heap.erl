%% This module implement a priority queue as a heap where the top priority is
%% the least value.
%% As I don't need it, the function remove_min/1 is missing.
%% For my use the priority of an element is equal to its value but bumpt/3 is
%% the only function to rely on that fact so it could easily be adressed if
%% needed.

-module(pq_heap).

-export([new/1, val/1, add/2, bump/1, bump/2, bumpt/3]).

new(Next) -> {Next, 0, empty}.

val({_, _, {P, _, _}}) -> element(1, P);
val({_, 0, empty}) -> nothing.

add({Next, Size, Heap}, Elem) -> {Next, Size+1, add(Heap, Elem, path(Size+1))}.

bump({Next, Size, Heap}, N) -> {Next, Size, bumpt(Heap, N, Next)}.

bump({Next, Size, {Head, Left, Right}}) ->
	{Next, Size, fix({Next(Head), Left, Right})}.

% an element might be present several times So we replace Head by its next value
% until the smallest element in Heap is larger than V.
bumpt({Head, _, _} = Heap, V, Next) ->
	case element(1, Head) of
		Vp when Vp =< V -> bumpt(fix(setelement(1, Heap, Next(Head))), V, Next);
		_ -> Heap
	end.

%%
%% implementation
%%

leaf(Elem) -> {Elem, empty, empty}.

path(N) -> path(N, []).

path(1, L) -> L;
path(N, L) -> path(N bsr 1, [N band 1 | L]).

add(empty, Elem, []) -> leaf(Elem);
add({_,Left,_} = H, Elem, [0|Path]) -> setelement(2, H, add(Left,Elem,Path));
add({_,_,Right} = H, Elem, [1|Path]) -> setelement(3, H, add(Right,Elem,Path)).

% adjust heap by swapping elements down until it's a heap again
fix({Head, {Left, empty, empty}, empty} = Heap) ->
	case Head < Left of
		true -> Heap;
		_ -> {Left, {Head, empty, empty}, empty}
	end;
fix({Head, {Hl, Ll, Rl} = Left, {Hr, Lr, Rr} = Right} = Heap) ->
	case {Head, Hl, Hr} of
		{V, Vl, Vr} when V < Vl, V < Vr -> Heap;
		{_, Vl, Vr} when Vl < Vr -> {Hl, fix({Head, Ll, Rl}), Right};
		_ -> {Hr, Left, fix({Head, Lr, Rr})}
	end;
fix({_, empty, empty} = Tree) -> Tree.
