%% This module implement a priority queue as a skew tree where the top
%% priority is the least value.
%% As I don't need it, the function remove_min/1 is missing.
%% For my use the priority of an element is equal to its value but bumpt/4 is
%% the only function to rely on that fact so it could easily be adressed if
%% needed.

-module(pq_skew).

-export([new/1, val/1, add/2, bump/2, bumpt/3]).

new(Next) -> {Next, empty}.

val({_, {P, _, _}}) -> element(1, P);
val({_, empty}) -> nothing.

add({Next, Tree}, Element) -> {Next, merge(Tree, leaf(Element))}.

bump({Next, Tree}, N) -> {Next, bumpt(Tree, N, Next)}.

bumpt(Tree, V, Next) -> bumpt(Tree, V, empty, Next).

%%
%% implementation
%%

leaf(Elem) -> {Elem, empty, empty}.

% swapping Left and Right help balancing the tree
merge({Hl, Ll, Rl}, {Hr, _, _} = Right) when Hl =< Hr ->
	{Hl, Rl, merge(Ll, Right)};
merge({_, _, _} = Left, {Hr, Lr, Rr}) -> {Hr, Rr, merge(Lr, Left)};
merge(Tree, empty) -> Tree;
merge(empty, Tree) -> Tree.

% an element might be present several times. So we replace Head by its next value
% until the smallest element in Tree is larger than V.
bumpt({Head, Left, Right}, V, Add, Next) when element(1, Head) =< V ->
	bumpt(merge(Left, Right), V, merge(Add, leaf(Next(Head))), Next);
bumpt(Tree, _, Add, _) -> merge(Tree, Add).
