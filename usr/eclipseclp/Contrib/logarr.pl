
%   File   : LOGARR.PL
%   Author : Mostly David H.D.Warren, some changes by Fernando Pereira
%   Updated: 24 September 1984
%   Purpose: Extendable arrays with logarithmic access time.

/*  LOGARITHMIC ARRAYS.

    An array extends from 0 to 2**Size - 1, where Size is a multiple of 2.
    Note that 2**Size = 1<<Size.

    External interface.

    new_array(A) returns a new empty array A.

    is_array(A) checks whether A is an array.

    aref(Index,Array,Element) unifies Element with Array[Index],
	or fails if Array[Index] has not been set.

    arefa(Index,Array,Element) is as aref/3, except that it unifies
	Element with a new array if Array[Index] is undefined.
	This is useful for multidimensional arrays implemented
	as arrays of arrays.

    arefl(Index,Array,Element) is as aref/3, except that Element
	appears as '[]' for undefined cells.

    aset(Index,Array,Element,NewArray) unifies NewArray with the
	result of setting Array[Index] to Element.

    array_to_list(Array,List) returns a list of pairs Index-Element
	of all the elements of Array that have been set.

	In the interests of uniformity, R.A.O'K used the Prolog source
	code tidier on this file; this is not its original layout.  He
	made no algorithmic changes, however.
*/

:- module(logarr).			% SEPIA header
:- export
	new_array/1,
	is_array/1,
	aref/3,
	arefa/3,
	arefl/3,
	aset/4,
	array_to_list/2.

/*
:- mode
	aref(+, +, ?),
	arefa(+, +, ?),
	arefl(+, +, ?),
	array_to_list(+, -),
	aset(+, +, +, -),
	array_item(+, +, +, +, ?),
	is_array(+),
	new_array(-),
	not_undef(+),
	subarray(+, +, ?),
	subarray_to_list(+, +, +, +, ?, ?),
	update_subarray(+, +, ?, ?, -).
*/

new_array(array($($,$,$,$),2)).


is_array(array(_,_)).


aref(Index, array(Array,Size), Item) :-
	check_int(Index),
	Index < 1<<Size,
	N is Size-2,
	Subindex is (Index>>N) /\ 3,
	array_item(Subindex, N, Index, Array, Item).


array_to_list(array($(A0,A1,A2,A3),Size), L0) :-
	N is Size-2,
	subarray_to_list(0, N, 0, A0, L0, L1),
	subarray_to_list(1, N, 0, A1, L1, L2),
	subarray_to_list(2, N, 0, A2, L2, L3),
	subarray_to_list(3, N, 0, A3, L3, []).


arefa(Index, array(Array,Size), Item) :-
	check_int(Index),
	Index < 1<<Size,
	N is Size-2,
	Subindex is (Index>>N) /\ 3,
	array_item(Subindex, N, Index, Array, Item), !.
arefa(_, _, Item) :-
	new_array(Item).


arefl(Index, array(Array,Size), Item) :-
	check_int(Index),
	Index < 1<<Size,
	N is Size-2,
	Subindex is (Index>>N) /\ 3,
	array_item(Subindex, N, Index, Array, Item), !.
arefl(_, _, []).


aset(Index, array(Array0,Size0), Item, array(Array,Size)) :-
	check_int(Index),
	enlarge_array(Index, Size0, Array0, Size, Array1),
	update_array_item(Size, Index, Array1, Item, Array).


check_int(I) :-
	integer(I), !.
check_int(X) :-
	write('Array index not integer: '), write(X), nl,
%	trace,
	fail.

% Guts

enlarge_array(I, Size, Array, Size, Array) :-
	I < 1<<Size, !.
enlarge_array(I, Size0, Array0, Size, Array) :-
	Size1 is Size0 + 2,
	Array1 = $(Array0,$,$,$),
	enlarge_array(I, Size1, Array1, Size, Array).


array_item(0, 0, _, $(Item,_,_,_), Item) :- !,
	not_undef(Item).
array_item(0, N, Index, $(Array,_,_,_), Item) :-
	N1 is N-2,
	Subindex is (Index >> N1) /\ 3,
	array_item(Subindex, N1, Index, Array, Item).
array_item(1, 0, _, $(_,Item,_,_), Item) :- !,
	not_undef(Item).
array_item(1, N, Index, $(_,Array,_,_), Item) :-
	N1 is N-2,
	Subindex is (Index >> N1) /\ 3,
	array_item(Subindex, N1, Index, Array, Item).
array_item(2, 0, _, $(_,_,Item,_), Item) :- !,
	not_undef(Item).
array_item(2, N, Index, $(_,_,Array,_), Item) :-
	N1 is N-2,
	Subindex is (Index >> N1) /\ 3,
	array_item(Subindex, N1, Index, Array, Item).
array_item(3, 0, _, $(_,_,_,Item), Item) :- !,
	not_undef(Item).
array_item(3, N, Index, $(_,_,_,Array), Item) :-
	N1 is N-2,
	Subindex is (Index >> N1) /\ 3,
	array_item(Subindex, N1, Index, Array, Item).


not_undef($) :- !,
	fail.
not_undef(_).


%% [BEFORE OPEN-CODING 'subarray']
%%
%% array_item(0,Index,Item,Item) :- !,
%%	not_undef(Item).
%% array_item(N,Index,Array,Item) :-
%%	N1 is N-2,
%%	Subindex is (Index >> N1) /\ 3,
%%	subarray(Subindex,Array,Array1),
%%	array_item(N1,Index,Array1,Item).
%%
%% subarray(0,$(X,_,_,_),X).
%% subarray(1,$(_,X,_,_),X).
%% subarray(2,$(_,_,X,_),X).
%% subarray(3,$(_,_,_,X),X).

update_array_item(0, _, _, NewItem, NewItem) :- !.
update_array_item(N, Index, Array, NewItem, NewArray) :-
	N1 is N-2,
	Subindex is (Index >> N1) /\ 3,
	update_subarray(Subindex, Array, Array1, NewArray1, NewArray),
	update_array_item(N1, Index, Array1, NewItem, NewArray1).


update_subarray(I, $, X, X1, Array) :- !,
	update_subarray(I, $($,$,$,$), X, X1, Array).
update_subarray(0, $(W,X,Y,Z), W, W1, $(W1,X,Y,Z)).
update_subarray(1, $(W,X,Y,Z), X, X1, $(W,X1,Y,Z)).
update_subarray(2, $(W,X,Y,Z), Y, Y1, $(W,X,Y1,Z)).
update_subarray(3, $(W,X,Y,Z), Z, Z1, $(W,X,Y,Z1)).


subarray_to_list(K, 0, M, Item, [N-Item|L], L) :-
	not_undef(Item), !,
	N is K+M.
subarray_to_list(K, N, M, $(A0,A1,A2,A3), L0, L) :-
	N > 0, !,
	N1 is N-2,
	M1 is (K+M) << 2,
	subarray_to_list(0, N1, M1, A0, L0, L1),
	subarray_to_list(1, N1, M1, A1, L1, L2),
	subarray_to_list(2, N1, M1, A2, L2, L3),
	subarray_to_list(3, N1, M1, A3, L3, L).
subarray_to_list(_, _, _, _, L, L).
