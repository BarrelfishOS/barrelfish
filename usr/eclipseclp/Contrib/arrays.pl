
:- module(arrays).			% SEPIA header
:- export
	array_length/2,
	array_to_list/2,
	fetch/3,
	list_to_array/2,
	store/4.


%   File   : ARRAYS.PL
%   Author : R.A.O'Keefe
%   Updated: 8 November 1983
%   Purpose: Updatable arrays in Prolog.

%   In this package an array is represented in the following notation
%   {1,2,3} is shown initially as
%   array([1|_1],[2|_2],[3|_3])+0
%   where the [1|_1] etc. are lists in which the most recent value for
%   the element comes last, and the +0 is a zero updates count.
%   Updating the 2nd element to a 5 for example would make the array into
%   array([1|_1],[2,5|_2],[3|_3])+1
%   Function "list_to_array" converts list e.g. [1,2,3,4] to array in
%   this format, "array_to_list" converts back.
%   fetch(+N,+A,-Elem)      Fetches Nth element
%   store(+N,+A,+Elem,+NewA)    Stores Elem at Nth element of A giving NewA

/*  These operations are fully described in
    "Updatable Arrays in Prolog", R.A.O'Keefe, DAI Working Paper 150.
    Note that store(Index, Old, Elem, New) sometimes side-effects Old and
    sometimes doesn't; you cannot rely on Old remaining unchanged.
    (i.e. uninstantiated arguments in Old may become instantiated.)
    This is NOT an example of logic programming.  For a logic programming
    solution (with cost O(lgN) rather O(1)) see Trees.Pl.
*/

array_length(Array+_, Length) :-
    functor(Array, array, Length).


array_to_list(Array+_, List) :-
    Array =.. [array|Wrapped],
    un_wrap(Wrapped, List).

un_wrap([History|Histories], [Element|Elements]) :-
    get_last(History, Element),
    !,
    un_wrap(Histories, Elements).

un_wrap([], []).


fetch(Index, Array+_, Element) :-
    arg(Index, Array, History),
    get_last(History, Element).

get_last([Head|Tail], Element) :-
    var(Tail),
    !,
    Element = Head.

get_last([_|Tail], Element) :-
    get_last(Tail, Element).


list_to_array(List, Array+0) :-
    wrap_up(List, Wrapped),
    Array =.. [array|Wrapped].

wrap_up([Element|Elements], [[Element|_]|Wrapped]) :- !,
    wrap_up(Elements, Wrapped).

wrap_up([], []).


store(Index, Array+Updates, Element, NewArray+NewUpdates) :-
    functor(Array, array, Length),
    arg(Index, Array, History),
    put_last(History, Element),
    K is Updates+1,
    !,
    store(Length, K, NewUpdates, Array, NewArray).

store(N, N, 0, Old, New) :- !,
    Old =.. [array|OldList],
    un_wrap(OldList, MidList),
    !,
    wrap_up(MidList, NewList),
    New =.. [array|NewList].

store(_, U, U, Array, Array).

put_last(History, Element) :-
    var(History),
    !,
    History = [Element|_].
put_last([_|History], Element) :-
    put_last(History, Element).

