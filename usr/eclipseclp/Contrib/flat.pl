:- module(flat).

:- local flatten/2.

:- export

	binary_to_list/4,
	binary_to_list/5,
	list_to_binary/3,
	list_to_binary/4,

	and_to_list/2,
	list_to_and/2,
	or_to_list/2,
	list_to_or/2,
	plus_to_list/2,
	list_to_plus/2,
	times_to_list/2,
	list_to_times/2.


%   File   : FLAT.PL
%   Author : R.A.O'Keefe
%   Updated: 5 April 1984
%   Converted for NIP: Ken Johnson, 1-6-87
%   Purpose: Flatten various binary trees to lists and convert back.

/*  This file was originally for PRESS, where you often want to take
    a tree such as 1+x+0+(u*v+9)+(x^2+2) and flatten it to a list
    such as [1,x,u*v,9,x^2,2] so that you can easily pick out all the
    constants or all the terms involving x or something, without having
    to write N different sets of predicates to handle N different
    binary operators.  It can be useful for other things as well.

    The <operator>_to_list predicates take a binary tree (where leaf
    nodes are anything not labelled by the operator) and flatten it
    to a list.  They also omit "units" of that operator, that is, if
    the operator is & {| + *} the constant true {false 0 1} will not
    appear in the list.  The predicate
	binary_to_list(Tree, Operator, Unit, Before, After)
    enables you to make your own versions.  Note that the answer is
    accumulated in the differnce Before-After.
	binary_to_list(Tree, Operator, Before, After)
    lets you convert trees where the operator has no unit.

    The well known and not often useful predicate "flatten" is a not
    very interesting special case of binary_to_list/5.

    The list_to_<operator> predicates take a list and turn it back
    into a tree.  Now there is an interesting question here: is
    [a,b,c] to be turned into f(a,f(b,c)) or into f(f(a,b),c)?  The
    former is a good idea for & | and '.', while the latter is a
    good idea for + and *.  My solution was to have the top-level
    predicate check whether the Operator is a yfx operator (such as
    + and * are) and if so to generate f(f(a,b),c).  In all other
    cases (xfy,xfx, or no operator declaration) f(a,f(b,c)) is
    generated.
	list_to_binary(List, Operator, Unit, Tree)
    lets you make your own versions.  If the list is [] the Unit will
    be returned, that is the only use of the Unit.
	list_to_binary(List, Operator, Tree)
    should be used when the Operator has no Unit, if given an empty
    list it will fail.
*/

%   Example 1
%   binary_to_list(1+2+3,Op,B,A)
%   sets
%   Op = +
%   B = [1,2,3|_1]
%   A = _1

%   Example 2
%   list_to_binary([1,2,3],+,A) and list_to_binary([1,2,3|_],+,A)
%   both set
%   A = 1+2+3

and_to_list(Conjunction, List) :-
	binary_to_list(Conjunction, &, true, List, []).


list_to_and(List, Conjunction) :-
	list_to_binary(List, &, true, Conjunction).



or_to_list(Disjunction, List) :-
	binary_to_list(Disjunction, (';'), false, List, []).


list_to_or(List, Disjunction) :-
	list_to_binary(List, (';'), false, Disjunction).



plus_to_list(Sum, List) :-
	binary_to_list(Sum, +, 0, List, []).


list_to_plus(List, Sum) :-
	list_to_binary(List, +, 0, Sum).



times_to_list(Product, List) :-
	binary_to_list(Product, *, 1, List, []).


list_to_times(List, Product) :-
	list_to_binary(List, *, 1, Product).



flatten(List, Flat) :-
	binary_to_list(List, ., [], Flat, []).



binary_to_list(Unit, _, Unit, List, List) :- !.
binary_to_list(Term, Operator, Unit, Before, After) :-
	Term =.. [Operator,Lhs,Rhs],	% Term can't be a variable
	!,
	binary_to_list(Lhs, Operator, Unit, Before, Middle),
	binary_to_list(Rhs, Operator, Unit, Middle, After).
binary_to_list(Term, _, _, [Term|After], After).


binary_to_list(Term, Operator, Before, After) :-
	nonvar(Term),
	Term =.. [Operator,Lhs,Rhs],
	!,
	binary_to_list(Lhs, Operator, Before, Middle),
	binary_to_list(Rhs, Operator, Middle, After).
binary_to_list(Term, _, [Term|After], After).



list_to_binary([], _, Unit, Unit) :- !.
list_to_binary([Head|Tail], Operator, _, Answer) :-
	current_op(_, yfx, Operator),
	!,
	list_to_binaryL(Tail, Operator, Head, Answer).
list_to_binary(List, Operator, _, Answer) :-
	list_to_binaryR(List, Operator, Answer).


list_to_binary([Head|Tail], Operator, Answer) :-
	current_op(_, yfx, Operator),
	!,
	list_to_binaryL(Tail, Operator, Head, Answer).
list_to_binary(List, Operator, Answer) :-
	list_to_binaryR(List, Operator, Answer).


list_to_binaryL([], _, Answer, Answer) :- !.
list_to_binaryL([Head|Tail], Operator, Sofar, Answer) :-
	Next =.. [Operator,Sofar,Head],
	list_to_binaryL(Tail, Operator, Next, Answer).


list_to_binaryR([Term], _, Term) :- !.
list_to_binaryR([Head|Tail], Operator, Answer) :-
	Answer =.. [Operator,Head,Rest], !,
	list_to_binaryR(Tail, Operator, Rest).


