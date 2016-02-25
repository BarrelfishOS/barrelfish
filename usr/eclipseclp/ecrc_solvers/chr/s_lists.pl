% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Carmen Gervet and Pascal Brisset, ECRC. 
% 
% END LICENSE BLOCK

:- module_interface(s_lists).

:- define_macro({} /1, tr_s_lists/2,[protect_arg]).
:- define_macro({} /0, tr_s_lists/2,[protect_arg]).
:- define_macro(s_ /1, tr_s_lists/2,[write, protect_arg]).

tr_s_lists(s_([]), no_macro_expansion({})).
tr_s_lists(s_(L), no_macro_expansion({C})) :-
	call(list2conj(L, C), s_lists).
tr_s_lists(no_macro_expansion({}), Set) :-
	list2set([], Set).
tr_s_lists(no_macro_expansion({}(Conjunction)), Set) :-
	call(conj2list(Conjunction, List, []), s_lists),
	list2set(List, Set).

:- export tr_s_lists/2.

:- begin_module(s_lists).

:- Preds = (s_intersection/3, s_union/3, s_member/2, s_equality/2,
	    s_included/2, is_set/1, s_insertion/3, s_delta/3, s_card/2,
	    list2set/2, set2list/2, s_memberchk/2, s_remove/3),
   export(Preds),
   skipped(Preds).


:- mode set2list(++, ?).
set2list(S, L) :-
	slist_set(L, S).


:- mode s_card(++, ?).
s_card(Set, Card) :-
	slist_set(List, Set),
	length(List, Card).


:- mode s_insertion(++, ++, ?).
s_insertion(Element, S1, S2) :-
	slist_set(L1, S1),
	sinsertion(Element, L1, L2),
	slist_set(L2, S2).


:- mode s_remove(++, ++, ?).
s_remove(Element, S1, S2) :-
	slist_set(L1, S1),
	sremove(Element, L1, L2),
	slist_set(L2, S2).


sinsertion(Element, [], [Element]).
sinsertion(Element, [Element | L1], [Element | L1]) :- !.
sinsertion(Element, [E1 | L1], [Element, E1 | L1]) :-
	Element @< E1, !.
sinsertion(Element, [E1 | L1], [E1 | L2]) :-
	sinsertion(Element, L1, L2).


sremove(_Element, [], []).
sremove(Element, [Element | L1], L1) :- !.
sremove(Element, [E1 | L1], [E1 | L1]) :-
	Element @< E1, !.
sremove(Element, [E1 | L1], [E1 | L2]) :-
	sremove(Element, L1, L2).


:- mode is_set(++).
is_set(s_(_)) ?- true.


:- mode conj2list(++, -, ?).
conj2list((First, Second), List, Tail) :-
	!,
	conj2list(First, List, Tail1),
	conj2list(Second, Tail1, Tail).
conj2list(Element, [Element | Tail], Tail).


list2conj([X], X) :- !.
list2conj([X | L], (X, C)) :-
	list2conj(L, C).


:- mode list2set(++, -).
list2set(List, Set) :-
	sort(List, SList),
	slist_set(SList, Set).


:- mode slist_set(?, ?).
slist_set(List, s_(List)).


:- mode s_member(?, ++).
s_member(Element, Set) :-
	slist_set(List, Set),
	member(Element, List).

:- mode s_member(++, ++).
s_memberchk(Element, Set) :-
	slist_set(List, Set),
	memberchk(Element, List).

:- mode s_intersection(++, ++, ?).
s_intersection(S1, S2, S) :-
	slist_set(L1, S1),
	slist_set(L2, S2),
	sintersection(L1, L2, L),
	slist_set(L, S).


:- mode s_delta(++, ++, ?).
s_delta(S1, S2, S) :-
	slist_set(L1, S1),
	slist_set(L2, S2),
	sdelta(L1, L2, L),
	slist_set(L, S).


:- mode s_union(++, ++, ?).
s_union(S1, S2, S) :-
	slist_set(L1, S1),
	slist_set(L2, S2),
	sunion(L1, L2, L),
	slist_set(L, S).


sintersection([], _, []) :- !.
sintersection(_, [], []) :- !.
sintersection([E1 | L1], [E2 | L2], L) :-
	sintersection5(E1, L1, E2, L2, L).

sintersection5(E, L1, E, L2, [E | L]) :-
	!,
	sintersection(L1, L2, L).
sintersection5(E1, L1, E2, L2, L) :-
	E1 @< E2, !,
	sintersection(L1, [E2 | L2], L).
sintersection5(E1, L1, _E2, L2, L) :-
	sintersection([E1 | L1], L2, L).


sunion([], L, L) :- !.
sunion(L, [], L) :- !.
sunion([E1 | L1], [E2 | L2], L) :-
	sunion5(E1, L1, E2, L2, L).

sunion5(E, L1, E, L2, [E | L]) :-
	!,
	sunion(L1, L2, L).
sunion5(E1, L1, E2, L2, [E1 | L]) :-
	E1 @< E2, !,
	sunion(L1, [E2 | L2], L).
sunion5(E1, L1, E2, L2, [E2 | L]) :-
	sunion([E1 | L1], L2, L).


sdelta([], _, []) :- !.
sdelta(L, [], L) :- !.
sdelta([E1 | L1], [E2 | L2], L) :-
	sdelta5(E1, L1, E2, L2, L).

sdelta5(E, L1, E, L2, L) :-
	!,
	sdelta(L1, L2, L).
sdelta5(E1, L1, E2, L2, [E1 | L]) :-
	E1 @< E2, !,
	sdelta(L1, [E2 | L2], L).
sdelta5(E1, L1, _E2, L2, L) :-
	sdelta([E1 | L1], L2, L).


:- mode s_equality(++, ++).
s_equality(X, X).


:- mode s_included(++, ++).
s_included(S1, S2) :-
	s_intersection(S1, S2, S),
	s_equality(S1, S).
