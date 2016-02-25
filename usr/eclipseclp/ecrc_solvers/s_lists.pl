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

%----------------------------------------------------------------------
:- module(s_lists).
%----------------------------------------------------------------------

:- reexport(fd).
:- use_module(library(ordset)).

:- export macro(no_macro_expansion({}/1), tr_slists/2,[]).
:- export macro(no_macro_expansion({}/0), tr_slists/2,[]).
:- set_event_handler(129, true/0).
:- export macro(s_ /1, tr_s_lists/2,[write]).

%tr_s_lists(Var, Var) :-
%	var(Var), !.
tr_s_lists(s_([]), no_macro_expansion({})) :-!.
tr_s_lists(s_([s_(L)]), no_macro_expansion({{C}})) :-
	!,
	list2conj(L, C).
tr_s_lists(s_(L), no_macro_expansion({C})) :-
	list2conj(L, C).

tr_slists(no_macro_expansion({}), Set) :-
	!,
	list2set([], Set).
tr_slists(no_macro_expansion({}(Conjunction)), Set) :-
	conj2list(Conjunction, List, []),
	list2set(List, Set).

:- export tr_s_lists/2.
:- export tr_slists/2.

:- export s_intersection/3, s_union/3, s_member/2, s_equality/2,
	    s_included/2, set/1, s_insertion/3, s_delta/3, s_card/2,
	    list2set/2, set2list/2, s_memberchk/2, s_remove/3, s_dis/2,
	    s_weight/2, s_weight_discr/2.

%----------------------------------------------------------------------
:- pragma(nodebug).

:- import sdelta/3 from sepia_kernel.

%:- Preds = (s_intersection/3, s_union/3, s_member/2, s_equality/2,
%	    s_included/2, set/1, s_insertion/3, s_delta/3, s_card/2,
%	    list2set/2, set2list/2, s_memberchk/2, s_remove/3, s_dis/2,
%	    s_weight/2, s_weight_discr/2),
%   skipped(Preds).


% macro to improve performance
tr_slist_set(slist_set(L, S), S = s_(L)).
:- inline(slist_set/2, tr_slist_set/2).

:- mode slist_set(?, ?).
slist_set(List, s_(List)).


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
	ord_insert(L1, Element, L2),
	slist_set(L2, S2).


:- mode set(++).
set(s_(_)) ?- true.

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



:- mode s_member(?, ++).
s_member(Element, Set) :-
	slist_set(List, Set),
	member(Element, List).

:- mode s_memberchk(++, ++). /* change carmen */
s_memberchk(Element, Set) :-
	slist_set(List, Set),
	memberchk(Element, List).

:- mode s_intersection(++, +, ?). /* change carmen ++ -> + */
s_intersection(S1, S2, S) :-
	slist_set(L1, S1),
	slist_set(L2, S2),
	ord_intersect(L1, L2, L),
	slist_set(L, S).

:- mode s_delta(++, ++, ?).
s_delta(S1, S2, S) :-
	slist_set(L1, S1),
	slist_set(L2, S2),
%	ord_subtract(L1, L2, L),
	sdelta(L1, L2, L),
	slist_set(L, S).

:- mode s_remove(++, ++, ?).
s_remove(Element, S1, S2) :-
	slist_set(L1, S1),
	sdelta(L1, [Element], L2),
	slist_set(L2, S2).


:- mode s_union(++, ++, ?).
s_union(S1, S2, S) :-
	slist_set(L1, S1),
	slist_set(L2, S2),
	ord_union(L1, L2, L),
	slist_set(L, S).

:- mode s_dis(++, ++).
s_dis(S1,S2) :-
	slist_set(L1, S1),
	slist_set(L2, S2),
	ord_disjoint(L1, L2).


:- mode s_equality(++, ++).
s_equality(X, X).


:- mode s_included(++, ++).
s_included(S1, S2) :-
	s_delta(S1,S2,{}).


/* computes the weight of a ground set by summing up its elements
according to the argument value */

:- mode s_weight(++, ?).
s_weight(S, W) :-
	slist_set(L,S),
	s_weightsum(L, 0, W).

:- mode s_weightsum(++, +, ?).
s_weightsum([], W, W).
s_weightsum([E|L], W0, W) :-
	( E = e(_, WE) ->
	    W1 is W0 + WE,
	    s_weightsum(L, W1, W)
	;
	    s_weightsum(L, W0, W)
	).


:- mode s_weight_discr(++, ?).
s_weight_discr(S, Wdom) :-
	slist_set(L, S),
	list_weights(L,Wdom).

list_weights([], []).
list_weights([ A | L], [Arg | LW]) :-
	arg(2, A, Arg),
	list_weights(L, LW).

