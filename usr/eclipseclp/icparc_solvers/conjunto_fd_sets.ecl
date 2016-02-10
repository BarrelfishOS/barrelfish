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
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK

:- module(conjunto_fd_sets).

:- comment(categories, ["Compatibility","Constraints"]).
:- comment(summary, "lib(conjunto) compatibility wrapper for lib(fd_sets)").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2009/07/16 09:11:25 $").
:- comment(copyright, "Cisco Systems, Inc.").

:- comment(desc, html("<P>
    This is a wrapper for library(fd_sets) to make it more backward
    compatible with its predecessor, the set solver library(conjunto).
    Supported are:
    <UL>
    <LI>the curly-bracket syntax for sets (input only)
    <LI>the backquote-syntax for several constraints (`&lt;&gt;,`&lt;,`::,`=)
    <LI>refine/1
    </UL>
    Not supported are:
    <UL>
    <LI>non-integer set elements
    <LI>el_weight/2, sum_weight/2 (but see fd_sets:weight/2)
    <LI>max_weight/2 (but see fd_sets:insetdomain/4)
    </UL>
</P>")).

:- reexport fd_sets except
	(::)/2.		% because conjunto has `:: and no conflict with fd

% the curly-bracket syntax

:- export macro({} /1, tr_slists/2,[]).
:- export macro({} /0, tr_slists/2,[]).
:- export tr_slists/2.
:- export list2set/2.
:- export set2list/2.

set2list(S, L) :-
	nonvar(S),
	L=S.

list2set(L, S) :-
	sort(L, S).


tr_slists(no_macro_expansion({}), Set) :- !,
	Set=[].
tr_slists(no_macro_expansion({}(CommaSequence)), Set) :-
	comma_to_list(CommaSequence, List, []),
	sort(List, Set),
	!.
tr_slists(no_macro_expansion({}(CommaSequence)), _Set) :-
	printf(warning_output, "Warning: Not a proper set: %w%n",
		no_macro_expansion({}(CommaSequence))),
	fail.

    comma_to_list((X1,X2), List, List0) ?- !,
    	comma_to_list(X1, List, List1),
    	comma_to_list(X2, List1, List0).
    comma_to_list(X1, [X1|List0], List0) :- nonvar(X1).


:- export
	op(700, xfx, `<>),
	op(700, xfx, `=),
	op(700, xfx, `::),
	op(700, xfx, `<).

:- export
	(`<>)/2,		% ?Set1 `<> Set2	disjoint
	(`=)/2,			% ?Set1 `= Set2		equality
	(`<)/2,			% ?Set1 `< Set2		subset
	(`::)/2,		% ?Set1 `:: +Gset..+Gset
	refine/1.		% refine(?Set)

:- inline((`<>)/2, tr_conjunto_fd_sets/2).
:- inline((`=)/2, tr_conjunto_fd_sets/2).
:- inline((`<)/2, tr_conjunto_fd_sets/2).
:- inline((`::)/2, tr_conjunto_fd_sets/2).

tr_conjunto_fd_sets(X `:: Y, fd_sets:(X :: Y)).
tr_conjunto_fd_sets(X `<> Y, X disjoint Y).
tr_conjunto_fd_sets(X `< Y, X subset Y).
tr_conjunto_fd_sets(X `= Y, X sameset Y).

X `:: Y :-
	fd_sets:(X :: Y).

X `<> Y :-
	X disjoint Y.

X `< Y :-
	X subset Y.

X `= Y :-
	X sameset Y.

:- comment(refine/1, [
    summary:"Instantiate Set to a possible value",
    template:"refine(?Set)",
    resat:yes,
    see_also:[insetdomain/4],
    desc:html("
    Instantiate Set to a possible value. The predicate backtracks over
    all possible set instantiations. refine/1 is a special instance of
    the more general insetdomain/4 predicate, and is defined as
<PRE>
    refine(Set) :-
    	insetdomain(Set, any, small_first, in_notin).
</PRE>
")
]).
refine(Set) :-
    	insetdomain(Set, any, small_first, in_notin).

set([]) ?- true.
set([First|Xs]) ?-
	integer(First),
	( foreach(X,Xs), fromto(First,Prev,X,_) do integer(X), Prev < X ).


% for conjunto compatibility
max_weight(X, Weights, Heaviest) :- var(X),
	potential_members(X, Elements),
	find_heaviest(Elements, Weights, Heaviest).
max_weight(X, Weights, Heaviest) :- nonvar(X),
	find_heaviest(X, Weights, Heaviest).	% strange, but as in conjunto

    find_heaviest(List, Weights, Heaviest) :-
    	(
	    foreach(I, List),
	    fromto(-1, MaxW0, MaxW1, _),
	    fromto(_, Best0, Best1, Heaviest),
	    param(Weights)
	do
	    ( Weights[I] > MaxW0 ->
		MaxW1 is Weights[I], Best1 = I
	    ;
		MaxW1 = MaxW0, Best1 = Best0
	    )
	).


glb(S, Glb) :-
	set_range(S, Glb, _).

lub(S, Lub) :-
	set_range(S, _, Lub).

