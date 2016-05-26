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
% Copyright (C) 1998 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Vassilis Liatsos, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: not_equals.pl,v 1.1 2006/09/23 01:53:51 snovello Exp $
%
% Emacs:    --*-prolog-*--
% not_equals.pl - 
%
% Author:   vassilis liatsos 
% 
% Purpose:  asking whether two variables can  
%           unify or not, taking disequality
%           constraints into account
%
% Usage:   
%     ne/2   asserts a disequality constraint
%     neqt/2  tests whether variables are not equal
%
% This succeeds with the following queries:
%
% ne(X,Y),not_unify(X,Y).
% ne(X,a),not_unify(X,a).
% ne(a,b).
%
% and fails with:
%
% ne(X,X).
% ne(a,a).
% ne(X,Y),X=Y.
% ne(X,a),X=a.
%
% Date: February 98
% ----------------------------------------------------------------------

:- pragma(expand).

:- module(not_equals).


:- export
        ne/2,
        neqt/2.

:- begin_module(not_equals).
:- lib(structures).

:- import
        add_attribute/3,
        setarg/3
    from sepia_kernel.

:- meta_attribute(not_equals,[
                    unify:unify_ne/2,
		    test_unify: test_unify_ne/2,
		    print:print_ne/2]).

:- define_struct(not_equals(ne)).

ne(X,Y):-
	nonvar(X),
	nonvar(Y),!,
	X\=Y.

ne(X,Y):-
	(X==Y -> 
	    fail
        ;
	    (var(X) ->
		get_ne_attr(X,AX),
		AX = not_equals with ne:NeX,
		sort([Y|NeX],NewX),
		setarg(ne of not_equals,AX,NewX)
	    ;
	        true
	    ),
	    
	    (var(Y) ->
		get_ne_attr(Y,AY),
		AY = not_equals with ne:NeY,
		sort([X|NeY],NewY),
		setarg(ne of not_equals,AY,NewY)
	    ;
	        true
	    )
	).

% neqt(+X,+Y)
% if X\=Y can be deduced then succeed
% otherwise fail
 
neqt(X,Y):-
	not_unify(X,Y).

/*****************************
      UTILITY PREDICATES
******************************/

get_ne_attr(X{A},Attr):-
	-?->
	get_ne_attr1(X,Attr,A).
get_ne_attr(X,Attr):-
	free(X),
	new_ne_attr(X,Attr).

get_ne_attr1(X,Attr,A):-
	var(A),new_ne_attr(X,Attr).
get_ne_attr1(_,Attr,A):-
	nonvar(A),Attr = A.

new_ne_attr(X,Attr):-
	Attr = not_equals with ne:[],
	add_attribute(X,Attr).

mem_check(X,[Y|_]):- X==Y,!.
mem_check(X,[_|R]):- mem_check(X,R).

/*****************************
          HANDLERS 
******************************/
		
% unify_ne(+Term, Attribute)
unify_ne(_, Attr):-
        /*** ANY + VAR ***/
        var(Attr).             % Ignore if no attributes for this extension
unify_ne(Term, Attr):-
        compound(Attr),
        unify_term_ne(Term, Attr).

unify_term_ne(Value, Attr):-
        nonvar(Value),         % The metaterm was instantiated
	Attr = not_equals with ne: List,
        /*** NONVAR + META ***/
        not mem_check(Value,List).
unify_term_ne(Y{AttrY},AttrX):-
        -?->
        unify_ne_ne(Y,AttrX,AttrY).

unify_ne_ne(_, AttrX, AttrY):-
        var(AttrY),            % no attribute for this extension
        /*** VAR + META ***/
        AttrX = AttrY.
unify_ne_ne(Y, AttrX, AttrY):-
        nonvar(AttrY),
        /*** META + META ***/
        AttrX = not_equals with ne:NeX,
	AttrY = not_equals with ne:NeY,
	not mem_check(Y,NeX),
	append(NeX,NeY,NeXY),
	sort(NeXY,New),
	setarg(ne of not_equals,AttrY,New).

% test_unify_ne(+Term, Attribute)
test_unify_ne(_, Attr):-
        /*** ANY + VAR ***/
        var(Attr).             % Ignore if no attributes for this extension
test_unify_ne(Term, Attr):-
        compound(Attr),
        test_unify_term_ne(Term, Attr).

test_unify_term_ne(Value, Attr):-
        nonvar(Value),         % The metaterm was instantiated
	Attr = not_equals with ne:List,
        /*** NONVAR + META ***/
        not mem_check(Value,List).
test_unify_term_ne(Y{AttrY},AttrX):-
        -?->
        test_unify_ne_ne(Y,AttrX,AttrY).

test_unify_ne_ne(_, _, AttrY):-
        /*** VAR + META ***/
        var(AttrY).            % no attribute for this extension

test_unify_ne_ne(Y, AttrX, AttrY):-
        nonvar(AttrY),
        /*** META + META ***/
        AttrX = not_equals with ne:NeX,
	not mem_check(Y,NeX).

print_ne(not_equals(NE),Print):- 
        -?->
        Print = ( ne: NE).
