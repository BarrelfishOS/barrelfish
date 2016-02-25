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
% Contributor(s): 
% 
% END LICENSE BLOCK

:-module(mutable).

:- meta_attribute(mutable,[unify:mutable_unify_handler/2]).
:- export struct(mutable(mut, muted)).


:-tool(suspend_on_change/2, suspend_on_change/3).
:-tool(get_changeable_value/2, get_changeable_value/3).


:-export suspend_on_change/2.
:-export get_changeable_value/2.
:-export mutate/2.


mutable_unify_handler(_, Attr) :-
	var(Attr).

mutable_unify_handler(Term, Attr) :-
	compound(Attr),
	mutable_unify_handler1(Term, Attr).

mutable_unify_handler1(_Var{Attr1}, Attr2) :-
	-?->
	!,
	( var(Attr1) ->
	    Attr1 = Attr2	% transfer the whole attribute
	;
            false
        ).

get_attr(Var{A},Attr):- -?->
        get_attr1(Var,Attr,A).
get_attr(Var,Attr):-
        free(Var),
        new_attr(Var,Attr).

    get_attr1(X, Attr, A) :-
	var(A), new_attr(X, Attr).
    get_attr1(_, Attr, A) :-
	nonvar(A), Attr=A.
        
new_attr(X,Attr) :-
        Attr = (mutable with []),
        init_suspension_list(muted of mutable,Attr),
        add_attribute(X, Attr).

mutate(Var,Val):-
        get_attr(Var,Attr),
        setarg(mut of mutable, Attr, Val),
        schedule_suspensions(muted of mutable, Attr),
        wake.

suspend_on_change(Var,Susp,_Module):-
        get_attr(Var,Attr),
        enter_suspension_list(muted of mutable, Attr, Susp).

get_changeable_value(Var{mutable:Attr}, Val, _Module) :- -?-> !,
        arg(mut of mutable, Attr, Val),
        true.
get_changeable_value(Var, Var, _Module).
        
        
