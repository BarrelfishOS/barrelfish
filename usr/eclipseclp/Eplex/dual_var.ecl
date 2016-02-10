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
%-----------------------------------------------------------------------------
:- module(dual_var).
%-----------------------------------------------------------------------------

:- export dual_var_print/2.

:- meta_attribute(dual_var, [print:dual_var_print/2, unify:unify_dual/2]).
:- export struct(dual_var(dual_val, coeff, eplex_idx, type,
                          primal_rhs, primal_lhs_range, susps, solver, next)).

:- export var_dual/7.
:- export get_dual/3.
:- export get_coeff/3.

:- export always_set_dual/3.

:- export set_dual/3.
:- export get_idx/3.
:- export get_rhs/3.
:- export get_lhs_range/3.
:- export set_lhs_range/3.
:- export get_type/3.
:- export satisfiable_primal_cstr/2.

%-----------------------------------------------------------------------------


% dual_var attribute handlers

unify_dual(_, Attribute) :-
    /* NOT A DUAL-VAR */
    var(Attribute).
unify_dual(Term, Attribute) :-
    /* DUAL-VAR */
    compound(Attribute),
    unify_term_dual(Term, Attribute).

unify_term_dual(Value, _) :-
    /* DUAL-VAR and NONVAR - instantiated */
    nonvar(Value),
    true.
unify_term_dual(Y{AttrY}, AttrX) :-
    -?->
    /* DUAL-VAR and VAR */
    unify_dual_dual(Y, AttrY, AttrX).

unify_dual_dual(Y, AttrY, AttrX) :-
    /* DUAL-VAR and NON-DUAL-VAR - share attribute */
    var(AttrY),
    AttrX = AttrY,
    add_attribute(Y, AttrY).
unify_dual_dual(_, AttrY, AttrX) :-
    /* DUAL-VAR and DUAL-VAR - add them */
    % but what to do about eplex indexes?
    % probably something like eplex chain of solver ids
    nonvar(AttrY),
    AttrY = dual_var with [dual_val:DualY],
    AttrX = dual_var with [dual_val:DualX],
    merge_suspension_lists(susps of dual_var, AttrX,
			   susps of dual_var, AttrY),
    (DualY = DualX ->
         true
    ;
         NewDual is DualY + DualX,
         setarg(dual_val of dual_var, AttrY, NewDual),
         schedule_suspensions(susps of dual_var, AttrY),
         wake
    ).

dual_var_print(_{Attr}, Printed) :-
    -?->
    nonvar(Attr),
    printed_dv_attributes(Attr, Printed).

printed_dv_attributes(Attr, Printed) :-
    ( compound(Attr) ->
        Attr = dual_var with [
                              dual_val:Dual,
                              solver:Handle,
                              next:NextAttr
                             ],
        Printed = [Handle:[dual_val:Dual]|Rest],
        printed_dv_attributes(NextAttr, Rest)
    ;
        % chain terminated by atom end
        Printed = []
    ).

% ----------------------------------------------------------------------

% creating a new dual_var variable

var_dual(Var, Dual, Coeff, Idx, Type, Rhs, Handle) :-
        get_dual_attr(Var, Handle, Attr),
        Attr = dual_var with [
                              dual_val:Dual,
                              coeff:Coeff,
                              eplex_idx:Idx,
                              type:Type,
                              primal_rhs:Rhs
                             ].

get_dual_attr(X{dual_var:Attr0}, Handle, Attr) ?-
        ( var(Attr0) ->
              new_dual_attr(X, Handle, Attr)
        ;
              Attr0 = dual_var with [solver:Handle0, next:Next],
              % should not fail unless Attr0 incorrect
              ( Handle0 == Handle ->
                    Attr = Attr0
              ;
                    get_dual_attr1(Next, Attr0, Handle, Attr)
              )
        ).
get_dual_attr(X, Handle, Attr) :-           % make a new dual_var variable
        free(X),
        new_dual_attr(X, Handle, Attr).

get_dual_attr1(ThisAttr, Attr0, Handle, Attr) :-
	atom(ThisAttr), !, % chain terminated by atom 'end'
	new_dual_attrstruct(Handle, Attr),
	setarg(next of dual_var, Attr0, Attr).
get_dual_attr1(ThisAttr, _Attr0, Handle, Attr) :-
        ThisAttr = dual_var with [solver:Handle0, next:Next],
        ( Handle0 == Handle ->
              Attr = ThisAttr
        ;
              get_dual_attr1(Next, ThisAttr, Handle, Attr)
        ).

new_dual_attr(X, Handle, Attr) :-         % make a new dual_var variable:
        new_dual_attrstruct(Handle, Attr),
        add_attribute(X, Attr, dual_var).

:- mode new_dual_attrstruct(+,-).
new_dual_attrstruct(Handle, Attr) :-
        Attr = dual_var with [
                              solver:Handle,
                              next:end
                             ],
        init_suspension_list(susps of dual_var, Attr).

% From a dual_var attr, searches for the attribute corresponding to
% that for the first argument. Fails if none found. 
get_dual_attr_for_handle(Handle, Attr0, Attr) :-
        compound(Attr0), 
	get_dual_attr_for_handle1(Handle, Attr0, Attr).

get_dual_attr_for_handle1(Handle, Attr0, Attr) :-
        % no need to test for var(Attr0) in chain
        Attr0 = dual_var with [solver:Handle0, next:NextAttr],
	(Handle0 == Handle ->
	     Attr0 = Attr
	;    
	     get_dual_attr_for_handle1(Handle, NextAttr, Attr)
	).

% -------------------------------------------------------------------------

satisfiable_primal_cstr(_{Attr0}, Handle) :-
    -?->
    get_dual_attr_for_handle(Handle, Attr0, Attr),
    Attr = dual_var with [
                          type:Type,
                          primal_rhs:Rhs,
                          primal_lhs_range:Min..Max
                         ],
    ground(Min..Max),
    ( Type == (=<) -> Min =< Rhs
    ; Type == (=:=) -> Min =< Rhs, Max >= Rhs
    ; Type == (>=) -> Max >= Rhs ).

% dual_var attribute setting and testing predicates

get_idx(_{Attr0}, Idx, Handle) :-
    -?->
    get_dual_attr_for_handle(Handle, Attr0, Attr),
    Attr = dual_var with eplex_idx:Idx.

get_rhs(_{Attr0}, Rhs, Handle) :-
    -?->
    get_dual_attr_for_handle(Handle, Attr0, Attr),
    Attr = dual_var with primal_rhs:Rhs.

get_lhs_range(_{Attr0}, Range, Handle) :-
    -?->
    get_dual_attr_for_handle(Handle, Attr0, Attr),
    Attr = dual_var with primal_lhs_range:Range.

set_lhs_range(_{Attr0}, Lo..Hi, Handle) :-
    -?->
    get_dual_attr_for_handle(Handle, Attr0, Attr),
    Attr = dual_var with primal_lhs_range:Lo0..Hi0,
    Lo1 is max(Lo, Lo0),
    Hi1 is min(Hi, Hi0),
    setarg(primal_lhs_range of dual_var, Attr, Lo1..Hi1).

get_type(_{Attr0}, Type, Handle) :-
    -?->
    get_dual_attr_for_handle(Handle, Attr0, Attr),
    Attr = dual_var with type:Type.

get_dual(_{Attr0}, Dual, Handle) :-
    -?->
    get_dual_attr_for_handle(Handle, Attr0, Attr),
    Attr = dual_var{dual_val:Dual}.

always_set_dual(_{Attr0}, Dual, Handle) :-
    -?->
    get_dual_attr_for_handle(Handle, Attr0, Attr),
    Attr = dual_var with dual_val:Old,
    ( Old =:= Dual ->
        true
    ;
        setarg(dual_val of dual_var, Attr, Dual)
    ),
    schedule_suspensions(susps of dual_var, Attr),
    wake.

set_dual(_{Attr0}, Dual, Handle) :-
    -?->
    get_dual_attr_for_handle(Handle, Attr0, Attr),
    Attr = dual_var with dual_val:Old,
    ( Old =:= Dual ->
        true
    ;
        setarg(dual_val of dual_var, Attr, Dual),
        schedule_suspensions(susps of dual_var, Attr),
        wake
    ).

get_coeff(_{Attr0}, Coeff, Handle) :-
    -?->
    get_dual_attr_for_handle(Handle, Attr0, Attr),
    Attr = dual_var{coeff:Coeff}.
