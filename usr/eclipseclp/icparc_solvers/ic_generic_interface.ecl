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
% Contributor(s): Andrew Sadler and Warwick Harvey, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: ic_generic_interface.ecl,v 1.3 2013/02/13 00:58:47 jschimpf Exp $
%
% Description:		Generic interface to IC library
%
% Author:		A.Sadler, IC-Parc
%			W.Harvey, IC-Parc
%
% The IC implementation of the generic solver interface for global
% constraints.
%
% Notes regarding the IC implementation of the generic interface:
%
% - The generic suspension condition `any' is mapped to `min', `max' and
%   `hole'.
%
% ----------------------------------------------------------------------

:- module(ic_generic_interface).

:- lib(ic_kernel).
:- lib(ic).

    %
    % Export the generic interface.
    %

:- export
	get_lwb/2,
	get_upb/2,
	get_bounds/3,
	get_finite_bounds/3,
	get_domain/2,
	get_compact_domain_as_list/2,
	get_compact_domain_rep/2,
	get_subtract_domain_rep/2,
	get_full_domain_as_list/2,
	get_size/2,
	get_constraints_number/2,
	is_integer_type/1,
	check_in/2,
	lwb/2,
	upb/2,
	excl/2,
	domain_union/4,
	subtract_domain/2.


    %
    % Set up module name translations.
    %

trans_module_names(sbds_module, ic_sbds).

:- export trans_module_names/2.
:- export macro(sbds_module/0, trans_module_names/2, []).


    %
    % Set up translations of eval/1 
    % (use evaluate to allow eval/1 in non-constraint context)

trans_eval(evaluate(X), eval(X)).

:- export trans_eval/2.
:- export macro(evaluate/1, trans_eval/2, []).

    %
    % Define the transformations to be done for the generic interface
    % predicates.
    %

tr_ic_in(get_lwb(Handle, Lwb),
		get_bounds(Handle, Lwb, _Upb)).	% Will be transformed further
tr_ic_in(get_upb(Handle, Upb),
		get_bounds(Handle, _Lwb, Upb)).	% Will be transformed further
tr_ic_in(get_bounds(Handle, Lwb, Upb),
		ic:get_integer_bounds(Handle, Lwb, Upb)).
tr_ic_in(get_finite_bounds(Handle, Lwb, Upb),
		ic:get_finite_integer_bounds(Handle, Lwb, Upb)).
tr_ic_in(get_domain(Handle, Domain),
		Domain = Handle).
tr_ic_in(get_compact_domain_rep(Var, Rep),
		ic:get_domain(Var, Rep)).
tr_ic_in(get_subtract_domain_rep(Var, Rep),
		get_compact_domain_as_list(Var, Rep)).
tr_ic_in(get_full_domain_as_list(Var, DomList),
		ic:get_domain_as_list(Var, DomList)).
tr_ic_in(get_size(Handle, Size),
		ic:get_domain_size(Handle, Size)).
tr_ic_in(get_constraints_number(Var, Number),
		ic:delayed_goals_number(Var, Number)).
tr_ic_in(lwb(Var, Val),
		ic_kernel:impose_min(Var, Val)).
tr_ic_in(upb(Var, Val),
		ic_kernel:impose_max(Var, Val)).
tr_ic_in(excl(Var, Val),
		% Like ic_kernel:exclude(Var, Val), but doesn't require
		% Var to be a finite integer variable.
		% Would be nice to have a more direct implementation than
		% this?
		ic:(Var =\= Val)).
tr_ic_in(check_in(Val, Var),
         ic:is_in_domain(Val, Var)).


    %
    % Set up compile-time inlining of the generic interface predicates.
    % Note that this has to be done after the definition of tr_fd_in/2 so
    % that we can exploit the transformations in the rest of this file.
    % 

:- inline(get_lwb/2, tr_ic_in/2).
:- inline(get_upb/2, tr_ic_in/2).
:- inline(get_bounds/3, tr_ic_in/2).
:- inline(get_finite_bounds/3, tr_ic_in/2).
:- inline(get_domain/2, tr_ic_in/2).
:- inline(get_compact_domain_rep/2, tr_ic_in/2).
:- inline(get_subtract_domain_rep/2, tr_ic_in/2).
:- inline(get_full_domain_as_list/2, tr_ic_in/2).
:- inline(get_size/2, tr_ic_in/2).
:- inline(get_constraints_number/2, tr_ic_in/2).
:- inline(lwb/2, tr_ic_in/2).
:- inline(upb/2, tr_ic_in/2).
:- inline(excl/2, tr_ic_in/2).
:- inline(check_in/2, tr_ic_in/2).

    %
    % Almost "dummy" (but not really) versions of interface predicates for
    % when the above transformations have not occurred.  Note that the
    % bodies are transformed, giving the code we want without having to
    % duplicate it here.
    %

get_lwb(Handle, Lwb) :-
	get_lwb(Handle, Lwb).

get_upb(Handle, Upb) :-
	get_upb(Handle, Upb).

get_bounds(X, L, H) :-
	get_bounds(X, L, H).

get_finite_bounds(X, L, H) :-
	get_finite_bounds(X, L, H).

get_domain(Handle, Domain) :-
	get_domain(Handle, Domain).

get_compact_domain_rep(Var, Rep) :-
	get_compact_domain_rep(Var, Rep).

get_subtract_domain_rep(Var, Rep) :-
	get_subtract_domain_rep(Var, Rep).

get_full_domain_as_list(Var, DomList) :-
	get_full_domain_as_list(Var, DomList).

get_size(Var, Size) :-
	get_size(Var, Size).

get_constraints_number(Var, Number) :-
	get_constraints_number(Var, Number).

lwb(Var, Val) :-
	lwb(Var, Val).

upb(Var, Val) :-
	upb(Var, Val).

excl(Var, Val) :-
	excl(Var, Val).

check_in(Val, Var) :-
        check_in(Val, Var).


    %
    % Implement the generic interface predicates which do not have a natural
    % counterpart in IC and which we choose not to inline.
    %

get_compact_domain_as_list(Var, List) :-
	get_compact_domain_rep(Var, Rep),
	( Rep = [_ | _] ->
	    List = Rep
	;
	    List = [Rep]
	).

is_integer_type(Var) :-
	integer(Var).
is_integer_type(Var) :-
	var(Var),
	ic:get_solver_type(Var, integer).

domain_union(Dom1, Dom2, DomUnion, DomUnionSize) :-
	msg(Dom1, Dom2, DomUnion),
        get_size(DomUnion, DomUnionSize).

domain_to_list(Dom, List) :-
	get_domain_as_list(Dom, List).

subtract_domain(Var, DomList) :-
	(
	    foreach(X, DomList),
	    param(Var)
	do
	    ( X = L..H ->
		exclude_range(Var, L, H)
	    ;
		exclude(Var, X)
	    )
	),
	wake.

