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
% Copyright (C) 1998 - 2010 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Kish Shen (based on ic_generic_interface)
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: gfd_generic_interface.ecl,v 1.3 2013/02/13 01:00:43 jschimpf Exp $
%
% Description:		Generic interface to gfd library
%
% Author:		Kish Shen
%
% The gfd implementation of the generic solver interface 
%
%
% ----------------------------------------------------------------------

:- module(gfd_generic_interface).

%:- lib(gfd).
:- use_module(gfd).

    %
    % Export the generic interface.
    %

:- export
	get_lwb/2,
	get_upb/2,
	get_finite_bounds/3,
%	get_domain/2,
	get_compact_domain_as_list/2,
	get_compact_domain_rep/2,
%	get_subtract_domain_rep/2,
%	get_full_domain_as_list/2,
	get_size/2,
%	get_constraints_number/2,
	is_integer_type/1,
	check_in/2,
	lwb/2,
	upb/2,
	excl/2.
%	domain_union/4,
%	subtract_domain/2,

    %
    % Set up module name translations.
    %
trans_module_names(sbds_module, gfd_sbds).

:- export trans_module_names/2.
:- export macro(sbds_module/0, trans_module_names/2, []).

    %
    % Set up translations of eval/1 
    % (use evaluate to allow eval/1 in non-constraint context)

trans_eval(evaluate(X), X).

:- export trans_eval/2.
:- export macro(evaluate/1, trans_eval/2, []).

    %
    % Define the transformations to be done for the generic interface
    % predicates.
    %


tr_gfd_in(get_lwb(Var, Lwb),
		get_min(Var, Lwb)).
tr_gfd_in(get_upb(Var, Upb),
		get_max(Var, Upb)).
tr_gfd_in(get_finite_bounds(Var, Lwb, Upb),
		get_bounds(Var, Lwb, Upb)).
/*
tr_gfd_in(get_domain(Handle, Domain),
		Domain = Handle).
*/
tr_gfd_in(get_compact_domain_rep(Var, Rep),
		get_domain(Var, Rep)).
tr_gfd_in(get_compact_domain_as_list(Var, Rep),
		get_domain(Var, Rep)).
/*tr_gfd_in(get_subtract_domain_rep(Var, Rep),
		get_compact_domain_as_list(Var, Rep)).*/
tr_gfd_in(get_full_domain_as_list(Var, DomList),
		gfd:get_domain_as_list(Var, DomList)).
tr_gfd_in(get_size(Var, Size),
		get_domain_size(Var, Size)).
tr_gfd_in(lwb(Var, Val),
		impose_min(Var, Val)).
tr_gfd_in(upb(Var, Val),
		impose_max(Var, Val)).
tr_gfd_in(excl(Var, Val),
		exclude(Var, Val)).
tr_gfd_in(check_in(Val, Var),
         gfd:is_in_domain(Val, Var)).
tr_gfd_in(is_integer_type(Var),
         gfd:is_solver_type(Var)).


    %
    % Set up compile-time inlining of the generic interface predicates.
    % Note that this has to be done after the definition of tr_fd_in/2 so
    % that we can exploit the transformations in the rest of this file.
    % 

:- inline(get_lwb/2, tr_gfd_in/2).
:- inline(get_upb/2, tr_gfd_in/2).
:- inline(get_finite_bounds/3, tr_gfd_in/2).
%:- inline(get_domain/2, tr_gfd_in/2).
:- inline(get_compact_domain_rep/2, tr_gfd_in/2).
:- inline(get_compact_domain_as_list/2, tr_gfd_in/2).
%:- inline(get_subtract_domain_rep/2, tr_gfd_in/2).
:- inline(get_full_domain_as_list/2, tr_gfd_in/2).
:- inline(get_size/2, tr_gfd_in/2).
%:- inline(get_constraints_number/2, tr_gfd_in/2).
:- inline(lwb/2, tr_gfd_in/2).
:- inline(upb/2, tr_gfd_in/2).
:- inline(excl/2, tr_gfd_in/2).
:- inline(check_in/2, tr_gfd_in/2).
:- inline(is_integer_type/1, tr_gfd_in/2).

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

get_finite_bounds(X, L, H) :-
	get_finite_bounds(X, L, H).
/*
get_domain(Handle, Domain) :-
	get_domain(Handle, Domain).
*/

get_compact_domain_rep(Var, Rep) :-
	get_compact_domain_rep(Var, Rep).


get_compact_domain_as_list(Var, Rep) :-
	get_compact_domain_as_list(Var, Rep).
/*
get_subtract_domain_rep(Var, Rep) :-
	get_subtract_domain_rep(Var, Rep).
*/
get_full_domain_as_list(Var, DomList) :-
	get_full_domain_as_list(Var, DomList).

get_size(Var, Size) :-
	get_size(Var, Size).

/* implemented directly in gfd
get_constraints_number(Var, Number) :-
	get_constraints_number(Var, Number).
*/
lwb(Var, Val) :-
	lwb(Var, Val).

upb(Var, Val) :-
	upb(Var, Val).

excl(Var, Val) :-
	excl(Var, Val).

check_in(Val, Var) :-
        check_in(Val, Var).

is_integer_type(Var) :-
        is_integer_type(Var).



    %
    % Implement the generic interface predicates which do not have a natural
    % counterpart in gfd and which we choose not to inline.
    %



domain_union(Dom1, Dom2, DomUnion, DomUnionSize) :-
	msg(Dom1, Dom2, DomUnion),
        get_size(DomUnion, DomUnionSize).

domain_to_list(Dom, List) :-
	get_domain_as_list(Dom, List).
/*
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
*/
