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
% Version:	$Id: fd_generic_interface.ecl,v 1.3 2013/02/13 00:58:47 jschimpf Exp $
%
% Description:		Generic interface to FD library
%
% Author:		A.Sadler, IC-Parc
% 			W.Harvey, IC-Parc
%
% The FD implementation of the generic solver interface for global
% constraints.
% ----------------------------------------------------------------------

:- module(fd_generic_interface).

:- lib(fd).

    %
    % Export the generic interface.
    %

:- export
	get_lwb/2,
	get_upb/2,
	get_bounds/3,
	get_finite_bounds/3,
	get_domain/2,
	get_compact_domain_rep/2,
	get_compact_domain_as_list/2,
        get_full_domain_as_list/2,
	get_subtract_domain_rep/2,
	get_size/2,
	get_constraints_number/2,
	is_integer_type/1,
	check_in/2,
	lwb/2,
	upb/2,
	excl/2,
	empty_domain/1,
	domain_union/4,
	subtract_domain/2.


    %
    % Set up module name translations.
    %

trans_module_names(sbds_module, fd_sbds).

:- export trans_module_names/2.
:- export macro(sbds_module/0, trans_module_names/2, []).

    %
    % Set up translations of eval/1 
    % (use evaluate to allow eval/1 in non-constraint context)

trans_eval(evaluate(X), (X)).

:- export trans_eval/2.
:- export macro(evaluate/1, trans_eval/2, []).

    %
    % Define the transformations to be done for the generic interface
    % predicates.
    %

tr_fd_in(get_lwb(Handle, Lwb),
		fd:dvar_range(Handle, Lwb, _)).
tr_fd_in(get_upb(Handle, Upb),
		fd:dvar_range(Handle, _, Upb)).
tr_fd_in(get_bounds(X, L, H),
		( fd:dvar_range(X, L, H) ->
		    true
		;
		    fd:default_domain(X),
		    fd:dvar_range(X, L, H)
		)).
tr_fd_in(get_finite_bounds(X, L, H),
		get_bounds(X, L, H)).	% Will be transformed further
tr_fd_in(get_domain(Var, Dom),
		fd:dvar_domain(Var, Dom)).
tr_fd_in(get_compact_domain_rep(Var, Rep),
		fd:(Var :: Rep)).
tr_fd_in(get_compact_domain_as_list(Var, Rep),
		get_compact_domain_rep(Var, Rep)).
tr_fd_in(get_subtract_domain_rep(Dom, Rep),
		Rep = Dom).
tr_fd_in(get_full_domain_as_list(Var, DomList), 
                fd:dom(Var,DomList)).
tr_fd_in(get_size(Var, Size), (
		    fd:dvar_domain(Var, Dom),
		    fd:dom_size(Dom, Size)
		)).
tr_fd_in(get_constraints_number(Var, Number),
		fd:constraints_number(Var, Number)).
tr_fd_in(check_in(Val, Var), (
		    fd:dvar_domain(Var, Dom),
		    fd:dom_check_in(Val, Dom)
		)).
tr_fd_in(lwb(X, Y),
		fd:dvar_remove_smaller(X, Y)).
tr_fd_in(upb(X, Y),
		fd:dvar_remove_greater(X, Y)).
tr_fd_in(excl(X, Y),
		fd:dvar_remove_element(X, Y)).
tr_fd_in(empty_domain(Empty),
		Empty = EmptyDomain) :-
	sorted_list_to_dom([], EmptyDomain).
tr_fd_in(domain_union(Dom1, Dom2, DomUnion, DomUnionSize),
		fd:dom_union(Dom1, Dom2, DomUnion, DomUnionSize)).
tr_fd_in(subtract_domain(Var, Dom), (
		    fd:dvar_domain(Var, VarDom),
		    fd:dom_difference(VarDom, Dom, NewDom, _),
		    fd:dvar_update(Var, NewDom)
		)).


    %
    % Set up compile-time inlining of the generic interface predicates.
    % Note that this has to be done after the definition of tr_fd_in/2 so
    % that we can exploit the transformations in the rest of this file.
    % 

:- inline(get_lwb/2, tr_fd_in/2).
:- inline(get_upb/2, tr_fd_in/2).
:- inline(get_bounds/3, tr_fd_in/2).	% Not worth inlining?
:- inline(get_finite_bounds/3, tr_fd_in/2).
:- inline(get_domain/2, tr_fd_in/2).
:- inline(get_compact_domain_rep/2, tr_fd_in/2).
:- inline(get_compact_domain_as_list/2, tr_fd_in/2).
:- inline(get_subtract_domain_rep/2, tr_fd_in/2).
:- inline(get_full_domain_as_list/2, tr_fd_in/2).
:- inline(get_size/2, tr_fd_in/2).
:- inline(get_constraints_number/2, tr_fd_in/2).
:- inline(check_in/2, tr_fd_in/2).
:- inline(lwb/2, tr_fd_in/2).
:- inline(upb/2, tr_fd_in/2).
:- inline(excl/2, tr_fd_in/2).
:- inline(empty_domain/1, tr_fd_in/2).
:- inline(domain_union/4, tr_fd_in/2).
:- inline(subtract_domain/2, tr_fd_in/2).


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

get_domain(Var, Dom) :-
	get_domain(Var, Dom).

get_compact_domain_rep(Var, Rep) :-
	get_compact_domain_rep(Var, Rep).

get_compact_domain_as_list(Var, Rep) :-
	get_compact_domain_as_list(Var, Rep).

get_subtract_domain_rep(Var, Dom) :-
	get_subtract_domain_rep(Var, Dom).

get_full_domain_as_list(Var, List) :-
        get_full_domain_as_list(Var, List).


get_size(Var, Size) :-
	get_size(Var, Size).

get_constraints_number(Var, Number) :-
	get_constraints_number(Var, Number).

check_in(Val, Var) :-
	check_in(Val, Var).

lwb(X, Y) :-
	lwb(X, Y).

upb(X, Y) :-
	upb(X, Y).

excl(X, Y) :-
	excl(X, Y).

empty_domain(Empty) :-
	empty_domain(Empty).

domain_union(Dom1, Dom2, DomUnion, DomUnionSize) :-
	domain_union(Dom1, Dom2, DomUnion, DomUnionSize).

subtract_domain(Var, Dom) :-
	subtract_domain(Var, Dom).


    %
    % Implement the generic interface predicates which do not have a natural
    % counterpart in FD and which we choose not to inline.
    %

is_integer_type(X) :-
	integer(X).
is_integer_type(X) :-
	var(X),
	is_integer_domain(X).

