%----------------------------------------------------------------------
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
% The Original Code is  The Zinc Modelling Interface for ECLiPSe
% The Initial Developer of the Original Code is  Joachim Schimpf
% Portions created by the Initial Developer are
% Copyright (C) 2007 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf
% 
% END LICENSE BLOCK
%----------------------------------------------------------------------

:- module(fzn_eplex).

:- comment(categories, ["Interfacing","Constraints"]).
:- comment(summary, "Mapping from FlatZinc to lib(eplex)").
:- comment(author, "Joachim Schimpf, supported by Cisco Systems and NICTA Victoria").
:- comment(copyright, "Cisco Systems Inc, licensed under CMPL").
:- comment(date, "$Date: 2009/07/16 09:11:24 $").
:- comment(see_also, [library(flatzinc),library(eplex)]).
:- comment(desc, html("
This module defines a mapping from FlatZinc operations to lib(eplex)
and is intended to be used in conjunction with lib(flatzinc).
</P><P>
TODO:	Add a mapping for sets as array of booleans.
Add an appropriate globals.mzn.
<P>
")).


% Lazy person's way of exporting (almost) all locally defined predicates:
:- local initialization(
    (
	current_module_predicate(local, Pred),
	get_flag(Pred, auxiliary, off),
	nonmember(Pred, [eplex_opt/3, vector_sum/3]),
	export(Pred),
	fail
    ;
	true
    )).

:- lib(eplex).			% the actual solver library

:- import
	fzn_write/2,
	fzn_error/2
    from flatzinc.


% Declarations -----------------------------------------------

% Single variable declarations
bool_declare(X) :- X $:: 0..1, integers(X).
int_declare(X) :- integers(X).
float_declare(X) :- reals([X]).
int_declare(X, Min, Max) :- X $:: Min..Max, integers(X).
float_declare(X, Min, Max) :- X $:: Min..Max.

% Variable array declarations
% Unfortunately not all primitives can handle arrays
bool_declare_array(Xs) :- ( foreacharg(X,Xs) do X $:: 0..1, integers(X) ).
int_declare_array(Xs) :- ( foreacharg(X,Xs) do integers(X) ).
float_declare_array(Xs) :- ( foreacharg(X,Xs) do reals(X) ).
int_declare_array(Xs, Min, Max) :-
	( foreacharg(X,Xs), param(Min,Max) do X $:: Min..Max, integers(X) ).
float_declare_array(Xs, Min, Max) :- 
	( foreacharg(X,Xs), param(Min,Max) do X $:: Min..Max ).


% Comparisons -----------------------------------------------

% int_??(var int, var int)
int_eq(X, Y) :- X $= Y.
int_le(X, Y) :- X $=< Y.
int_ge(X, Y) :- X $>= Y.

% float_??(var float, var float)
float_eq(X, Y) :- X $= Y.
float_le(X, Y) :- X $=< Y.
float_ge(X, Y) :- X $>= Y.

% bool_??(var bool, var bool)
bool_eq(X, Y) :- X $= Y.
bool_le(X, Y) :- X $=< Y.
bool_ge(X, Y) :- X $>= Y.


% int_lin_??(array[int] of int, array[int] of var int, int)
int_lin_eq(Cs, Xs, Rhs) :- vector_sum(Cs, Xs, CXs), sum(CXs) $= Rhs.
int_lin_le(Cs, Xs, Rhs) :- vector_sum(Cs, Xs, CXs), sum(CXs) $=< Rhs.
int_lin_ge(Cs, Xs, Rhs) :- vector_sum(Cs, Xs, CXs), sum(CXs) $>= Rhs.

% float_lin_??(array[int] of float, array[int] of var float, float)
float_lin_eq(Cs, Xs, Rhs) :- vector_sum(Cs, Xs, CXs), sum(CXs) $= Rhs.
float_lin_le(Cs, Xs, Rhs) :- vector_sum(Cs, Xs, CXs), sum(CXs) $=< Rhs.
float_lin_ge(Cs, Xs, Rhs) :- vector_sum(Cs, Xs, CXs), sum(CXs) $>= Rhs.

    vector_sum(Cs, Xs, CXs) :-
	arity(Cs, N),
	( arity(Xs, N) ->
	    ( for(I,1,N), foreach(C*X,CXs), param(Cs,Xs) do
		arg(I, Cs, C), arg(I, Xs, X)
	    )
	;
	    fzn_error("Mismatched vector product %w %w", [Cs, Xs])
	).


% Arithmetic operations -----------------------------------------------

% int_???(var int, var int, var int)
int_negate(X, Z) :- Z $= -X.
int_plus(X, Y, Z) :- Z $= X+Y.
int_minus(X, Y, Z) :- Z $= X-Y.

% float_???(var float, var float, var float)
float_negate(X, Z) :- Z $= -X.
float_plus(X, Y, Z) :- Z $= X+Y.
float_minus(X, Y, Z) :- Z $= X-Y.


% Logical operations -----------------------------------------------


% Set operations -----------------------------------------------


% Array operations -----------------------------------------------


% Coercion operations -----------------------------------------------

int2float(I, F) :- ( var(I) -> F=I ; F is float(I) ).
bool2int(X, X).


% FZN data <-> ECLiPSe data -----------------------------------------------

bool_fzn_to_solver(false, 0).
bool_fzn_to_solver(true, 1).

bool_solver_to_fzn(0, false).
bool_solver_to_fzn(1, true).

% model floats are just solver floats
float_fzn_to_solver(X, X) :- float(X).

float_solver_to_fzn(X, X) :- real(X).

% Set constants are ordered lists (pars only)
set_fzn_to_solver(List, Set) :-
	sort(List, Set).

set_solver_to_fzn(List, List) :- is_list(List).

% Set constants are ordered lists (pars only)
range_fzn_to_solver(Min, Max, Set) :-
	( for(I,Min,Max), foreach(I,Set) do true ).


% Search -----------------------------------------------

satisfy(Anns) :-
	eplex_opt(min(0), Anns, _Cost).

minimize(Expr, Anns, Cost) :-
	eplex_opt(min(Expr), Anns, Cost).

maximize(Expr, Anns, Cost) :-
	eplex_opt(max(Expr), Anns, Cost).

    eplex_opt(Obj, _Anns, Cost) :-
	eplex_solver_setup(Obj),
	eplex_solve(Cost),
	eplex_get(vars, Vars),
	eplex_get(typed_solution, Vals),
	eplex_cleanup,
	Vars=Vals.

