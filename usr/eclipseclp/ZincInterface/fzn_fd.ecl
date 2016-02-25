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

:- module(fzn_fd).

:- comment(categories, ["Interfacing","Constraints"]).
:- comment(summary, "Mapping from FlatZinc to lib(fd) and lib(fd_sets)").
:- comment(author, "Joachim Schimpf, supported by Cisco Systems and NICTA Victoria").
:- comment(copyright, "Cisco Systems Inc, licensed under CMPL").
:- comment(date, "$Date: 2015/01/14 01:31:09 $").
:- comment(see_also, [library(flatzinc),
	library(fd),library(fd_sets),library(fd_global),
	library(propia),library(branch_and_bound)]).
:- comment(desc, html("
This module defines a mapping from FlatZinc operations to lib(fd),
lib(fd_sets) and lib(fd_global), and is intended to be used in
conjunction with lib(flatzinc).  It uses lib(propia) to implement
variants of the element constraint that are not supported by lib(fd).
Moreover, lib(branch_and_bound) is used to provide optimization.
</P><P>
This mapping supports bool, integer and set variables.
It does currently not support all constraints in reified form,
in particular set constraints, according to the limitations of
the underlying solvers.
</P><P>
The following extra annotations are supported by this mapping:
<DL>
<DT>annotation strategy(string:s)</DT>
    <DD>the branch-and-bound strategy (default: \"continue\"). Valid names
    are \"continue\", \"restart\", \"dichotomic\", See bb_min/3.</DD>
<DT>annotation delta(float:f)</DT>
    <DD>minimal absolute improvement for branch-and-bound steps (default 1.0).
    See bb_min/3.</DD>
<DT>annotation factor(float:f)</DT>
    <DD>minimal improvement ratio (with respect to the lower cost bound)
    for strategies 'continue' and 'restart' (default 1.0), or split factor
    for strategy 'dichotomic' (default 0.5). See bb_min/3.</DD>
<DT>annotation timeout(float:f)</DT>
    <DD>timeout for branch-and-bound in seconds (default: unlimited).
    See bb_min/3.</DD>
</DL>
You must include \"eclipse.mzn\" in your MiniZinc model to use these
annotations.
<P>
")).


% Lazy person's way of exporting (almost) all locally defined predicates:
:- local initialization(
    (
	current_module_predicate(local, Pred),
	get_flag(Pred, auxiliary, off),
	nonmember(Pred, [
		arg_nd/3,
		array_any_element/3,
		vector_sum/3,
		search_ann/1,
		set_choice/3,
		termify/2
	    ]),
	export(Pred),
	fail
    ;
	true
    )).

:- lib(fd).			% the actual solver libraries
:- lib(fd_search).
:- lib(fd_sets).
:- lib(propia).
:- lib(branch_and_bound).

:- import			% resolve ambiguous imports
	(::)/2,
	intersection/3,
	subset/2,
	union/3
    from fd_sets.

:- import
	fzn_write/2,
	fzn_error/2
    from flatzinc.


% Declarations -----------------------------------------------

% Single variable declarations
bool_declare(X) :- X #:: 0..1.
int_declare(X) :- integers([X]).
int_declare(X, Min, Max) :- X #:: Min..Max.
int_declare(X, Elems) :- X #:: Elems.
set_declare(X, Min, Max) :- intset(X, Min, Max).
set_declare(X, Elems) :- X :: Elems.

% Variable array declarations
% Unfortunately not all primitives can handle arrays
bool_declare_array(Xs) :-
	( foreacharg(X,Xs) do X #:: 0..1 ).
int_declare_array(Xs) :-
	( foreacharg(X,Xs) do integers([X]) ).
int_declare_array(Xs, Min, Max) :-
	( foreacharg(X,Xs), param(Min,Max) do X #:: Min..Max ).
int_declare_array(Xs, Elems) :-
	( foreacharg(X,Xs), param(Elems) do X #:: Elems ).
set_declare_array(Xs, Min, Max) :-
	( foreacharg(X,Xs), param(Min,Max) do intset(X, Min, Max) ).
set_declare_array(Xs, Elems) :-
	( foreacharg(X,Xs), param(Elems) do X :: Elems ).


% Comparisons -----------------------------------------------

% int_??(var int, var int)
int_eq(X, Y) :- X = Y.
int_ne(X, Y) :- X #\= Y.
int_le(X, Y) :- X #=< Y.
int_ge(X, Y) :- X #>= Y.
int_lt(X, Y) :- X #< Y.
int_gt(X, Y) :- X #> Y.

% float_??(var float, var float)

% bool_??(var bool, var bool)
bool_eq(X, Y) :- X = Y.
bool_ne(X, Y) :- X #\= Y.
bool_le(X, Y) :- X #=< Y.
bool_ge(X, Y) :- X #>= Y.
bool_lt(X, Y) :- X #< Y.
bool_gt(X, Y) :- X #> Y.

% set_??(var set, var set)
set_eq(X, Y) :- X = Y.
%bool_ne(X, Y) :-
%bool_le(X, Y) :-
%bool_ge(X, Y) :-
%bool_lt(X, Y) :-
%bool_gt(X, Y) :-

% int_lin_??(array[int] of int, array[int] of var int, int)
int_lin_eq(Cs, Xs, Rhs) :- vector_sum(Cs, Xs, CXs), sum(CXs) #= Rhs.
int_lin_ne(Cs, Xs, Rhs) :- vector_sum(Cs, Xs, CXs), sum(CXs) #\= Rhs.
int_lin_le(Cs, Xs, Rhs) :- vector_sum(Cs, Xs, CXs), sum(CXs) #=< Rhs.
int_lin_ge(Cs, Xs, Rhs) :- vector_sum(Cs, Xs, CXs), sum(CXs) #>= Rhs.
int_lin_lt(Cs, Xs, Rhs) :- vector_sum(Cs, Xs, CXs), sum(CXs) #< Rhs.
int_lin_gt(Cs, Xs, Rhs) :- vector_sum(Cs, Xs, CXs), sum(CXs) #> Rhs.

% float_lin_??(array[int] of float, array[int] of var float, float)

% int_??(var int, var int, var bool)
int_eq_reif(X, Y, B) :- #=(X, Y, B).
int_ne_reif(X, Y, B) :- #\=(X, Y, B).
int_le_reif(X, Y, B) :- #=<(X, Y, B).
int_ge_reif(X, Y, B) :- #>=(X, Y, B).
int_lt_reif(X, Y, B) :- #<(X, Y, B).
int_gt_reif(X, Y, B) :- #>(X, Y, B).

% float_??(var float, var float, B)

% bool_??(var bool, var bool)
bool_eq_reif(X, Y, B) :- #=(X, Y, B).
bool_ne_reif(X, Y, B) :- #\=(X, Y, B).
bool_le_reif(X, Y, B) :- #=<(X, Y, B).
bool_ge_reif(X, Y, B) :- #>=(X, Y, B).
bool_lt_reif(X, Y, B) :- #<(X, Y, B).
bool_gt_reif(X, Y, B) :- #>(X, Y, B).

% set_??(var set, var set, B)
%set_eq_reif(X, Y, B) :-
%bool_ne_reif(X, Y, B) :-
%bool_le_reif(X, Y, B) :-
%bool_ge_reif(X, Y, B) :-
%bool_lt_reif(X, Y, B) :-
%bool_gt_reif(X, Y, B) :-

% int_lin_??(array[int] of int, array[int] of var int, int, var bool)
int_lin_eq_reif(Cs, Xs, Rhs, B) :- vector_sum(Cs, Xs, CXs), #=(sum(CXs), Rhs, B).
int_lin_ne_reif(Cs, Xs, Rhs, B) :- vector_sum(Cs, Xs, CXs), #\=(sum(CXs), Rhs, B).
int_lin_le_reif(Cs, Xs, Rhs, B) :- vector_sum(Cs, Xs, CXs), #=<(sum(CXs), Rhs, B).
int_lin_ge_reif(Cs, Xs, Rhs, B) :- vector_sum(Cs, Xs, CXs), #>=(sum(CXs), Rhs, B).
int_lin_lt_reif(Cs, Xs, Rhs, B) :- vector_sum(Cs, Xs, CXs), #<(sum(CXs), Rhs, B).
int_lin_gt_reif(Cs, Xs, Rhs, B) :- vector_sum(Cs, Xs, CXs), #>(sum(CXs), Rhs, B).

% float_lin_??(array[int] of float, array[int] of var float, float, B)

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
int_negate(X, Z) :- Z #= -X.
int_plus(X, Y, Z) :- Z #= X+Y.
int_minus(X, Y, Z) :- Z #= X-Y.
int_times(X, Y, Z) :- Z #= X*Y.
int_div(Dividend, Divisor, Quotient) :-
 	Remainder #>=0, Remainder #< abs(Divisor),
 	Dividend #= Quotient*Divisor + Remainder.
int_mod(Dividend, Divisor, Remainder) :-
 	Remainder #>=0, Remainder #< abs(Divisor),
 	Dividend #= _Quotient*Divisor + Remainder.
int_min(X, Y, Z) :- Z #= min(X,Y).
int_max(X, Y, Z) :- Z #= max(X,Y).
int_abs(X, Z) :- Z #= abs(X).

% float_???(var float, var float, var float)


% Logical operations -----------------------------------------------

% bool_???(var bool, var bool, var bool)
bool_and(X, Y, Z) :- #/\(X, Y, Z).
bool_or(X, Y, Z) :- #\/(X, Y, Z).
bool_left_imp(X, Y, Z) :- #=>(Y, X, Z).
bool_right_imp(X, Y, Z) :- #=>(X, Y, Z).
bool_xor(X, Y, Z) :- #\=(X, Y, Z).
bool_not(X, Z) :- #\+(X, Z).

% array_bool_???(array[int] of var bool, var bool)
array_bool_and(Xs, B) :- fd_global:minlist(Xs, B).
array_bool_or(Xs, B) :- fd_global:maxlist(Xs, B).

% bool_clause(array[int] of var bool, array[int] of var bool)
bool_clause(Ps, Ns) :-
	bool_clause_reif(Ps, Ns, 1).
bool_clause_reif(Ps, Ns, B) :-
	ic_global:maxlist(Ps, B1),
	ic_global:minlist(Ns, B2),
	#>=(B1,B2,B).


% Set operations -----------------------------------------------

% set_???(... var set of int ...)
set_in(I, S) :- I in S.
set_subset(Sub, Super) :- subset(Sub, Super).
set_intersect(X, Y, Z) :- intersection(X, Y, Z).
set_union(X, Y, Z) :- union(X, Y, Z).
set_diff(X, Y, Z) :- difference(X, Y, Z).
set_symdiff(X, Y, Z) :- symdiff(X, Y, Z).
set_card(S, I) :- #(S, I).

set_in_reif(I, S, B) :- in(I, S, B).


% Array operations -----------------------------------------------

% array_xx_yy_element(var int, array[int] of xx yy, var yy)
array_bool_element(I, Array, E) :- Array =.. [_|List], element(I, List, E).
array_int_element(I, Array, E) :- Array =.. [_|List], element(I, List, E).
%array_set_element(I, Array, E) :- array_any_element(I, Array, E).
array_var_bool_element(I, Array, E) :- array_any_element(I, Array, E).
array_var_int_element(I, Array, E) :- array_any_element(I, Array, E).
%array_var_set_element(I, Array, E) :- array_any_element(I, Array, E).

    array_any_element(I, Array, E) :-
	arity(Array, N),
	I #:: 1..N,
	arg_nd(I, Array, E) infers fd.

    arg_nd(I, Array, E) :-
	indomain(I),
	arg(I, Array, E).


% Coercion operations -----------------------------------------------

% This is only useful for float-typed annotations,
% as lib(fd) cannot handle floats elsewhere.
int2float(I, F) :- ( var(I) -> F=I ; F is float(I) ).

bool2int(X, X).


% FZN data <-> ECLiPSe data -----------------------------------------------

bool_fzn_to_solver(false, 0).
bool_fzn_to_solver(true, 1).

bool_solver_to_fzn(0, false).
bool_solver_to_fzn(1, true).

% This is only useful for float-typed annotations,
% as lib(fd) cannot handle floats elsewhere.
float_fzn_to_solver(X, F) :- F is float(X).

% should never be needed
float_solver_to_fzn(X, X) :- real(X).


% Set constants are ordered lists in fd_sets
set_fzn_to_solver(List, Set) :- eclipse_language:sort(List, Set).

set_solver_to_fzn(List, List) :- is_list(List).

% Set constants are ordered lists in fd_sets
range_fzn_to_solver(Min, Max, Set) :-
	( for(I,Min,Max), foreach(I,Set) do true ).


% Search -----------------------------------------------

satisfy(Anns) :-
	( foreach(Ann,Anns) do
	    termify(Ann, Ann1),	% for Flatzinc<1.0 compatibility
	    search_ann(Ann1)
	).

:- export minimize/3.	% silence warning
minimize(Expr, Anns, Cost) :-
	extract_bb_anns(Anns, Anns1, BbOptions),
	Cost #= Expr, 
	bb_min(satisfy(Anns1), Cost, BbOptions).

maximize(Expr, Anns, ObjVal) :-
	extract_bb_anns(Anns, Anns1, BbOptions),
	Cost #= -Expr, 
	bb_min(satisfy(Anns1), Cost, BbOptions),
	ObjVal is -Cost.

    termify(In, Out) :-
	functor(In, F, N), functor(Out, F, N),
    	( for(I,1,N),param(In,Out) do
	    arg(I,In,InI), arg(I,Out,OutI),
	    ( string(InI) -> term_string(OutI,InI) ; OutI=InI )
	).


    search_ann(seq_search(Searches)) :- !,
    	( foreacharg(Search,Searches) do search_ann(Search) ).
    search_ann(bool_search(Vars, Select, Choice, Explore)) :- !,
	search(Vars, 0, Select, Choice, Explore, []).
    search_ann(int_search(Vars, Select, Choice, Explore)) :- !,
	search(Vars, 0, Select, Choice, Explore, []).
    search_ann(float_search([], _Prec, _, _, _)) :- !.
    search_ann(set_search(Sets, input_order, Choice, complete)) :-
	set_choice(Choice, ElemSel, Order), !,
	( functor(Sets, [], _) ->
	    ( foreacharg(Set,Sets), param(ElemSel,Order) do
		insetdomain(Set, any, ElemSel, Order)
	    )
	;
	    ( foreach(Set,Sets), param(ElemSel,Order) do
		insetdomain(Set, any, ElemSel, Order)
	    )
	).
    search_ann(Ann) :-
	fzn_error("Unsupported search annotation: %w", [Ann]).


    :- mode set_choice(+,-,-).
    set_choice(indomain_min, small_first, in_notin).
    set_choice(indomain_max, big_first, in_notin).
    set_choice(outdomain_min, small_first, notin_in).
    set_choice(outdomain_max, big_first, notin_in).


    extract_bb_anns(Anns, RestAnns, BbOptions) :-
    	(
	    foreach(Ann,Anns),
	    fromto(RestAnns,Anns2,Anns1,[]),
	    param(BbOptions)
	do
	    ( bb_ann(Ann, BbOptions) -> Anns2 = Anns1 ; Anns2 = [Ann|Anns1] )
	).

    % Corresponding annotation-declarations are in eclipse.mzn
    bb_ann(delta(R), bb_options{delta:F}) :- float(R, F).
    bb_ann(factor(R), bb_options{factor:F}) :- float(R, F).
    bb_ann(timeout(R), bb_options{timeout:F}) :- float(R, F).
    bb_ann(strategy(S), bb_options{strategy:A}) :- atom_string(A, S).


% Global Constraints (see fzn_fd/globals.mzn) -------------------

all_different_int(Xs) :- fd_global:alldifferent(Xs).

at_most_int(N,Xs,V) :- fd_global:atmost(N, Xs, V).
%at_most(N,Xs,V) :- Count #=< N, fd_global:occurrences(V, Xs, Count).

at_least_int(N,Xs,V) :- Count #>= N, fd_global:occurrences(V, Xs, Count).

exactly_int(N,Xs,V) :- fd_global:occurrences(V, Xs, N).

count_avint_int_int(Xs,V,N) :- fd_global:occurrences(V, Xs, N).

%cumulative_avint_aint_aint_vint(Ss,Ds,Rs,B) :- cumulative:cumulative(Ss,Ds,Rs,B).
cumulative_avint_aint_aint_vint(Ss,Ds,Rs,B) :- edge_finder:cumulative(Ss,Ds,Rs,B).
%cumulative_avint_aint_aint_vint(Ss,Ds,Rs,B) :- edge_finder3:cumulative(Ss,Ds,Rs,B).

:- export sort/2.	% to resolve ambiguity with sort/2 builtin
sort(Xs,Ss) :- fd_global:sorted(Xs, _Ps, Ss).


:- reexport disjoint/2 from fd_sets.

:- export all_disjoint/1.
all_disjoint(Array) :- Array =.. [[]|List], fd_sets:all_disjoint(List).

link_set_to_booleans(Set, Bools) :- fd_sets:membership_booleans(Set, Bools).

