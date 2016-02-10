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
% Contributor(s): Pierre Lim, ECRC.
% 
% END LICENSE BLOCK

:- module(r).

:- export op(700,xfx,$=).
:- export op(700,xfx,$<>).
:- export op(700,xfx,$>=).
:- export op(700,xfx,$<=).
:- export op(700,xfx,$=<).         % CHIP compatibility
:- export op(700,xfx,$>).
:- export op(700,xfx,$<).

:- export portray(type(rational), tr_rat/2, []).

:- local other_error_handler/3.
:- dynamic other_error_handler/3.

/*
:- define_macro($$= /2, tr_r/2, [write]).
:- define_macro($$<> /2, tr_r/2, [write]).
:- define_macro($$>= /2, tr_r/2, [write]).
:- define_macro($$<= /2, tr_r/2, [write]).
:- define_macro($$=< /2, tr_r/2, [write]).         % CHIP compatibility
:- define_macro($$> /2, tr_r/2, [write]).
:- define_macro($$< /2, tr_r/2, [write]).
:- define_macro(rrmin /1, tr_r/2, [write]).
:- define_macro(rrmax /1, tr_r/2, [write]).
*/


:- export
	$= /2,
	$<> /2,
	$>= /2,
	$<= /2,
	$=< /2,
	$> /2,
	$< /2,
	(rmin) /1,
	(rmax) /1,
	tr_rat/2,
	tr_r/2,
	print_global_list/0,
	print_global_list2/0,
        get_constraint_store/1,
        is_slack_variable/1,
        variable_name/2.


:- pragma(nodebug).
:- pragma(expand).                      % debugging multiple solver screwup
:- set_flag(prefer_rationals,on).	% turn on rational number system
:- set_flag(output_mode, "QVP").

:- import
	collect/3,
	setarg/3,
	suspensions/1
   from sepia_kernel.

make_const(no_macro_expansion('ZERO'), 0).
make_const(no_macro_expansion('ONE'), 1).
make_const(no_macro_expansion('MONE'), -1).
 
mono_to_dot(mono(X,Y),[X|Y]).
 
:- local macro(no_macro_expansion('ZERO'), make_const/2, []).
:- local macro(no_macro_expansion('ONE'), make_const/2, []).
:- local macro(no_macro_expansion('MONE'), make_const/2, []).
:- local macro(no_macro_expansion(mono/2), mono_to_dot/2, []).


tr_r($$=(A, B), $=(A1, B1)) :-
	object2user(A, A1),
	object2user(B, B1).
tr_r($$<>(A, B), $<>(A1, B1)) :-
	object2user(A, A1),
	object2user(B, B1).
tr_r($$>=(A, B), $>=(A1, B1)) :-
	object2user(A, A1),
	object2user(B, B1).
tr_r($$<=(A, B), $<=(A1, B1)) :-
	object2user(A, A1),
	object2user(B, B1).
tr_r($$=<(A, B), $=<(A1, B1)) :-
	object2user(A, A1),
	object2user(B, B1).         % CHIP compatibility
tr_r($$>(A, B), $>(A1, B1)) :-
	object2user(A, A1),
	object2user(B, B1).
tr_r($$<(A, B), $<(A1, B1)) :-
	object2user(A, A1),
	object2user(B, B1).
tr_r(rrmin(A), rmin(A1)) :-
	object2user(A, A1).
tr_r(rrmax(A), rmax(A1)) :-
	object2user(A, A1).


tr_rat(Term, Out) :-
	N is numerator(Term), D is denominator(Term),
        (D =:= 'ONE' ->
   	    Out = N
	;
	    Out = N/D
	).

:- local struct(lin(dg, constr, rhs, pvar, dead, user)).

:- meta_attribute(r, [
        unify:			linear_constraint_handler/2
	]).

% no longer supported in release 4.1
%:- debug_macro(0'/, "@set_flag(output_mode, \"QVP\"),nl").
%:- debug_macro(0'\, "@set_flag(output_mode, \"QVPM\"),nl").
%:- debug_macro(0'=, "@print_global_list.").
%:- debug_macro(0'|, "@print_global_list2.").           % debugging rationals

% This works only after release 5.X
:- local reference(global_list, []).


%----------------------------------------------------------
% user level predicates
%----------------------------------------------------------

$$<>(X, Y) :-
    %index_integrity_check("At $$<> integrity check 1"),
    (linnorm(X-Y,Norm0) ->
        mark_solver_variables(Norm0),
        substitute(1,Norm0,[],Norm1),
        simplify(Norm1,Norm,_),
        disequality(Norm)
    ;
	delay([X,Y],$$<>(X, Y))
    ).
    %index_integrity_check("At $$<> integrity check 2").

$$=<(X, Y) :-
    $$<=(X, Y).

$$<=(X, Y) :-
    %index_integrity_check("At $$<= integrity check 1"),
    (linnorm(P+X-Y,Norm0) ->	% the order of X-Y+P is important!
        mark_solver_variables(Norm0),
        positive(P),
        substitute(1,Norm0,[],Norm1),
        simplify(Norm1,Norm,_),
        ( constant_rhs(Norm, Val) ->
	    Val = 'ZERO'
        ; all_pvars(Norm) ->
	    simplex(Norm)
        ;
	    eliminate(Norm)
        )
    ;
	delay([X,Y], $$<=(X, Y))
    ).
    %index_integrity_check("At $$<= integrity check 2").

$$>=(X, Y) :-
    %index_integrity_check("At $$>= integrity check 1"),
    (linnorm(P-X+Y,Norm0) ->	% the order of X-Y+P is important!
        mark_solver_variables(Norm0),
        positive(P),
        substitute(1,Norm0,[],Norm1),
        simplify(Norm1,Norm,_),
        ( constant_rhs(Norm, Val) ->
	    Val = 'ZERO'
        ; all_pvars(Norm) ->
	    simplex(Norm)
        ;
	    eliminate(Norm)
        )
    ;
	delay([X,Y],$$>=(X, Y))
    ).
    %index_integrity_check("At $$>= integrity check 2").

$$>(X, Y) :-
    $$<(Y, X).

$$<(X, Y) :-
    $$<>(X, Y),
    $$<=(X, Y).

$$=(X, Y) :-
    %index_integrity_check("At $$= integrity check 1"),
    (linnorm(X-Y,Norm0) ->
        lin_eq(Norm0)
    ;
	delay([X,Y], $$=(X, Y))
    ).
    %index_integrity_check("At $$= integrity check 2").

lin_eq(Norm0) :-
    mark_solver_variables(Norm0),
    substitute(1,Norm0,[],Norm1),
    simplify(Norm1,Norm,_),
    ( constant_rhs(Norm, Val) ->
	Val = 'ZERO'
    ; all_pvars(Norm) ->
	simplex(Norm)
    ;
	eliminate(Norm)
    ).

%----------------------------------------------------------
% Debugging
%----------------------------------------------------------

linerr(Msg) :-
	printf(error, "%Vw\n%b", [Msg]),
	abort.

%----------------------------------------------------------
% Accessing metaterms
%----------------------------------------------------------

is_pvar(_{lin with [pvar:Pvar]}) ?-
	nonvar(Pvar).

positive(X) :-
	meta(X), !,
	get_lin_attr(X,Attr),
	Attr = lin with [pvar:true].
positive(X) :-
	free(X), !,
	add_attribute(X, lin with [pvar:true,constr:[]]).
positive(X) :-
	linerr("free var expected in"-positive(X)),
%       writeln("free var expected in"-positive(X)),
	true.

get_lin_attr(_{Lin}, Lin1) ?-
	Lin = lin with [],	% Lin = lin(_,_,_)
	Lin1 = Lin.

lin_rhs(_{lin with [rhs:Rhs]}, Rhs1) ?-
	Rhs = Rhs1.

lin_constr(_{lin with [constr:Constr]}, Constr1) ?-
	Constr = Constr1.

lin_all(_{Attr}, Attr1) ?-
	Attr = Attr1.


insert_constr(Vrho, Vlhs{Lin}) ?-
	Lin = lin with [constr:OldLocal],
	setarg(constr of lin, Lin, [Vrho|OldLocal]),
	r_notify_constrained(Vlhs).
	%writeln(insert_constr(Lin)).
	

linear_term(_{lin with []}) ?-
	true.
%
% 16.11.93
delayed_term(X) :-
	meta(X).

mark_solver_variables([]).
mark_solver_variables([mono(_, V)|T]) :-
	( number(V) ->
	    true
	; linear_term(V) ->
	    true
	; delayed_term(V) ->
	    lin_all(V,Attr),
	    Attr = lin with [constr:Constr],
	    Constr = []
	;
	    add_attribute(V, lin with [constr:[]])
	),
	mark_solver_variables(T).

new_variable(V) :-
	get_lin_attr(V, lin with [rhs:Rhs, constr:[]]),
	var(Rhs).

parametric(V) :-
	lin_rhs(V,Rhs),
	var(Rhs).


%----------------------------------------------------------
% General utilities
%----------------------------------------------------------

delay(Vars, Goal) :-
	suspend(Goal, 3, Vars->bound).

all_new_variables([]).
all_new_variables([mono(_C,V)|T]) :-
	parametric(V),
	all_new_variables(T).

add_rhs(V, Rhs) :-
%       test_rhs(Rhs),
	get_lin_attr(V, Attr),
% Gauss-Jordan
 	Attr = lin with [rhs:Rhs, constr:Constr],
	r_notify_constrained(V),
 	substitute_single_var(V, Rhs, Constr),
% SUBS
	put_in_global_list(V),
	put_in_constr_lists(V, Rhs).

delete_var_from_constr(_,[],[]).
delete_var_from_constr(Var,[V|T],New) :-
	( Var == V ->
	    New = T
	;
	    New = [V|New0],
	    delete_var_from_constr(Var,T,New0)
	).

remove_from_local(_,[]) :- !.
remove_from_local(Var,[mono(_,V)|T]) :-
	delete_from_constr_list(Var, V),
	remove_from_local(Var,T).

delete_from_constr_list(_Var, V) :-
        nonvar(V),
	!.
delete_from_constr_list(Var, V) :-
        get_lin_attr(V, Attr),
	Attr = lin with [constr:Constr],
	delete_var_from_constr(Var,Constr,New),
        setarg(constr of lin, Attr, New).

remove_from_global(V) :-
	getval(global_list,G),
	delete_var_from_constr(V,G,G1),
	setval(global_list,G1).

delete_rhs(V) :-
	get_lin_attr(V, Attr),
	Attr = lin with [rhs:Rhs],
	get_constant(Rhs,_,Rest),
	remove_from_local(V,Rest),
	remove_from_global(V),
	setarg(rhs of lin, Attr, _).

delete_simplex_row(Pvar) :-
        get_lin_attr(Pvar,_Attr),
	delete_rhs(Pvar).

all_pvars([]).
all_pvars([mono(_,V)|T]) :-
	(number(V) ; is_pvar(V)),
	!,
	all_pvars(T).

constant_rhs([], 'ZERO').
constant_rhs([mono(C,V)], Val) :-
	number(C),
	number(V),
	Val is C*V.

zero(X) :- var(X), !, fail.
zero(X) :- sgn(X, 0).

%get_var_coeff([], _, _) :-
%	linerr("variable not found in rhs in get_var_coeff").
get_var_coeff([], _Var, 0) :-
%	writeln("variable not found in rhs in get_var_coeff"),
        true.
get_var_coeff([mono(C,V)|T], Var, Coeff) :-
	(Var == V ->
		Coeff = C
	;
		get_var_coeff(T, Var, Coeff)
        ).

%----------------------------------------------------------
% Handler for Unification of linear variables
% It does two things:
% 1. A binding of a variable that is a left had side is treated
%    as a new equality and given to the solver. If this is not
%    wanted or needed, the dead flag should be set before doing
%    the binding.
% 2. All right hand sides that involve the (no bound) variable
%    are simplified.
% We assume that pvars are not bound in an uncontrolled way,
% so we don't make special checks here.
%----------------------------------------------------------

linear_constraint_handler(X, Y) :-
	( var(Y) ->
	    true
	;
	    linear_unify(X, Y)
	).

linear_unify(Val, lin with [dg:DG, rhs:Rhs, constr:Constr, dead:Dead]) :-
	number(Val),
	!,
	( var(Dead), nonvar(Rhs) ->
	    (zero(Val) ->
	    	lin_eq(Rhs)			% treat as new equality
	    ;
	        lin_eq([mono('MONE',Val)|Rhs])	% treat as new equality
	    )
	;
	    true
	),
	simplify_rhs(Constr),
	schedule_woken(DG).
linear_unify(killed, _) :- !.
linear_unify(Val, X) :-
        linerr("Error in linear_unify "-linear_unify(Val, X)).

simplify_rhs([]) :- !.
simplify_rhs([Vlhs|T]) :-
	( var(Vlhs) ->
	    get_lin_attr(Vlhs, Attr),
	    arg(rhs of lin, Attr, Rhs0),
	    substitute(1,Rhs0,[],Rhs),
	    simplify(Rhs, RhsSimp, Zeros),
	    remove_from_local(Vlhs,Zeros),
	    update(Vlhs, Attr, RhsSimp),
	    (is_pvar(Vlhs) ->
		check_rhs(RhsSimp,Form),
		get_constant(RhsSimp,_Const,RhsRest),
		(Form == ok ->
		    true
		;Form == violates_3 ->
		    fail                           % nonpositive constant
		;Form == violates_4 ->
		    (all_new_variables(RhsRest) ->
			true                       % work already done
		    ;
			delete_simplex_row(Vlhs)   % redundant 
		    )
		;Form == violates_5a ->
		    bind_all_zero(RhsRest)
		;Form == violates_5b -> 
		    (all_new_variables(RhsRest) ->
			true                       % work already done
		    ;
			delete_simplex_row(Vlhs)   % redundant
		    )
		)
	    ;
		true
	    )
	;
	    true
	),
	simplify_rhs(T).

update_rhs(V, NewRhs) :-
	get_lin_attr(V, Attr),
	update(V, Attr, NewRhs).

update(V, lin with [dead:Dead], []) :-
	!,
	Dead = dead,
	V = 'ZERO'.
update(V, lin with [dead:Dead,pvar:Pvar], [mono(C1,V1)]) :-
	number(C1),
	number(V1),
	!,
	V2 is C1*V1,
	(nonvar(Pvar) ->
	    V2 >= 0
	;
	    true
	),
	Dead = dead,
	V = V2.
update(V, Attr, RhsSimp) :-
%       test_rhs(RhsSimp),
	setarg(rhs of lin, Attr, RhsSimp),
	r_notify_constrained(V),
% Patch 4.6.93
        (is_pvar(V) ->
	lin_constr(V,Constr),
		substitute_single_var(V,RhsSimp,Constr)
	;
		true
	).

divide_remove(_,_,[],[]) :- !.
divide_remove(C,Var,[mono(C1,V)|T],R) :-
    (Var == V ->
        divide_remove(C,Var,T,R)
    ;
	C2 is C1/C,
        R = [mono(C2,V)|T1],
        divide_remove(C,Var,T,T1)
    ).

put_in_constr_lists(_, []).
put_in_constr_lists(Vrho, [mono(_, V)|T]) :-
	( var(V) ->
	    insert_constr(Vrho, V)
	;
	    true
	),
	put_in_constr_lists(Vrho, T).

put_in_global_list(V) :-
	getval(global_list, Old),
	setval(global_list, [V|Old]).

find_rho([mono(C,V)|T],R) :-
    (var(V) ->
        ( is_pvar(V) ->
            find_rho(T,R)
        ;
            R = mono(C,V)
        )
    ;
        find_rho(T,R)
    ).


/* Working spec
% substitute(+Coeff, +Rhs, +RhsSubst, -RhsSubst)
:- mode substitute(+,+,+,-).
substitute(_,[],R0,R) :- !, R0=R.
substitute(Coeff,[mono(C,V)|T],R0,R) :-
    C1 is Coeff*C,
    ( lin_rhs(V, Rhs), nonvar(Rhs) ->
	    substitute(C1,Rhs,R0,R1)
    ;
	    R1 = [mono(C1,V)|R0]
    ),
    substitute(Coeff,T,R1,R).
*/
% substitute(+Coeff, +Rhs, +RhsSubst, -RhsSubst)
:- mode substitute(+,+,+,-).
substitute(_,[],R0,R) :- !, R0=R.
substitute(Coeff,[mono(C,V)|T],R0,R) :-
    C1 is Coeff*C,
    substitute_aux(C1,V,R0,R1),
    substitute(Coeff,T,R1,R).

:- mode substitute_aux(+,?,+,-).
substitute_aux(C,V,R0,R) :-
    number(V),
    !,
    R = [mono(C,V)|R0]. 
substitute_aux(C,V,R0,R) :-
    lin_rhs(V, Rhs),
    substitute_aux1(C,V,Rhs,R0,R).

:- mode substitute_aux1(+,+,?,+,-).
substitute_aux1(C,V,Rhs,R0,R) :-
    var(Rhs),
    !,
    R = [mono(C,V)|R0]. 
substitute_aux1(C,_,Rhs,R0,R) :-
    substitute(C,Rhs,R0,R).

:- mode simplify2(+,+,-,-,-).
/*
% Working spec
simplify2(N1, N2, N, NewVars, Zeros) :-
	sort(2, >=, N1, N1s),
	sort(2, >=, N2, N2s),
	merge_and_detect_new_vars(N1s, N2s, N3, NewVars),
	collect(N3, N, Zeros).
*/
% Optimized version
simplify2(N1, N2, N, NewVars, Zeros) :-
	merge_and_detect_new_vars(N1, N2, N3, NewVars),
	collect(N3, N, Zeros).

% merge_and_detect_new_vars(Old, New, Merged, NewVars)

:- mode merge_and_detect_new_vars(+,+,-,-).
merge_and_detect_new_vars([], New, New, New) :- !.
merge_and_detect_new_vars(Old, [], Old, []) :- !.
/* Working spec
merge_and_detect_new_vars([Old|Olds], [New|News], Merged, NewVars) :-
	New = mono(_,NewV),
	Old = mono(_,OldV),
	compare(R, NewV, OldV),
        (   R = (<) ->
		Merged = [Old|Merged1],
		merge_and_detect_new_vars(Olds, [New|News], Merged1, NewVars)
        ;   R = (>) ->
		Merged = [New|Merged1],
		NewVars = [New|NewVars1],
		merge_and_detect_new_vars([Old|Olds], News, Merged1, NewVars1)
        ;
		Merged = [Old,New|Merged1],
		merge_and_detect_new_vars(Olds, News, Merged1, NewVars)
        ).
*/
merge_and_detect_new_vars([Old|Olds], [New|News], Merged, NewVars) :-
	New = mono(_,NewV),
	Old = mono(_,OldV),
	compare(R, NewV, OldV),
	index_merge_and_detect_new_vars(R,Old,Olds,New,News,Merged,_Merged1,NewVars,_NewVars1),
	!.

index_merge_and_detect_new_vars((<),Old,Olds,New,News,Merged,Merged1,NewVars,_NewVars1) :-
	Merged = [Old|Merged1],
	merge_and_detect_new_vars(Olds, [New|News], Merged1, NewVars).
index_merge_and_detect_new_vars((>),Old,Olds,New,News,Merged,Merged1,NewVars,NewVars1) :-
	Merged = [New|Merged1],
	NewVars = [New|NewVars1],
	merge_and_detect_new_vars([Old|Olds], News, Merged1, NewVars1).
index_merge_and_detect_new_vars(_R,Old,Olds,New,News,Merged,Merged1,NewVars,_NewVars1) :-
	Merged = [Old,New|Merged1],
	merge_and_detect_new_vars(Olds, News, Merged1, NewVars).

:- mode collect_vars(+,-,?).
collect_vars([], Vs, Vs).
collect_vars([mono(_,V)|T], [V|Vs1], Vs0) :-
	collect_vars(T, Vs1, Vs0).



simplify(N0,N,Zeros) :-
    sort(2,>=,N0,N1),
    collect(N1,N,Zeros).


/*
check_rhs(Rhs, _Form) :-
% If the Rhs is a negative constant then fail
        get_constant(Rhs,Con,Rest),
        Con < 0,
	Rest == [],
	writeln("Neg constant rhs"),
	!,
	fail.
*/
check_rhs(Rhs, Form) :-
	get_constant(Rhs, Con, Rest),
	( Con < 0 ->
		% required action: delete constraint and re-add
		Form = violates_3
	;
		count_coeff_signs(Rest, 0, Plus, 0, Minus),
		( Minus = 0 ->
			% required action: delete constraint (redundant)
			Form = violates_4
		; zero(Con) ->
			( Plus = 0 ->
				% required action: bind all vars to zero
				Form = violates_5a
			; Plus = 1 ->
				% required action: delete constraint (redundant)
				Form = violates_5b
			;
				Form = ok
			)
		;
			Form = ok
		)
	).

:- mode count_coeff_signs(+,+,-,+,-).
count_coeff_signs([], P, P, M, M).
count_coeff_signs([mono(C,_)|T], P0, P, M0, M) :-
	( C < 'ZERO' ->
		M1 is M0+1,
		count_coeff_signs(T, P0, P, M1, M)
	;
		P1 is P0+1,
		count_coeff_signs(T, P1, P, M0, M)
	).


%----------------------------------------------------------
% Gauss
%----------------------------------------------------------

eliminate(N) :-
    find_rho(N,mono(C,V)),
    NegC is -C,
    divide_remove(NegC,V,N,N1),
    ( lin_rhs(V,Rhs), nonvar(Rhs) ->
	linerr("lhs in eliminate")
    ;
	true
    ),
    eliminate1(V, N1).

eliminate1(V, []) :-
	!,
	V = 'ZERO'.
eliminate1(V, [mono(C1,V1)]) :-
	number(C1),
	number(V1),
	!,
	V is C1*V1.
eliminate1(V, N1) :-
	add_rhs(V, N1).


%------------------------------------------
% Printing
%------------------------------------------

lin_print_values(N, V, M) :-
	tidy_output1(D),
	other_error_handler(N, V, M),
	print_global_list,
	output_diseqs(D).

print_global_list :-
	getval(global_list, Store),
	( Store == [] ->
	    nl,
	    true
	;
	  Store == 0 ->
	    nl,
	    true                    % At loadtime there's no initialization
	;
	    ( nonground(Store) ->
		writeln(toplevel_output, "\n\nLinear Store:\n "),
		print_global_list(Store)
	    ;
	      true
	    )
	).

constant_test([mono(_C,V)]) :-
    number(V).

print_global_list([]).
print_global_list([H|T]) :-
	( get_lin_attr(H, Attr) ->
	    Attr = lin with [rhs:Rhs],
	    (var(Rhs) ->
		true
	    ;
	        substitute(1,Rhs,[],Rhs1),
                simplify(Rhs1,Rhs2,_),
		separate_pvars(Rhs2, NonP, P),
                P = P1,
                projection(H,P1,NP,Rel),
		(Rel == none ->
		     true
		;
		     ((NonP == []; constant_test(NonP)) ->
		         append(NonP, NP, Rhs3),
			 Rel1 = Rel
		     ;
                         append(NonP, P1,Rhs3),
			 Rel1 = ($=)
		     ),
	             output(Rhs3, Term),
		     object2user([H, Term], [Hu, Termu]),
	             printf(toplevel_output, "%VQPw %VQPw %VQPw\n", [Hu,Rel1,Termu])
		)
	    )
	;
	    true
	),
	print_global_list(T).

print_global_list2 :-
	getval(global_list, Store),
	( Store == [] ->
	    true
	;
	    writeln(toplevel_output, "\nUnsimplified Linear Store:\n"),
	    print_global_list2(Store)
	).

print_global_list2([]).
print_global_list2([H|T]) :-
	( get_lin_attr(H, Attr) ->
	    Attr = lin with [rhs:Rhs],
	    (var(Rhs) ->
		true
	    ;
	        output(Rhs, Term),
	        printf(toplevel_output, "%VQPw\n", $$=(H, Term))
	    )
	;
	    true
	),
	print_global_list2(T).

% :- mode remove_pvars(+,+,-).
% remove_pvars(H,B,B) :- !.
remove_pvars(H,B,Bout) :-
    not(is_pvar(H)),
    select_pvar(B,Bout),
    !.
remove_pvars(_H,B,B).

select_pvar([mono(_C,V)|Rest],PVar) :-
    number(V),
    !,
    select_pvar(Rest,PVar).
select_pvar([mono(_C,V)|_Rest],PVar) :-
    is_pvar(V),
    appears_on_rhs(V),
    !,
    PVar = V.
select_pvar([mono(_C,_V)|Rest],PVar) :-
    select_pvar(Rest,PVar).

appears_on_rhs(V) :-
    lin_constr(V,[C]),
    lin_rhs(C,Rhs),
    writeln(constr(C,Rhs)).

:- mode separate_pvars(+,-,-).
separate_pvars([], [], []).
separate_pvars([mono(C,V)|T], NonP, P) :-
	( is_pvar(V) ->
		P = [mono(C,V)|P1],
		NonP = NonP1
	;
		P = P1,
		NonP = [mono(C,V)|NonP1]
	),
	separate_pvars(T, NonP1, P1).


output([], 0).
output([X|Xs], Z) :-
	output_aux(X, Prod),
	output3(Xs, Prod, Z).

output3([], Z, Z) :- !.
output3([X|Xs], Z0, Z) :-
	output_aux(X, Prod),
	output3(Xs, Z0+Prod, Z).

output_aux(mono(X,Y),Z) :-
    number(Y),
    !,
    Z is X*Y.
output_aux(mono(X,Y),X * Y).

%------------------------------------------
% Simplex procedure
%------------------------------------------

simplex([]) ?- !,
    linerr("simplex([]) called").
% simplex(Constr) :- writeln(simplex(Constr)), fail.
simplex(Constr) :-
    %index_integrity_check("At simplex/1 integrity check:"),
    get_constant(Constr,Con,Rest),
    separate_signs(Rest, Plus, Minus),
    ( zero(Con) ->
	simplex0(Plus, Minus, Rest)
    ;
	simplex2(Con, Plus, Minus, Rest)
    ).

% simplex(Plus, Minus) === simplex2(0, Plus, Minus)

simplex0([], Minus, _) :- !, bind_all_zero(Minus).
simplex0(Plus, [], _) :- !, bind_all_zero(Plus).
simplex0([Plus], _Minus, All) :-
	!,
	(all_new_variables(All) ->
		simplex0_aux(Plus,All)
	;
		true
	).
simplex0(_Plus, [Minus], All) :-
	!,
	(all_new_variables(All) ->
		simplex0_aux(Minus,All)
	; 
		true 
	).
simplex0(_Plus, _Minus, All) :-
	simplex3('ZERO', [], All, All).

simplex0_aux(mono(C,V),Norm) :-
        C1 is -C,
        divide_remove(C1,V,Norm,Norm1),
	add_rhs(V,Norm1),
	lin_constr(V,Constr),
	substitute_single_var(V,Norm1,Constr).

bind_all_zero([]) :- !.
bind_all_zero([mono(_,'ZERO')|T]) :-
	bind_all_zero(T).

% simplex2(Cons, Plus, Minus) where Cons =\= 0

% If equation is of the form -Con = -P then P is positive; succeed
% When the constant is extracted it has not yet been moved to the other side
% Multiplication by -1 on the coefficient moves the variable to the other side
% Since only PVars remain; check that the constant is positive
% Division scales the constant to a unit of the variable;
% Then the variable is bound to the constant
simplex2(Con, [], [mono(C,V)], _) :- !, V1 is Con/(-C), V1 > 0, V = V1.
simplex2(Con, [mono(C,V)], [], _) :- !, V1 is Con/(-C), V1 > 0, V = V1.
simplex2(Con, Plus, Minus, All) :-
	( Con < 'ZERO' ->
	    simplex3(Con, Minus, Plus, All)
	;
	    simplex3(Con, Plus, Minus, All)
	).

% simplex3(A, B, C, D) :- writeln(simplex3(A,B,C,D)), fail.
simplex3(_, _, [], _) :- !, fail.
simplex3(Con, _Plus, Minus, All) :-
	find_new_variable(Minus, Cnew, Vnew),
	!,					% step 4a
	Cnew1 is -Cnew,
	(zero(Con) ->
		divide_remove(Cnew1, Vnew, All, All1)
	;
		divide_remove(Cnew1, Vnew, [mono(Con, 'ONE')|All], All1)
	),
	add_rhs(Vnew, All1),
	lin_constr(Vnew, Constr),
/***/   substitute_single_var(Vnew, All1, Constr).
simplex3(Con, Plus, Minus, All) :-
	find_new_variable(Plus, Cnew, Vnew),
	!,					% step 4b
	Cnew1 is -Cnew,
	(zero(Con) ->
		divide_remove(Cnew1, Vnew, All, Pi)
	;
		divide_remove(Cnew1, Vnew, [mono(Con, 'ONE')|All], Pi)
	),
	Minus \== [], 		% no possible pivots: fail
	pick_pivot(Minus, none, Pivot_or_Unbounded),
	( Pivot_or_Unbounded = pivot(_,_) ->	% step 8
		pivot(Pivot_or_Unbounded),
		(zero(Con) ->
			substitute(1,All,[],Norm1)
		;
			substitute(1,[mono(Con, 'ONE')|All],[],Norm1)
		),
		simplify(Norm1,Norm,_),
		simplex(Norm)
	;					% step 7b
		Pivot_or_Unbounded = unbounded(C,V),
 		C1 is -C,
		get_var_coeff(All, V, Coeff),	%debugging
		( Coeff =:= C ->
			true
		;
			linerr("bogus coeff in simplex3"-(Coeff=\=C))
		),
		(zero(Con) ->
			divide_remove(C1, V, All, Pi1)
		;
			divide_remove(C1, V, [mono(Con, 'ONE')|All], Pi1)
		),
		add_rhs(V, Pi1),
		lin_constr(V, Constr),
/***/           substitute_single_var(V, Pi1, Constr)
	).
simplex3(Con, _Plus, [mono(C,V)|_T], All) :-	% no new variable, step 3
	C1 is -C,
	(zero(Con) ->
		divide_remove(C1, V, All, Pi)
	;
		divide_remove(C1, V, [mono(Con, 'ONE')|All], Pi)
	),
	lin_constr(V, Constr),
	substitute_single_var(V, Pi, Constr, RemovedRhs),
	readd_constr(RemovedRhs).

readd_constr([]) :- !.
readd_constr([Rhs0|T]) :-
        substitute(1,Rhs0,[],Rhs),
        simplify(Rhs,Rhs1,_),
	simplex(Rhs1),
	readd_constr(T).

insert_var_mono([], Mono, [Mono]).
insert_var_mono([FirstMono|Others], Mono, Out) :-
	FirstMono = mono(_,Con),
	( var(Con) ->
		Out = [Mono,FirstMono|Others]
	;
		Out = [FirstMono,Mono|Others]
	).

substitute_single_var(Var, Rhs, Constr) :-
	substitute_single_var(Var, Rhs, Constr, _Removed).
%	( Removed == [] ->
%		true
%	;
%		true
%
% If there are no new variables then we have to allow a parametic
% variable to become non-parametric. This can lead to removal of constraints.
%
%linerr("constraint unexpectedly removed in substitute_single_var")
%	).


:- mode substitute_single_var(?,+,+,-).
substitute_single_var(Var, _, [], []) :-
% The constr list of Var must now be empty
	true,
        (var(Var) ->
            get_lin_attr(Var, Attr),
	    Attr = lin with [constr:_Constr],
            setarg(constr of lin, Attr, [])
	;
	    true
	).
substitute_single_var(Var, Pi0, [V|T], RemRhs) :-
        (nonvar(V) ->
	     RemRhs = RemRhs1,
             true
        ;
     	     (lin_rhs(V, Vrhs) ->
/*
% ****** test code
             substitute(1,Vrhs0,[],Vrhs1),
             simplify(Vrhs1,Vrhs,_),
% ****** test code
*/
	     remove_mono(Var, Vrhs, VrhsRest, Coeff),
	     multiply_all(Coeff, Pi0, Pi),
	simplify2(VrhsRest, Pi, NewRhs, NewVars, Zeros),
%%
	(NewRhs \= [], get_constant(NewRhs,NNNCon,_), NNNCon < 0 ->
	    mywriteln(old(V,Vrhs)),
	    mywriteln(new(V,NewRhs))
	;
	    true
	),
%%
	remove_from_local(V, Zeros),
%	remove_from_local(Var, [V]),
        true
        ;  % else lin_rhs(V, Vrhs)
	    mywriteln("failed lin_rhs")
	),
% Var is no longer in the rhs of of V 
	( is_pvar(V) ->
	    insert_var_mono(NewRhs,mono('MONE',V), NewRhs00),
	    check_rhs(NewRhs00, Form),
	    ( Form = ok ->
			RemRhs = RemRhs1,
			sort(2, >=, NewRhs, NewRhs1),
			update_rhs(V, NewRhs1),
			delete_from_constr_list(V, Var),
			put_in_constr_lists(V, NewVars)
		; Form = violates_3 ->
		% DEBUGGING
%			writeln("nonpositive constant"-NewRhs),
			delete_simplex_row(V),
			insert_var_mono(NewRhs, mono('MONE',V), NewRhs1),
			RemRhs = [NewRhs1|RemRhs1]
		; Form = violates_4 ->
			(all_new_variables(NewRhs) ->
				RemRhs = RemRhs1,
				sort(2, >=, NewRhs, NewRhs1),
				update_rhs(V, NewRhs1),
				delete_from_constr_list(V, Var),
				put_in_constr_lists(V, NewVars)
			;
			%% DEBUGGING
%				writeln("redundant "-NewRhs),
				RemRhs = RemRhs1,
				delete_simplex_row(V)
			)
		; Form = violates_5a ->
			RemRhs = RemRhs1,
			bind_all_zero(NewRhs),
			V = 'ZERO'
		; Form = violates_5b ->
			(all_new_variables(NewRhs) ->
				RemRhs = RemRhs1,
				sort(2, >=, NewRhs, NewRhs1),
				update_rhs(V, NewRhs1),
				delete_from_constr_list(V, Var),
				put_in_constr_lists(V, NewVars)
			;
			% DEBUGGING
%				writeln("redundant "-NewRhs),
				RemRhs = RemRhs1,
				delete_simplex_row(V)
			)
		;
			linerr("Form does not have correct form")
		)
	;
		RemRhs = RemRhs1,
		update_rhs(V, NewRhs),
		delete_from_constr_list(V, Var),
		put_in_constr_lists(V, NewVars)
	)
        ),
	substitute_single_var(Var, Pi0, T, RemRhs1).

%remove_mono(_, [], _, _) :-
remove_mono(_, [], [], 0) :-
        !,
%	linerr("occurrence not found in remove_mono").
% Application developers want more informative error messages
%        linerr("Index integrity violation").
        true.
remove_mono(Var, [Mono|T], Out, Coeff) :-
	Mono = mono(C,V),
	( V == Var ->
		Coeff = C,
		Out = T
	;
		Out = [Mono|Out1],
		remove_mono(Var, T, Out1, Coeff)
	).

:- mode multiply_all(+,+,-).
multiply_all(_, [], []).
multiply_all(Coeff, [mono(C,V)|T], [mono(C1,V)|T1]) :-
	C1 is Coeff*C,
	multiply_all(Coeff, T, T1).



:- mode pick_pivot(+,+,-).
pick_pivot([], V, V).
pick_pivot([mono(C,V)|T], Vin, Vout) :-
	( only_pos_coeff(V) ->
		Vout = unbounded(C,V)
	; Vin = none ->
		pick_pivot(T, pivot(C,V), Vout)
% Test is reversed here because we used the previously
% split list without the pre multiplication
%	; Vin = pivot(Cmax,_), C > Cmax ->
%		pick_pivot(T, pivot(C,V), Vout)
	; Vin = pivot(Cmax,_) ->
		(C =:= 0 ->
		    pick_pivot(T, Vin, Vout)        % no change
		;
		(C < 0 ->
		    (C > Cmax ->
		        pick_pivot(T, pivot(C,V), Vout)
		    ;
		        pick_pivot(T, Vin, Vout)        % no change
                    )
		;
                    (C < Cmax ->
		        pick_pivot(T, pivot(C,V), Vout)
	            ;	
		        pick_pivot(T, Vin, Vout)        % no change
		    )
		)
		)
	; Vin = pivot(Cmax,_), C =:= Cmax ->
		% need cycle breaking code here
		% writeln(error, "possible cycle detected in pick_pivot/3"),
		pick_pivot(T, Vin, Vout)
	;
		pick_pivot(T, Vin, Vout)
	).

% this check could be done with a simple test if the constr-list were
% split into one with positive and one with negative occurrences of the
% variable (as done in chip).

only_pos_coeff(V) :-
	lin_constr(V, Constr),
	only_pos_coeff(V, Constr).

only_pos_coeff(_, []).
only_pos_coeff(Var, [V|T]) :-
	( is_pvar(V) ->
		lin_rhs(V, Rhs),
		only_pos_in_rhs(Var, Rhs)
	;
 		true
	),
	only_pos_coeff(Var, T).

only_pos_in_rhs(_, []) :-
        !,
        fail.
%	linerr("Var does not occur in it's local list in only_pos_in_rhs/2").
only_pos_in_rhs(Var, [mono(C,V)|T]) :-
	( Var == V ->
		C >= 'ZERO'
	;
		only_pos_in_rhs(Var, T)
	).


get_constant(Constr, Const, Rest) :-
    nonvar(Constr),
%
% If it doesn't have a RHS then it should fail
%
    Constr = [mono(C,V)|T],
    ( var(V) ->
	Const = 'ZERO',
	Rest = Constr
    ;
	Const is C*V,
	Rest = T
    ).

:- mode separate_signs(+,-,-).
separate_signs([],[],[]) :- !.
separate_signs([Mono|T],Plus,Minus) :-
    Mono = mono(C,V),
    ( nonvar(V) ->		% debugging
	linerr("unexpected constant in separate_signs")
    ; zero(C) ->
	linerr("unexpected zero coefficient in separate_signs")
    ;
	true
    ),
    ( C =< 'ZERO' ->
	Minus = [Mono|T1],
	separate_signs(T,Plus,T1)
    ;
	Plus = [Mono|T1],
	separate_signs(T,T1,Minus)
    ).


find_new_variable([mono(C,V)|T], Cnew, Vnew) :-
	(new_variable(V) ->
%       (parametric(V) ->
		Vnew = V,
		Cnew = C
	;
		find_new_variable(T, Cnew, Vnew)
	).


%------------------------------------------
% Pivoting
%------------------------------------------

pivot(pivot(_,Vpivot)) :-
	lin_constr(Vpivot, Constr),
	choose_leaving(Constr, Vpivot, leave(none,_,_), Leave),
	Leave = leave(_,Coeff,Vleave),
	lin_rhs(Vleave, RhsL),
	insert_var_mono(RhsL, mono('MONE',Vleave), RhsPiv00),
	sort(2, >=, RhsPiv00, RhsPiv0),
% not in sorted order anymore !!!
	Coeff1 is -Coeff,
	divide_remove(Coeff1, Vpivot, RhsPiv0, RhsPiv),
	delete_rhs(Vleave),
	add_rhs(Vpivot, RhsPiv),
	lin_constr(Vpivot, ConstrPiv),
	substitute_single_var(Vpivot, RhsPiv, ConstrPiv).


:- mode choose_leaving(+,?,+,-).
choose_leaving([], _Vpivot, Max, Max) :-
	Max = leave(MaxC,_,_),			% debugging
        !,
	( MaxC = 'ONE' ->
		linerr("no leave var found in choose_leaving")
	;
		true
	).
choose_leaving([V|T], Vpivot, Max0, Max) :-
%	( is_pvar(V) ->
	( var(V) ->
		lin_rhs(V, Rhs),
		mywriteln(choose_leaving(V,Rhs)),
		get_constant(Rhs, Con, Rhs1),
		get_var_coeff(Rhs1, Vpivot, Coeff),
		( Coeff < 'ZERO' ->
			Quot is Con/(-Coeff),
			FQuot is Quot * 1.0,
			mywriteln(quot = FQuot),
			Max0 = leave(MaxC,_,_),
			( MaxC = none ->
				Max1 = leave(Quot,Coeff,V)
			; Quot < MaxC ->
				Max1 = leave(Quot,Coeff,V)
			; Quot > MaxC ->
				Max1 = Max0
			;
				Max0 = leave(_,_,Max0V),
				lin_rhs(Max0V,Max0Rhs0),
	                        insert_var_mono(Max0Rhs0,
						mono('MONE',Max0V), Max0Rhs),
	                        sort(2, >=, Max0Rhs, Max0Rhs1),
	                        insert_var_mono(Rhs,
						mono('MONE',V), Rhs2),
	                        sort(2, >=, Rhs2, Rhs3),
				break_tie(V,Rhs3,Max0V,Max0Rhs1,Result,ResultRhs),
				get_var_coeff(ResultRhs,Vpivot,VCoeff),
				% DEBUGGING
				% writeln(break_coeff(VCoeff)),
				Max1 = leave(Quot,VCoeff,Result)
			)
		;
			Max1 = Max0
		)
	;
		Max1 = Max0
	),
	choose_leaving(T, Vpivot, Max1, Max).

% break_tie(V1,Rhs1,V2,Rhs2,Result,ResultRhs).

%break_tie(_,[],_,[],_,_) :-
%	linerr("could not break tie").
break_tie(Var1,[],_Var2,[],Result,ResultRhs) :-
	Result = Var1,
	lin_rhs(Var1,ResultRhs).
break_tie(Var1,[mono(_C1,V1)|T1],Var2,[mono(_C2,V2)|T2],Result,ResultRhs) :-
	(V1 @> V2 ->
		Result = Var1,
		lin_rhs(Var1,ResultRhs)
	; V1 @< V2 ->
		Result = Var2,
		lin_rhs(Var2,ResultRhs)
	; % V1 == V2
		break_tie(Var1,T1,Var2,T2,Result,ResultRhs)
	).

%------------------------------------------
% Optimization
%------------------------------------------

:- mode all_negative(+).
all_negative([]).
all_negative([mono(C,_V)|T]) :-
    C < 0,
    all_negative(T).

rrmin(F) :-
    mywriteln(rrmin(F)),
    rrmax(-F).

check_disequations([]):- !.
check_disequations([H|T]) :-
    get_suspension_data(H, qualified_goal, G),
    (G = r:disequality1(_D) ->
        fail
    ;
        true
    ),
    check_disequations(T).

process_disequations([]) :- !.
process_disequations([mono(_C,V)|T]) :-
    (var(V) ->
        suspensions(Susps),
        check_disequations(Susps)
    ;
        true
    ),
    process_disequations(T).

rrmax(F) :-
% TIMING incval(ncon),
    (linnorm(F,Norm) ->
        rrmax1(Norm,Max),
        (process_disequations(Norm) ->
            $$=(F, Max)
        ;
            (F = -(Form) ->
                delay(F,rrmin(Form))
            ;
                delay(F,rrmax(F))
            )
        )
    ;
	delay(F,rrmax(F))
    ).

rrmax1(F,Max) :-
    mywriteln(rrmax1(F,Max)),
    mytest,
    substitute(1,F,[],F1),
    simplify(F1,F2,_),
    get_constant(F2,Con,Rest),
    (all_negative(Rest) ->
	Max = Con
    ;
	separate_signs(Rest,Plus,_Minus),
	obj_pick_pivot(Plus,none,Pivot_or_unbounded),
	mywriteln('***pivot_rrmax1'(pivot_or_unbounded = Pivot_or_unbounded,rest =Rest, plus = Plus)),
	(Pivot_or_unbounded = unbounded(_,_) ->
	    linerr("Objective function is not bounded")
	;
	    obj_pivot(Pivot_or_unbounded),
	    rrmax1(F,Max)
	)
    ).

%------------------------------------------
% disequality
%------------------------------------------

positive_pvars([]).
positive_pvars([mono(C,V)|T]) :-
    C > 'ZERO',
    is_pvar(V),
    positive_pvars(T).

disequality([]) :- !, fail.
disequality([mono(C,V)]) :-
    number(C),
    number(V),
    !,
    C*V =\= 'ZERO'.
disequality(D) :-
    get_constant(D,Con,Rest),
    Con > 'ZERO',
    positive_pvars(Rest),
    !.
disequality(D) :-
    make_suspension(disequality1(D), 2, Susp),
    insert_suspension(D, Susp, dg of lin).


disequality1(D) :-
    simplify(D,D1,_),
    disequality(D1).

%------------------------------------------
% install the handlers
%------------------------------------------

% Fix for error handler bug
:- get_event_handler(155,F/N,M),
   functor(Call,F,N),
   Call =.. [_|Args],
   Head =.. [other_error_handler|Args],
   compile_term(Head :- M:Call).
:- set_event_handler(155, lin_print_values/3).


%-----------------------------------------------------------------------------
% Pretty up output
%-----------------------------------------------------------------------------

tidy_output1(Out) :-
    suspensions(Dgs),
    remove_mygoals(Dgs,Out).

remove_mygoals([],[]) :- !.
remove_mygoals([H|T],R) :-
    get_suspension_data(H, qualified_goal, G),
    (G = r:disequality1(D) ->
	kill_suspension(H),
        R = [D|D2]
    ;
	D2 = R
    ),
    remove_mygoals(T,D2).


projection(H,Term,TermOut,Rel) :-
    (is_pvar(H) ->
	Rel = none
    ;
        (Term == [] ->
	    Rel = ($=)
	;
            process_aux(H),
	    Term = [mono(X,_Var)|TermOut1],
	    (true ->
		TermOut1 = TermOut,
	        (X =< 0 ->
		    Rel = ($<=)
	        ;
		    Rel = ($>=)
	        )
	    ;
		TermOut = Term,
		Rel = ($=)
	    )
	)
    ).

process_aux(H) :-
    get_lin_attr(H, Attr),
    Attr = lin with [rhs:Rhs],
    process_aux1(Rhs,H=Rhs).

process_aux1([],_) :- !.
process_aux1([H|T],Eqn) :-
    H = mono(_C,V),
%    writeln(mono(C,V)),
    (number(V) ->
        true
    ;
        get_lin_attr(V,Attr),
        Attr = lin with [constr:_Constr],
%       process_aux_link(Constr,Eqn),
        true
    ),
    process_aux1(T,Eqn).

process_aux_link([],_) :- !.
process_aux_link([H|T],Eqn) :-
    (positive(H) ->
        get_lin_attr(H,Attr),
        Attr = lin with [rhs:_Rhs]
/*
        (var(Rhs) ->
            true
        ;
            writeln((rhs=(H=Rhs),eqn=Eqn))
        )
*/
    ;
        process_aux_link(T,Eqn)
    ).

print_disequality(D) :-
    D = [mono(X,Y)|Rest],
    (number(Y) ->
	Rest = [mono(X1,Y1)|_]
    ;
	X = X1,
	Y = Y1
    ),
    X2 is -1 * X1,
    divide_remove(X2,Y1,D,R),
    output(R, Term),
    ( object_variable(Y1) ->
	object2user([Y1, Term], [Y1u, Termu]),
	printf(toplevel_output, "%VQPw $<> %VQPw\n", [Y1u,Termu])
    ;
        true
    ).

output_diseqs([]) :- !.
output_diseqs([H|T]) :- 
    print_disequality(H),
    output_diseqs(T).

%%%%%%%%% Isolate the handler variables from the user instanciations

:- meta_attribute(cutoff, [unify:cutoff_handler/2]).

:- import add_attribute/3 from sepia_kernel.

%%%%%%%%% The two types of variables %%%%%%%%%%%%%%%%%%%%%%%%%
user_variable(_{cutoff:user(_Object)}) ?- true.

object_variable(_{cutoff:object(_User)}) ?- true.


get_object_variable(_{cutoff:user(Vo)}, Vo1) ?-
	Vo = Vo1.

get_user_variable(_{cutoff:object(V)}, V1) ?-
	V = V1.

create_object_variable(V, Vo) :-
	add_attribute(V, user(Vo), cutoff),
	add_attribute(Vo, object(V), cutoff).


r_notify_constrained(_{cutoff:object(User)}) ?-
	!,
	notify_constrained(User).
r_notify_constrained(_). %%% Not a object variable

%%%%%%%% Unification %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cutoff_handler(Term, user(ObjectV)) ?-
	!,
	user2object(Term, TermO),
	$$=(ObjectV, TermO).
cutoff_handler(Value, object(UserV)) ?-
	!,
	( object2user(Value, ValueU)
         ->
	    UserV = ValueU
	 ;
          var(Value) ->
	    add_attribute(Value, object(UserV), cutoff)
         ;
	    printf(stderr, "Can't return a nonground object (%p) value to the user level\n", [Value]),
	   abort
        ).
%cutoff_handler(_, _NotSet).
% If its not set we can't just ignore it. PVars may not be wrapped so
% make a test for it and then
cutoff_handler(Term, Obj) :-
    is_pvar(Obj),
    !,
    $$=(Term,Obj).

cutoff_handler(_, _NotSet).


%%%%%%%% Translations from one level to the other %%%%%%%%%%%%%

user2object(V, Vo) :-
	user_variable(V),
	!,
        get_object_variable(V, Vo).
user2object(V, Vo) :-
	var(V), !,                  % Other variables
	create_object_variable(V, Vo).
user2object(Term, Term1) :-
	Term =.. [Name | Args],
	l_user2object(Args, Args1),
	Term1 =.. [Name | Args1].

l_user2object([], []).
l_user2object([Arg | Args], [Arg1 | Args1]) :-
    user2object(Arg, Arg1),
    l_user2object(Args, Args1).


object2user(V, Vo) :-
	object_variable(V),
	!,
        get_user_variable(V, Vo).
object2user(V, Vo) :-
        var(V), !,
	Vo = V.
object2user(V, _Vo) :-
	var(V), !,                  % Other variables
	linerr("Can't return non object variables").
object2user(R, N) :-
	rational(R),
	One is denominator(R), %%% Bug ECLiPSe
	One = 1,
	!,
	N is numerator(R).
object2user(Term, Term1) :-
	Term =.. [Name | Args],
	l_object2user(Args, Args1),
	Term1 =.. [Name | Args1].

l_object2user([], []).
l_object2user([Arg | Args], [Arg1 | Args1]) :-
    object2user(Arg, Arg1),
    l_object2user(Args, Args1).


%%%%%%%% Redefinition of the primitives %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

X $= Y :-
	user2object(X, Xp),
	user2object(Y, Yp),
	$$=(Xp, Yp).

X $<> Y :-
	user2object(X, Xp),
	user2object(Y, Yp),
	$$<>(Xp,  Yp).

X $>= Y :-
	user2object(X, Xp),
	user2object(Y, Yp),
	$$>=(Xp,  Yp).

X $<= Y :-
	user2object(X, Xp),
	user2object(Y, Yp),
	$$<=(Xp,  Yp).

X $=< Y :-
	user2object(X, Xp),
	user2object(Y, Yp),
	$$=<(Xp,  Yp).

X $> Y :-
	user2object(X, Xp),
	user2object(Y, Yp),
	$$>(Xp,  Yp).

X $< Y :-
	user2object(X, Xp),
	user2object(Y, Yp),
	$$<(Xp,  Yp).

rmin(Y) :-
	user2object(Y, Yp),
	rrmin(Yp).

rmax(Y) :-
	user2object(Y, Yp),
	rrmax(Yp).

variable_name(X,Y) :-
	user2object(X,X1),
        variable_name_aux(X1,Y).

variable_name_aux(_{lin with [user:User]}, User1) ?-
	User1 = User.

is_slack_variable(X) :-
        is_pvar(X).

get_constraint_store(StoreOut) :-
	getval(global_list, Store),
	( Store == [] ->
	    StoreOut = [],
	    true
	;
	  Store == 0 ->
	    StoreOut = [],
	    true                    % At loadtime there's no initialization
	;
	    ( nonground(Store) ->
		get_constraint_store(Store,StoreOut)
	    ;
              StoreOut = [],
	      true
	    )
	).

get_constraint_store([],[]).
get_constraint_store([H|T],[H1|T1]) :-
	( get_lin_attr(H, Attr) ->
	    Attr = lin with [rhs:Rhs],
	    (var(Rhs) ->
		true
	    ;
	        output(Rhs, Term),
                object2user([H, Term],[HO, TermO]),
	        H1= $=(HO, TermO)
	    )
	;
	    true
	),
	get_constraint_store(T,T1).


%%%%%%%%%%%%%%% Normalization %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
t ::= constant | variable |
      t + t |
      t - t |
      -t |
      constant * t |
      t * constant |
      t \ c

Output: List of mono(coeff,variable) pairs
*/

linnorm(V,List) :-
    var(V),
    !,
    List = [mono(1, V)].
linnorm(C,List) :-
    number(C),
    !,
    List = [mono(C, 1)].
linnorm(C0,List) :-
    constant(C0,C),
    !,
    List = [mono(C, 1)].
linnorm(T1 + T2,List) :-
    !,
    linnorm(T1,FlatT1),
    linnorm(T2,FlatT2),
    append(FlatT1,FlatT2,List).
linnorm(T1 - T2,List) :-
    !,
    linnorm(T1,FlatT1),
    distribute(-1,T2,NT2),
    linnorm(NT2,FlatT2),
    append(FlatT1,FlatT2,List).
linnorm(-T,List) :-
    !,
    distribute(-1,T,TDist),
    linnorm(TDist,List).
linnorm(C0*T0,List) :-
    constant(C0,C),
    constant(T0,T),
    !,
    Con is C * T,
    List = [mono(Con, 1)].
linnorm(C0*T,List) :-
    constant(C0,C),
    var(T),
    !,
    List = [mono(C, T)].
linnorm(C0*T,List) :-
    constant(C0,C),
    !,
    distribute(C,T,TDist),
    linnorm(TDist,List).
linnorm(T*C0,List) :-
    constant(C0,C),
    !,
    linnorm(C*T,List).
linnorm(T/C0,List) :-
    constant(C0,C),
    !,
    C1 is 1 / C,
    linnorm(C1*T,List).


% Evaluates constant expressions,
% makes error for invalid expressions,
% fails for nonground expressions (used for delay)
constant(C,_) :-
    var(C), !, fail.
constant(C,C3) :-
    number(C), !, C3 = C.
constant(-C1,C3) :- !,
    constant(C1,C11),
    C3 is -C11.
constant(C1/C2,C3) :- !,
    constant(C1,C11),
    constant(C2,C21),
    C3 is C11/C21.
constant(C1*C2,C3) :- !,
    constant(C1,C11),
    constant(C2,C21),
    C3 is C11*C21.
constant(C1+C2,C3) :- !,
    constant(C1,C11),
    constant(C2,C21),
    C3 is C11+C21.
constant(C1-C2,C3) :- !,
    constant(C1,C11),
    constant(C2,C21),
    C3 is C11-C21.
constant(_,_) :-
    linerr("Non-arithmetic functor encountered in rational constraint.").


%
% Distribute constant C all over CTerm
%

distribute(C,CTerm,T) :-
    var(CTerm),
    !,
    T = C * CTerm.
distribute(C,CTerm,T) :-
    number(CTerm),
    !,
    T is C * CTerm.
distribute(C,CTerm0,T) :-
    constant(CTerm0,CTerm),
    !,
    T is C * CTerm.
distribute(C,-CTerm,T) :-
    !,
    NegC is -C,
    distribute(NegC,CTerm,T).
distribute(C,T1 + T2,T) :-
    !,
    distribute(C,T1,NT1),
    distribute(C,T2,NT2),
    T = NT1 + NT2.
distribute(C,T1 - T2,T) :-
    !,
    distribute(C,T1,NT1),
    distribute(C,T2,NT2),
    T = NT1 - NT2.
distribute(C,T1 * T2,T) :-
    !,
    distribute(C,T1,NT1),
    T = NT1 * T2.
distribute(C,T1 / T2,T) :-
    !,
    distribute(C,T1,NT1),
    T = NT1 / T2.
/*
Removed for compiler optimization
distribute(_,_,_) :-
    writeln("Error in LINnorm / distribute"),
    abort.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This code is for internal integrity checking.
% DO NOT REMOVE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_integrity(_, [], _, _) :-
        !,
        linerr("Index integrity check violation").
check_integrity(Var, [Mono|T], Out, Coeff) :-
	Mono = mono(C,V),
	( V == Var ->
		Coeff = C,
		Out = T
	;
		Out = [Mono|Out1],
		check_integrity(Var, T, Out1, Coeff)
	).

check_index_global_list :-
	getval(global_list, Store),
	( Store == [] ->
	    nl,
	    true
	;
	  Store == 0 ->
	    nl,
	    true                    % At loadtime there's no initialization
	;
	    ( nonground(Store) ->
		writeln(toplevel_output, "\n\nBeginning index integrity check\n "),
		writeln(store(Store)),
		check_index_global_list1(Store)
	    ;
	      true
	    )
	).

check_index_global_list1([]) :- !.
check_index_global_list1([H|T]) :-
    lin_rhs(H,Rhs),
    check_index_global_list(Rhs),
    check_index_global_list1(T).

check_index_global_list([]).
check_index_global_list([mono(_,H)|T]) :-
	( get_lin_attr(H, Attr) ->
	    Attr = lin with [rhs:Rhs],
	    (var(Rhs) ->
		true
	    ;
                lin_rhs(H, Rhs),
		write("Checking:  "),
		write(H = Rhs),
		nl,
                check_integrity(H, Rhs, _VrhsRest, _Coeff)
	    )
	    ;
	    true
	),
	check_index_global_list(T).

index_integrity_check(Mesg) :-
    writeln(Mesg),
    not(not(check_index_global_list)).

well_formed_rhs(X) :-
    (well_formed_rhs1(X) ->
	true
    ;
	writeln("bad rhs"-X),
	abort
    ).

well_formed_rhs1(X) :-
    nonvar(X),
    X == [],
    !.
well_formed_rhs1(X) :- 
    nonvar(X),
    X = [H|T],
    H = mono(C,_V),
    nonvar(C),
    well_formed_rhs1(T).

test_rhs([_H|_T]) :- !.
test_rhs([H]) :-
    (nonvar(H), H = [A|B], var(A), var(B) ->
	writeln("Bogus rhs found")
    ;
	true
    ).

obj_pick_pivot([], V, V).
obj_pick_pivot([mono(C,V)|T], Vin, Vout) :-
	( only_pos_coeff(V) ->
		Vout = unbounded(C,V)
	; Vin = none ->
		obj_pick_pivot(T, pivot(C,V), Vout)
% Test is reversed here because we used the previously
% split list without the pre multiplication
	; Vin = pivot(Cmax,_), C > Cmax ->
		obj_pick_pivot(T, pivot(C,V), Vout)
	; Vin = pivot(Cmax,_), C =:= Cmax ->
		% need cycle breaking code here
		% writeln(error, "possible cycle detected in pick_pivot/3"),
		obj_pick_pivot(T, Vin, Vout)
	;
		obj_pick_pivot(T, Vin, Vout)
	).

mytest :- true.

obj_pivot(pivot(_,Vpivot)) :-
	lin_constr(Vpivot, Constr),
	getval(global_list, Store),
	obj_choose_leaving(Constr, Vpivot, leave(none,_,_), Leave),
	mywriteln('*****chosen_leaving'(Leave)),
	Leave = leave(_,Coeff,Vleave),
	lin_rhs(Vleave, RhsL),
	insert_var_mono(RhsL, mono('MONE',Vleave), RhsPiv00),
	sort(2, >=, RhsPiv00, RhsPiv0),
% not in sorted order anymore !!!
	Coeff1 is -Coeff,
	divide_remove(Coeff1, Vpivot, RhsPiv0, RhsPiv),
	delete_rhs(Vleave),
	add_rhs(Vpivot, RhsPiv),
	mywriteln("&&&RHSPIV" = RhsPiv),
%	lin_constr(Vpivot, ConstrPiv),
	substitute_single_var(Vpivot, RhsPiv, Store).

:- mode obj_choose_leaving(+,?,+,-).
obj_choose_leaving([], _Vpivot, Max, Max) :-
	Max = leave(MaxC,_,_),			% debugging
        !,
	( MaxC = 'ONE' ->
		linerr("no leave var found in choose_leaving")
	;
		true
	).
obj_choose_leaving([V|T], Vpivot, Max0, Max) :-
	( var(V) ->
		lin_rhs(V, Rhs),
		mywriteln(choose_leaving(V,Rhs)),
		get_constant(Rhs, Con, Rhs1),
		get_var_coeff(Rhs1, Vpivot, Coeff),
		( Coeff < 'ZERO' ->
			Quot is Con/(-Coeff),
			FQuot is Quot * 1.0,
			mywriteln(quot = FQuot),
			Max0 = leave(MaxC,_,_),
			( MaxC = none ->
				Max1 = leave(Quot,Coeff,V)
			; Quot < MaxC ->
				Max1 = leave(Quot,Coeff,V)
			; Quot > MaxC ->
				Max1 = Max0
			;
				Max0 = leave(_,_,Max0V),
				lin_rhs(Max0V,Max0Rhs0),
	                        insert_var_mono(Max0Rhs0,
						mono('MONE',Max0V), Max0Rhs),
	                        sort(2, >=, Max0Rhs, Max0Rhs1),
	                        insert_var_mono(Rhs,
						mono('MONE',V), Rhs2),
	                        sort(2, >=, Rhs2, Rhs3),
				break_tie(V,Rhs3,Max0V,Max0Rhs1,Result,ResultRhs),
				get_var_coeff(ResultRhs,Vpivot,VCoeff),
				% DEBUGGING
				% writeln(break_coeff(VCoeff)),
				Max1 = leave(Quot,VCoeff,Result)
			)
		;
			Max1 = Max0
		)
	;
		Max1 = Max0
	),
	obj_choose_leaving(T, Vpivot, Max1, Max).


mywriteln(_).



:- comment(categories, ["Constraints"]).
:- comment(summary, "Linear constraints over rational numbers (unsupported)").
:- comment(author, "Pierre Lim, ECRC").
:- comment(date, "1993").

:- comment(/($=, 2), [
	summary:"Holds iff the rational T1 is equal to the rational term T2.

",
	template:"?T1 $= ?T2",
	desc:html("   The rational constraint solver checks to see if the equality can be
   added to the constraint store.  A modified Gaussian algorithm is used to
   perform the test.

<P>
"),
	args:["?X" : "A rational term.", "?Y" : "A rational term."],
	resat:"   No.",
	fail_if:"   Fails if adding the constraint T1 $= T2 to the constraint store produces\n   an infeasible set of constraints.\n\n",
	eg:"
Success:
2*X + Y $= 16, X + 2*Y $= 17, Y = 6.  gives X=5.

Fail:
4 $= 8/5.


",
	see_also:[]]).

:- comment(/($>, 2), [
	summary:"Holds iff the the rational term T1 is strictly greater than the rational
term T2.

",
	template:"?T1 $> ?T2",
	desc:html("   The rational constraint solver tests the conjunction of the strict
   inequality with the constraint store for feasibility.

<P>
"),
	args:["?T1" : "A rational term.", "?T2" : "A rational term."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	eg:"
Success:
3/4 $> 1/2.

Fail:
2/3 $> 8.


",
	see_also:[]]).

:- comment(/($>=, 2), [
	summary:"Holds iff the the rational term T1 is greater than or equal to the rational
term T2.

",
	template:"?T1 $>= ?T2",
	desc:html("   Determines whether the inequality together with the constraint store
   forms a feasible system.  A modified Simplex algorithm is used to make
   the test.

<P>
"),
	args:["?T1" : "A rational term.", "?T2" : "A rational term."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	eg:"
Success:
X + Y $>= 3.

Fail:
12/7 $>= 13/5.


",
	see_also:[]]).

:- comment(/($<>, 2), [
	summary:"Holds iff the the rational term T1 is different from the rational term T2.

",
	template:"?T1 $<> ?T2",
	desc:html("   The rational constraint solver checks if the rational term T1 is
   different from the rational term T2.

<P>
"),
	args:["?T1" : "A rational term.", "?T2" : "A rational term."],
	resat:"   No.",
	fail_if:"   Fails if the rational term (T1 - T2) becomes ground, taking a value of\n   zero.\n\n",
	eg:"
Success:
    15/7 $<> 23/4.

Fail:
    4 $<> 8/2.


",
	see_also:[]]).

:- comment(/($<, 2), [
	summary:"Holds iff the the rational term T1 is strictly less than the rational term
T2.

",
	template:"?T1 $< ?T2",
	desc:html("   The rational constraint solver tests the conjunction of the strict
   inequality with the constraint store for feasibility.

<P>
"),
	args:["?T1" : "A rational term.", "?T2" : "A rational term."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	eg:"
Success:
5/17 $< 32/4.

Fail:
3/5 $< 2/5.


",
	see_also:[]]).

:- comment(/($<=, 2), [
	summary:"Holds iff the the rational term T1 is less than or equal to the rational
term T2.

",
	template:"?T1 $<= ?T2",
	desc:html("   Determines whether the inequality together with the constraint store
   forms a feasible system.  A modified Simplex algorithm is used to make
   the test.

<P>
"),
	args:["?T1" : "A rational term.", "?T2" : "A rational term."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	eg:"
Success:
15/7 $=< 23/4.

Fail:
4/3 $=< 8/3.


",
	see_also:[]]).

:- comment(/(rmax, 1), [
	summary:"The objective function Func is maximized with respect to a set of
constraints in normal form.

",
	template:"rmax(?Func)",
	desc:html("   This is one of two optimization predicates provided by the ECLiPSe
   compiler, the other being rmin/1.

<P>
   rmax/1 amounts to finding a feasible solution where the objective
   function Func is maximal with respect to the constraints store.

<P>
"),
	args:["?Func" : "A rational term."],
	resat:"",
	fail_if:"   None\n\n",
	eg:"
Success:
X $=< 3, 2 * X $= Y, rmax(X + Y).
Succeeds if X = 3 and Y = 6.

Fail:
X $=< 3, rmax(X+Y), X $= 2 * Y.
Here rmax/1 fails because insufficient information
is available to find a solution when it is called.



",
	see_also:[/(rmin, 1)]]).

:- comment(/(rmin, 1), [
	summary:"The objective function Func is minimized with respect to a set of
constraints in normal form.

",
	template:"rmin(?Func)",
	desc:html("   This is one of two optimization predicates provided by the ECLiPSe
   compiler, the other being rmax/1.

<P>
   rmin/1 amounts to finding a feasible solution where the objective
   function Func is minimal with respect to the constraints store.

<P>
"),
	args:["?Func" : "A rational term."],
	resat:"",
	fail_if:"   None.\n\n",
	eg:"
Success:
X $>= 3, rmin(X).
The minimum value of X that satisfies the constraint is 3.



",
	see_also:[/(rmax, 1)]]).
