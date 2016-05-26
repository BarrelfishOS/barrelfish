
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- op(1180, fx, chr_if).
:- op(900, fy, chr_not).
:- op(900, fy, chr_chk).
:- op(1000, xfy, and).
:- op(1100, xfy, or).
eval_condition(_1108, _1094) :- nonvar(_1108), (copy_term(_1108, _1107), call(_1107) -> _1108 = _1107, _1094 = true ; _1094 = fail).



%%% Callables for or / 2

'CHRlabel_with'(or(A, B), C, D) ?-
	coca(try_clause(D, or(A, B), or(E, F), (nonvar(E), nonvar(F)))),
	no_global_bindings((nonvar(A), nonvar(B)), or(A, B)),
	coca(clause_fired(D)),
	'CHR='(C, clause_or(A, B)).
chr_if(A) :-
	'CHRgen_num'(B),
	coca(add_one_constraint(B, chr_if(A))),
	'CHRchr_if_1'(chr_if(A), C, D, B).



%%% Rules handling for chr_if / 1

'CHRchr_if_1'(chr_if(A), B, C, D) :-
	'CHRnonvar'(B),
	!.
'CHRchr_if_1'(chr_if(A then B), C, D, E) ?-
	coca(try_rule(E, chr_if(A then B), if_then, chr_if(F then G), replacement, eval_condition(F, H), H == true -> G)),
	no_global_bindings(eval_condition(A, I), chr_if(A then B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(if_then)),
	(
	    I == true
	->
	    B
	).
'CHRchr_if_1'(chr_if(A then B else C), D, E, F) ?-
	coca(try_rule(F, chr_if(A then B else C), if_then_else, chr_if(G then H else I), replacement, eval_condition(G, J), J == true -> H ; I)),
	no_global_bindings(eval_condition(A, K), chr_if(A then B else C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(if_then_else)),
	(
	    K == true
	->
	    B
	;
	    C
	).
'CHRchr_if_1'(chr_if(A), B, C, D) :-
	'CHRchr_if_1__0'(chr_if(A), B, C, D).
:- set_flag('CHRchr_if_1' / 4, leash, notrace).
:- current_macro('CHRchr_if_1' / 4, _2236, _2237, _2238) -> true ; define_macro('CHRchr_if_1' / 4, tr_chr / 2, [write]).
'CHRchr_if_1__0'(A, B, C, D) :-
	'CHRchr_if_1__1'(A, B, C, D).
:- set_flag('CHRchr_if_1__0' / 4, leash, notrace).
'CHRchr_if_1__1'(chr_if(A), B, C, D) :-
	(
	    'CHRvar'(B)
	->
	    'CHRdelay'([B, chr_if(A)], 'CHRchr_if_1'(chr_if(A), B, C, D))
	;
	    true
	).
:- set_flag('CHRchr_if_1__1' / 4, leash, notrace).
chr_not(A) :-
	'CHRgen_num'(B),
	coca(add_one_constraint(B, chr_not(A))),
	'CHRchr_not_1'(chr_not(A), C, D, B).



%%% Rules handling for chr_not / 1

'CHRchr_not_1'(chr_not(A), B, C, D) :-
	'CHRnonvar'(B),
	!.
'CHRchr_not_1'(chr_not(chr_not(A)), B, C, D) ?-
	coca(try_rule(D, chr_not(chr_not(A)), not_not, chr_not(chr_not(E)), replacement, true, chr_chk(E))),
	!,
	'CHRkill'(B),
	coca(fired_rule(not_not)),
	chr_chk(A).
'CHRchr_not_1'(chr_not(A), B, C, D) ?-
	coca(try_rule(D, chr_not(A), not, chr_not(E), replacement, eval_condition(E, F), F == fail)),
	no_global_bindings(eval_condition(A, G), chr_not(A)),
	!,
	'CHRkill'(B),
	coca(fired_rule(not)),
	G == fail.
'CHRchr_not_1'(chr_not(A), B, C, D) :-
	'CHRchr_not_1__2'(chr_not(A), B, C, D).
:- set_flag('CHRchr_not_1' / 4, leash, notrace).
:- current_macro('CHRchr_not_1' / 4, _3121, _3122, _3123) -> true ; define_macro('CHRchr_not_1' / 4, tr_chr / 2, [write]).
'CHRchr_not_1__2'(A, B, C, D) :-
	'CHRchr_not_1__3'(A, B, C, D).
:- set_flag('CHRchr_not_1__2' / 4, leash, notrace).
'CHRchr_not_1__3'(chr_not(A), B, C, D) :-
	(
	    'CHRvar'(B)
	->
	    'CHRdelay'([B, chr_not(A)], 'CHRchr_not_1'(chr_not(A), B, C, D))
	;
	    true
	).
:- set_flag('CHRchr_not_1__3' / 4, leash, notrace).
chr_chk(A) :-
	'CHRgen_num'(B),
	coca(add_one_constraint(B, chr_chk(A))),
	'CHRchr_chk_1'(chr_chk(A), C, D, B).



%%% Rules handling for chr_chk / 1

'CHRchr_chk_1'(chr_chk(A), B, C, D) :-
	'CHRnonvar'(B),
	!.
'CHRchr_chk_1'(chr_chk(A), B, C, D) ?-
	coca(try_rule(D, chr_chk(A), chk, chr_chk(E), replacement, eval_condition(E, F), F == true)),
	no_global_bindings(eval_condition(A, G), chr_chk(A)),
	!,
	'CHRkill'(B),
	coca(fired_rule(chk)),
	G == true.
'CHRchr_chk_1'(chr_chk(A), B, C, D) :-
	'CHRchr_chk_1__4'(chr_chk(A), B, C, D).
:- set_flag('CHRchr_chk_1' / 4, leash, notrace).
:- current_macro('CHRchr_chk_1' / 4, _3863, _3864, _3865) -> true ; define_macro('CHRchr_chk_1' / 4, tr_chr / 2, [write]).
'CHRchr_chk_1__4'(A, B, C, D) :-
	'CHRchr_chk_1__5'(A, B, C, D).
:- set_flag('CHRchr_chk_1__4' / 4, leash, notrace).
'CHRchr_chk_1__5'(chr_chk(A), B, C, D) :-
	(
	    'CHRvar'(B)
	->
	    'CHRdelay'([B, chr_chk(A)], 'CHRchr_chk_1'(chr_chk(A), B, C, D))
	;
	    true
	).
:- set_flag('CHRchr_chk_1__5' / 4, leash, notrace).
A then B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A then B)),
	'CHRthen_2'(A then B, D, E, C).



%%% Rules handling for then / 2

'CHRthen_2'(A then B, C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHRthen_2'(A then B, C, D, E) ?-
	coca(try_rule(E, A then B, then, F then G, replacement, eval_condition(F, H), H == true -> G)),
	no_global_bindings(eval_condition(A, I), A then B),
	!,
	'CHRkill'(C),
	coca(fired_rule(then)),
	(
	    I == true
	->
	    B
	).
'CHRthen_2'(A then B, C, D, E) :-
	'CHRthen_2__6'(A then B, C, D, E).
:- set_flag('CHRthen_2' / 4, leash, notrace).
:- current_macro('CHRthen_2' / 4, _4618, _4619, _4620) -> true ; define_macro('CHRthen_2' / 4, tr_chr / 2, [write]).
'CHRthen_2__6'(A, B, C, D) :-
	'CHRthen_2__7'(A, B, C, D).
:- set_flag('CHRthen_2__6' / 4, leash, notrace).
'CHRthen_2__7'(A then B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A then B], 'CHRthen_2'(A then B, C, D, E))
	;
	    true
	).
:- set_flag('CHRthen_2__7' / 4, leash, notrace).



%%% Prolog clauses for or / 2

clause_or(A, B) :-
	(
	    A
	;
	    B
	).
:- current_macro(clause_or / 2, _4950, _4951, _4952) -> true ; define_macro(clause_or / 2, tr_chr / 2, [write]).
or(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, or(A, B))),
	'CHRor_2'(or(A, B), D, E, C).



%%% Rules handling for or / 2

'CHRor_2'(or(A, B), C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHRor_2'(or(A, B), C, D, E) :-
	'CHRor_2__8'(or(A, B), C, D, E).
:- set_flag('CHRor_2' / 4, leash, notrace).
:- current_macro('CHRor_2' / 4, _5284, _5285, _5286) -> true ; define_macro('CHRor_2' / 4, tr_chr / 2, [write]).
'CHRor_2__8'(A, B, C, D) :-
	'CHRor_2__9'(A, B, C, D).
:- set_flag('CHRor_2__8' / 4, leash, notrace).
'CHRor_2__9'(or(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, or(A, B)], 'CHRor_2'(or(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRor_2__9' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
