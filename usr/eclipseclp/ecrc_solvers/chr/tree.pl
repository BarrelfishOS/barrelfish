
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- op(100, xfx, ~).
:- op(100, xfx, #).
smaller_var(_938, _939) :- var(_939), (nonvar(_938) ; meta(_939), (meta(_938) -> _939 @< _938 ; true)).
same_functor(_975, _970) :- functor(_975, _969, _968), functor(_970, _969, _968).
equate([], []).
equate([_1011|_1006], [_1010|_1005]) :- ~(_1011, _1010), equate(_1006, _1005).



%%% Callables for not_equate / 2

'CHRlabel_with'(not_equate(A, B), C, D) ?-
	coca(try_clause(D, not_equate(A, B), not_equate(E, F), true)),
	coca(clause_fired(D)),
	'CHR='(C, clause_not_equate(A, B)).
~(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, ~(A, B))),
	'CHR~_2'(~(A, B), D, E, C).



%%% Rules handling for ~ / 2

'CHR~_2'(~(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHR~_2'(~(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHR~_2'(~(A, A), B, C, D) ?-
	coca(try_rule(D, ~(A, A), ident, ~(E, E), replacement, true, true)),
	!,
	'CHRkill'(B),
	coca(fired_rule(ident)).
'CHR~_2'(~(A, B), C, D, E) ?-
	coca(try_rule(E, ~(A, B), orient, ~(F, G), replacement, smaller_var(F, G), ~(G, F))),
	no_delayed_goals(smaller_var(A, B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(orient)),
	~(B, A).
'CHR~_2'(~(A, B), C, D, E) ?-
	coca(try_rule(E, ~(A, B), decompose, ~(F, G), replacement, (nonvar(F), nonvar(G)), (same_functor(F, G), F =.. [H|I], G =.. [J|K], equate(I, K)))),
	no_delayed_goals((nonvar(A), nonvar(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(decompose)),
	same_functor(A, B),
	A =.. [L|M],
	B =.. [N|O],
	equate(M, O).
'CHR~_2'(~(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHR~_2__3'(F, [A], [G], H),
	coca(try_double(E, ~(A, B), H, ~(A, G), ~(I, J), ~(I, K), keep_second, (var(I), K @=< J), ~(K, J), simplify)),
	no_delayed_goals((var(A), G @=< B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(simplify)),
	~(G, B).
'CHR~_2'(~(A, B), C, D, E) :-
	'CHR~_2__2'(~(A, B), C, D, E).
'CHR~_2__3'(['CHR~_2'(~(A, B), C, D, E)|F], [A], [G], H) ?-
	'CHRvar'(C),
	'CHR='([B], [G]),
	'CHR='(E, H).
'CHR~_2__3'([A|B], C, D, E) :-
	'CHR~_2__3'(B, C, D, E).
:- set_flag('CHR~_2__3' / 4, leash, notrace).
:- set_flag('CHR~_2' / 4, leash, notrace).
:- current_macro('CHR~_2' / 4, _3224, _3225, _3226) -> true ; define_macro('CHR~_2' / 4, tr_chr / 2, [write]).
'CHR~_2__2'(A, B, C, D) :-
	'CHR~_2__4'(A, B, C, D).
:- set_flag('CHR~_2__2' / 4, leash, notrace).
'CHR~_2__4'(~(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHR~_2__4__5'(F, C, ~(A, B), D, E).
'CHR~_2__4'(~(A, B), C, D, E) :-
	'CHR~_2__4__6'(~(A, B), C, D, E).
:- set_flag('CHR~_2__4' / 4, leash, notrace).
'CHR~_2__4__5'(['CHR~_2'(~(A, B), C, D, E)|F], G, ~(A, H), I, J) ?-
	'CHRvar'(C),
	coca(try_double(J, ~(A, H), E, ~(A, B), ~(K, L), ~(K, M), keep_first, (var(K), L @=< M), ~(L, M), simplify)),
	no_delayed_goals((var(A), H @=< B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(simplify)),
	'CHR~_2__4__5'(F, G, ~(A, H), I, J),
	~(H, B).
'CHR~_2__4__5'([A|B], C, D, E, F) :-
	'CHR~_2__4__5'(B, C, D, E, F).
'CHR~_2__4__5'([], A, B, C, D) :-
	'CHR~_2__4__6'(B, A, C, D).
:- set_flag('CHR~_2__4__5' / 5, leash, notrace).
'CHR~_2__4__6'(~(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHR~_2__4__6__7'(F, C, ~(A, B), D, E).
'CHR~_2__4__6'(~(A, B), C, D, E) :-
	'CHR~_2__4__6__8'(~(A, B), C, D, E).
:- set_flag('CHR~_2__4__6' / 4, leash, notrace).
'CHR~_2__4__6__7'(['CHR#_2'(#(A, B), C, D, E)|F], G, ~(A, H), I, J) ?-
	'CHRvar'(C),
	coca(try_double(J, ~(A, H), E, #(A, B), ~(K, L), #(K, M), keep_first, (var(K), (nonvar(L), nonvar(M), same_functor(L, M) -> L @=< M ; true)), #(L, M), simplify)),
	no_delayed_goals((var(A), (nonvar(H), nonvar(B), same_functor(H, B) -> H @=< B ; true))),
	!,
	'CHRkill'(C),
	coca(fired_rule(simplify)),
	'CHR~_2__4__6__7'(F, G, ~(A, H), I, J),
	#(H, B).
'CHR~_2__4__6__7'([A|B], C, D, E, F) :-
	'CHR~_2__4__6__7'(B, C, D, E, F).
'CHR~_2__4__6__7'([], A, B, C, D) :-
	'CHR~_2__4__6__8'(B, A, C, D).
:- set_flag('CHR~_2__4__6__7' / 5, leash, notrace).
'CHR~_2__4__6__8'(~(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHR~_2__4__6__8__9'(F, C, ~(A, B), D, E).
'CHR~_2__4__6__8'(~(A, B), C, D, E) :-
	'CHR~_2__4__6__8__10'(~(A, B), C, D, E).
:- set_flag('CHR~_2__4__6__8' / 4, leash, notrace).
'CHR~_2__4__6__8__9'(['CHR#_2'(#(A, B), C, D, E)|F], G, ~(A, H), I, J) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(propagate, G, C, I, D),
	coca(try_double(J, ~(A, H), E, #(A, B), ~(K, L), #(K, M), augmentation, (var(K), same_functor(M, L), M @> L), #(M, L), propagate)),
	no_delayed_goals((var(A), same_functor(B, H), B @> H)),
	!,
	coca(fired_rule(propagate)),
	'CHR~_2__4__6__8__9'(F, G, ~(A, H), I, J),
	#(B, H).
'CHR~_2__4__6__8__9'([A|B], C, D, E, F) :-
	'CHR~_2__4__6__8__9'(B, C, D, E, F).
'CHR~_2__4__6__8__9'([], A, B, C, D) :-
	'CHR~_2__4__6__8__10'(B, A, C, D).
:- set_flag('CHR~_2__4__6__8__9' / 5, leash, notrace).
'CHR~_2__4__6__8__10'(~(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, ~(A, B)], 'CHR~_2'(~(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHR~_2__4__6__8__10' / 4, leash, notrace).
#(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, #(A, B))),
	'CHR#_2'(#(A, B), D, E, C).



%%% Rules handling for # / 2

'CHR#_2'(#(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHR#_2'(#(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHR#_2'(#(A, A), B, C, D) ?-
	coca(try_rule(D, #(A, A), ident, #(E, E), replacement, true, fail)),
	!,
	'CHRkill'(B),
	coca(fired_rule(ident)),
	fail.
'CHR#_2'(#(A, B), C, D, E) ?-
	coca(try_rule(E, #(A, B), orient, #(F, G), replacement, smaller_var(F, G), #(G, F))),
	no_delayed_goals(smaller_var(A, B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(orient)),
	#(B, A).
'CHR#_2'(#(A, B), C, D, E) ?-
	coca(try_rule(E, #(A, B), decompose, #(F, G), replacement, (nonvar(F), nonvar(G)), same_functor(F, G) -> (F =.. [H|I], G =.. [J|K], not_equate(I, K)) ; true)),
	no_delayed_goals((nonvar(A), nonvar(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(decompose)),
	(
	    same_functor(A, B)
	->
	    A =.. [L|M],
	    B =.. [N|O],
	    not_equate(M, O)
	;
	    true
	).
'CHR#_2'(#(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHR#_2__12'(F, [A], [G], H),
	coca(try_double(E, #(A, B), H, ~(A, G), #(I, J), ~(I, K), keep_second, (var(I), (nonvar(K), nonvar(J), same_functor(K, J) -> K @=< J ; true)), #(K, J), simplify)),
	no_delayed_goals((var(A), (nonvar(G), nonvar(B), same_functor(G, B) -> G @=< B ; true))),
	!,
	'CHRkill'(C),
	coca(fired_rule(simplify)),
	#(G, B).
'CHR#_2'(#(A, B), C, D, E) :-
	'CHR#_2__11'(#(A, B), C, D, E).
'CHR#_2__12'(['CHR~_2'(~(A, B), C, D, E)|F], [A], [G], H) ?-
	'CHRvar'(C),
	'CHR='([B], [G]),
	'CHR='(E, H).
'CHR#_2__12'([A|B], C, D, E) :-
	'CHR#_2__12'(B, C, D, E).
:- set_flag('CHR#_2__12' / 4, leash, notrace).
:- set_flag('CHR#_2' / 4, leash, notrace).
:- current_macro('CHR#_2' / 4, _6678, _6679, _6680) -> true ; define_macro('CHR#_2' / 4, tr_chr / 2, [write]).
'CHR#_2__11'(A, B, C, D) :-
	'CHR#_2__13'(A, B, C, D).
:- set_flag('CHR#_2__11' / 4, leash, notrace).
'CHR#_2__13'(#(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHR#_2__13__14'(F, C, #(A, B), D, E).
'CHR#_2__13'(#(A, B), C, D, E) :-
	'CHR#_2__13__15'(#(A, B), C, D, E).
:- set_flag('CHR#_2__13' / 4, leash, notrace).
'CHR#_2__13__14'(['CHR~_2'(~(A, B), C, D, E)|F], G, #(A, H), I, J) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(propagate, G, C, I, D),
	coca(try_double(J, #(A, H), E, ~(A, B), #(K, L), ~(K, M), augmentation, (var(K), same_functor(L, M), L @> M), #(L, M), propagate)),
	no_delayed_goals((var(A), same_functor(H, B), H @> B)),
	!,
	coca(fired_rule(propagate)),
	'CHR#_2__13__14'(F, G, #(A, H), I, J),
	#(H, B).
'CHR#_2__13__14'([A|B], C, D, E, F) :-
	'CHR#_2__13__14'(B, C, D, E, F).
'CHR#_2__13__14'([], A, B, C, D) :-
	'CHR#_2__13__15'(B, A, C, D).
:- set_flag('CHR#_2__13__14' / 5, leash, notrace).
'CHR#_2__13__15'(#(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, #(A, B)], 'CHR#_2'(#(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHR#_2__13__15' / 4, leash, notrace).



%%% Prolog clauses for not_equate / 2

clause_not_equate([A|B], [C|D]) :-
	(
	    #(A, C)
	;
	    not_equate(B, D)
	).
:- current_macro(clause_not_equate / 2, _7638, _7639, _7640) -> true ; define_macro(clause_not_equate / 2, tr_chr / 2, [write]).
not_equate(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, not_equate(A, B))),
	'CHRnot_equate_2'(not_equate(A, B), D, E, C).



%%% Rules handling for not_equate / 2

'CHRnot_equate_2'(not_equate(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRnot_equate_2'(not_equate(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRnot_equate_2'(not_equate([], []), A, B, C) ?-
	coca(try_rule(C, not_equate([], []), anonymous("0"), not_equate([], []), replacement, true, fail)),
	!,
	'CHRkill'(A),
	coca(fired_rule(anonymous("0"))),
	fail.
'CHRnot_equate_2'(not_equate([A], [B]), C, D, E) ?-
	coca(try_rule(E, not_equate([A], [B]), anonymous("1"), not_equate([F], [G]), replacement, true, #(F, G))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("1"))),
	#(A, B).
'CHRnot_equate_2'(not_equate(A, B), C, D, E) :-
	'CHRnot_equate_2__16'(not_equate(A, B), C, D, E).
:- set_flag('CHRnot_equate_2' / 4, leash, notrace).
:- current_macro('CHRnot_equate_2' / 4, _8274, _8275, _8276) -> true ; define_macro('CHRnot_equate_2' / 4, tr_chr / 2, [write]).
'CHRnot_equate_2__16'(A, B, C, D) :-
	'CHRnot_equate_2__17'(A, B, C, D).
:- set_flag('CHRnot_equate_2__16' / 4, leash, notrace).
'CHRnot_equate_2__17'(not_equate(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, not_equate(A, B)], 'CHRnot_equate_2'(not_equate(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRnot_equate_2__17' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
