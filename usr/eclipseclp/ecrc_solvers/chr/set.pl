
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- use_module(s_lists).
:- op(700, xfx, [in, notin, incl, eq, #]).



%%% Callables for set / 3

'CHRlabel_with'(set(A, B, C), D, E) ?-
	coca(try_clause(E, set(A, B, C), set(F, G, H), true)),
	coca(clause_fired(E)),
	'CHR='(D, clause_set(A, B, C)).



%%% Prolog clauses for set / 3

clause_set(A, B, C) :-
	s_member(D, C),
	\+ s_member(D, B),
	!,
	(
	    in(D, A)
	;
	    notin(D, A)
	).
:- current_macro(clause_set / 3, _9150, _9151, _9152) -> true ; define_macro(clause_set / 3, tr_chr / 2, [write]).
set(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, set(A, B, C))),
	'CHRset_3'(set(A, B, C), E, F, D).



%%% Rules handling for set / 3

'CHRset_3'(set(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHRset_3'(set(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHRset_3'(set(A, B, C), D, E, F) ?-
	coca(try_rule(F, set(A, B, C), anonymous("0"), set(G, H, I), replacement, s_equality(H, I), G = H)),
	no_global_bindings(s_equality(B, C), set(A, B, C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("0"))),
	A = B.
'CHRset_3'(set(A, B, C), D, E, F) ?-
	coca(try_rule(F, set(A, B, C), anonymous("1"), set(G, H, I), replacement, \+ s_included(H, I), fail)),
	no_global_bindings(\+ s_included(B, C), set(A, B, C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("1"))),
	fail.
'CHRset_3'(set(A, B, C), D, E, F) ?-
	coca(try_rule(F, set(A, B, C), ground, set(G, H, I), replacement, is_set(G), (s_included(H, G), s_included(G, I)))),
	no_global_bindings(is_set(A), set(A, B, C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(ground)),
	s_included(B, A),
	s_included(A, C).
'CHRset_3'(set(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__34'(G, [A], [H], I),
	coca(try_double(F, set(A, B, C), I, #(A, H), set(J, K, L), #(J, M), replacement, s_card(K, M), J = K, anonymous("9"))),
	no_global_bindings(s_card(B, H), (set(A, B, C), #(A, H))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("9"))),
	A = B.
'CHRset_3'(set(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__35'(G, [A], [H], I),
	coca(try_double(F, set(A, B, C), I, #(A, H), set(J, K, L), #(J, M), replacement, s_card(L, M), J = L, anonymous("10"))),
	no_global_bindings(s_card(C, H), (set(A, B, C), #(A, H))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("10"))),
	A = C.
'CHRset_3'(set(A, B, C), D, E, F) :-
	'CHRset_3__33'(set(A, B, C), D, E, F).
'CHRset_3__34'(['CHR#_2'(#(A, B), C, D, E)|F], [A], [G], H) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([B], [G]),
	'CHR='(E, H).
'CHRset_3__34'([A|B], C, D, E) :-
	'CHRset_3__34'(B, C, D, E).
:- set_flag('CHRset_3__34' / 4, leash, notrace).
'CHRset_3__35'(['CHR#_2'(#(A, B), C, D, E)|F], [A], [G], H) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([B], [G]),
	'CHR='(E, H).
'CHRset_3__35'([A|B], C, D, E) :-
	'CHRset_3__35'(B, C, D, E).
:- set_flag('CHRset_3__35' / 4, leash, notrace).
:- set_flag('CHRset_3' / 4, leash, notrace).
:- current_macro('CHRset_3' / 4, _10943, _10944, _10945) -> true ; define_macro('CHRset_3' / 4, tr_chr / 2, [write]).
'CHRset_3__33'(A, B, C, D) :-
	'CHRset_3__36'(A, B, C, D).
:- set_flag('CHRset_3__33' / 4, leash, notrace).
'CHRset_3__36'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__37'(G, D, set(A, B, C), E, F).
'CHRset_3__36'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36' / 4, leash, notrace).
'CHRset_3__36__37'(['CHRin_2'(in(A, B), C, D, E)|F], G, set(B, H, I), J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, set(B, H, I), E, in(A, B), set(L, M, N), in(O, L), keep_first, ((nonvar(O), s_insertion(O, M, P)), 'CHRkeep_heads_checking'(set(L, M, N), Q, set(L, P, N), R)), (s_memberchk(O, N), ('CHRhead_not_kept'(R) -> set(L, P, N) ; true)), belonging)),
	no_global_bindings(((nonvar(A), s_insertion(A, H, S)), 'CHRkeep_heads_checking'(set(B, H, I), G, set(B, S, I), T)), (set(B, H, I), in(A, B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(belonging)),
	'CHRset_3__36__37'(F, G, set(B, H, I), J, K),
	s_memberchk(A, I),
	(
	    'CHRhead_not_kept'(T)
	->
	    set(B, S, I)
	;
	    true
	).
'CHRset_3__36__37'([A|B], C, D, E, F) :-
	'CHRset_3__36__37'(B, C, D, E, F).
'CHRset_3__36__37'([], A, B, C, D) :-
	'CHRset_3__36__38'(B, A, C, D).
:- set_flag('CHRset_3__36__37' / 5, leash, notrace).
'CHRset_3__36__38'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__39'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38' / 4, leash, notrace).
'CHRset_3__36__38__39'(['CHRnotin_2'(notin(A, B), C, D, E)|F], G, set(B, H, I), J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, set(B, H, I), E, notin(A, B), set(L, M, N), notin(O, L), keep_first, ((nonvar(O), list2set([O], P), s_delta(N, P, Q)), 'CHRkeep_heads_checking'(set(L, M, N), R, set(L, M, Q), S)), (\+ s_memberchk(O, M), ('CHRhead_not_kept'(S) -> set(L, M, Q) ; true)), not_belonging)),
	no_global_bindings(((nonvar(A), list2set([A], T), s_delta(I, T, U)), 'CHRkeep_heads_checking'(set(B, H, I), G, set(B, H, U), V)), (set(B, H, I), notin(A, B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(not_belonging)),
	'CHRset_3__36__38__39'(F, G, set(B, H, I), J, K),
	\+ s_memberchk(A, H),
	(
	    'CHRhead_not_kept'(V)
	->
	    set(B, H, U)
	;
	    true
	).
'CHRset_3__36__38__39'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__39'(B, C, D, E, F).
'CHRset_3__36__38__39'([], A, B, C, D) :-
	'CHRset_3__36__38__40'(B, A, C, D).
:- set_flag('CHRset_3__36__38__39' / 5, leash, notrace).
'CHRset_3__36__38__40'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__41'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40' / 4, leash, notrace).
'CHRset_3__36__38__40__41'(['CHRincl_2'(incl(A, B), C, D, E)|F], G, set(B, H, I), J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, set(B, H, I), E, incl(A, B), set(L, M, N), incl(O, L), keep_first, ((is_set(O), s_union(O, M, P)), 'CHRkeep_heads_checking'(set(L, M, N), Q, set(L, P, N), R)), 'CHRhead_not_kept'(R) -> set(L, P, N) ; true, anonymous("2"))),
	no_global_bindings(((is_set(A), s_union(A, H, S)), 'CHRkeep_heads_checking'(set(B, H, I), G, set(B, S, I), T)), (set(B, H, I), incl(A, B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("2"))),
	'CHRset_3__36__38__40__41'(F, G, set(B, H, I), J, K),
	(
	    'CHRhead_not_kept'(T)
	->
	    set(B, S, I)
	;
	    true
	).
'CHRset_3__36__38__40__41'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__41'(B, C, D, E, F).
'CHRset_3__36__38__40__41'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__41' / 5, leash, notrace).
'CHRset_3__36__38__40__42'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__43'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42' / 4, leash, notrace).
'CHRset_3__36__38__40__42__43'(['CHRincl_2'(incl(A, B), C, D, E)|F], G, set(A, H, I), J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, set(A, H, I), E, incl(A, B), set(L, M, N), incl(L, O), keep_first, ((is_set(O), s_intersection(O, N, P)), 'CHRkeep_heads_checking'(set(L, M, N), Q, set(L, M, P), R)), 'CHRhead_not_kept'(R) -> set(L, M, P) ; true, anonymous("3"))),
	no_global_bindings(((is_set(B), s_intersection(B, I, S)), 'CHRkeep_heads_checking'(set(A, H, I), G, set(A, H, S), T)), (set(A, H, I), incl(A, B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("3"))),
	'CHRset_3__36__38__40__42__43'(F, G, set(A, H, I), J, K),
	(
	    'CHRhead_not_kept'(T)
	->
	    set(A, H, S)
	;
	    true
	).
'CHRset_3__36__38__40__42__43'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__43'(B, C, D, E, F).
'CHRset_3__36__38__40__42__43'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__43' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__45'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__45'(['CHRincl_set_5'(incl_set(A, B, C, D, E), F, G, H)|I], J, set(D, K, L), M, N) ?-
	'CHRvar'(F),
	coca(try_double(N, set(D, K, L), H, incl_set(A, B, C, D, E), set(O, P, Q), incl_set(R, S, T, O, U), keep_first, s_included(T, P), U = true, anonymous("6"))),
	no_global_bindings(s_included(C, K), (set(D, K, L), incl_set(A, B, C, D, E))),
	!,
	'CHRkill'(F),
	coca(fired_rule(anonymous("6"))),
	'CHRset_3__36__38__40__42__44__45'(I, J, set(D, K, L), M, N),
	E = true.
'CHRset_3__36__38__40__42__44__45'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__45'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__45'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__45' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__47'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__47'(['CHRincl_set_5'(incl_set(A, B, C, D, E), F, G, H)|I], J, set(D, K, L), M, N) ?-
	'CHRvar'(F),
	coca(try_double(N, set(D, K, L), H, incl_set(A, B, C, D, E), set(O, P, Q), incl_set(R, S, T, O, U), keep_first, (\+ s_included(T, P), s_intersection(T, Q, V), s_union(S, P, W)), (set(R, S, V), set(O, W, Q)), anonymous("7"))),
	no_global_bindings((\+ s_included(C, K), s_intersection(C, L, X), s_union(B, K, Y)), (set(D, K, L), incl_set(A, B, C, D, E))),
	!,
	'CHRkill'(F),
	coca(fired_rule(anonymous("7"))),
	'CHRset_3__36__38__40__42__44__46__47'(I, J, set(D, K, L), M, N),
	set(A, B, X),
	set(D, Y, L).
'CHRset_3__36__38__40__42__44__46__47'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__47'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__47'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__47' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__49'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__49'(['CHRunion_set_5'(union_set(A, B, C, D, E), F, G, H)|I], J, set(A, K, L), M, N) ?-
	'CHRvar'(F),
	coca(try_double(N, set(A, K, L), H, union_set(A, B, C, D, E), set(O, P, Q), union_set(O, R, S, T, U), keep_first, (s_union(S, P, V), s_union(T, Q, W)), set(U, V, W), anonymous("14"))),
	no_global_bindings((s_union(C, K, X), s_union(D, L, Y)), (set(A, K, L), union_set(A, B, C, D, E))),
	!,
	'CHRkill'(F),
	coca(fired_rule(anonymous("14"))),
	'CHRset_3__36__38__40__42__44__46__48__49'(I, J, set(A, K, L), M, N),
	set(E, X, Y).
'CHRset_3__36__38__40__42__44__46__48__49'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__49'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__49'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__49' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__51'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__51'(['CHRunion_S_3'(union_S(A, B, C), D, E, F)|G], H, set(A, I, J), K, L) ?-
	'CHRvar'(D),
	coca(try_double(L, set(A, I, J), F, union_S(A, B, C), set(M, N, O), union_S(M, P, Q), keep_first, true, union_S_S1(P, M, O, Q), anonymous("16"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("16"))),
	'CHRset_3__36__38__40__42__44__46__48__50__51'(G, H, set(A, I, J), K, L),
	union_S_S1(B, A, J, C).
'CHRset_3__36__38__40__42__44__46__48__50__51'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__51'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__51'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__51' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__53'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__53'(['CHRunion_S_S1_4'(union_S_S1(A, B, C, D), E, F, G)|H], I, set(A, J, K), L, M) ?-
	'CHRvar'(E),
	coca(try_double(M, set(A, J, K), G, union_S_S1(A, B, C, D), set(N, O, P), union_S_S1(N, Q, R, S), keep_first, true, (s_delta(S, R, T), incl(T, N), s_delta(S, P, U), incl(U, Q)), anonymous("17"))),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("17"))),
	'CHRset_3__36__38__40__42__44__46__48__50__52__53'(H, I, set(A, J, K), L, M),
	s_delta(D, C, V),
	incl(V, A),
	s_delta(D, K, W),
	incl(W, B).
'CHRset_3__36__38__40__42__44__46__48__50__52__53'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__53'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__53'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__53' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__55'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__55'(['CHRdisjoint_2'(disjoint(A, B), C, D, E)|F], G, set(B, H, I), J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, set(B, H, I), E, disjoint(A, B), set(L, M, N), disjoint(O, L), keep_first, ((is_set(O), s_delta(N, O, P)), 'CHRkeep_heads_checking'(set(L, M, N), Q, set(L, M, P), R)), 'CHRhead_not_kept'(R) -> set(L, M, P) ; true, anonymous("28"))),
	no_global_bindings(((is_set(A), s_delta(I, A, S)), 'CHRkeep_heads_checking'(set(B, H, I), G, set(B, H, S), T)), (set(B, H, I), disjoint(A, B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("28"))),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__55'(F, G, set(B, H, I), J, K),
	(
	    'CHRhead_not_kept'(T)
	->
	    set(B, H, S)
	;
	    true
	).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__55'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__55'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__55'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__55' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__57'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__57'(['CHRdisjoint_2'(disjoint(A, B), C, D, E)|F], G, set(A, H, I), J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, set(A, H, I), E, disjoint(A, B), set(L, M, N), disjoint(L, O), keep_first, ((is_set(O), s_delta(N, O, P)), 'CHRkeep_heads_checking'(set(L, M, N), Q, set(L, M, P), R)), 'CHRhead_not_kept'(R) -> set(L, M, P) ; true, anonymous("29"))),
	no_global_bindings(((is_set(B), s_delta(I, B, S)), 'CHRkeep_heads_checking'(set(A, H, I), G, set(A, H, S), T)), (set(A, H, I), disjoint(A, B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("29"))),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__57'(F, G, set(A, H, I), J, K),
	(
	    'CHRhead_not_kept'(T)
	->
	    set(A, H, S)
	;
	    true
	).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__57'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__57'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__57'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__57' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__59'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__59'(['CHRset_disjoint_4'(set_disjoint(A, B, C, D), E, F, G)|H], I, set(A, J, K), L, M) ?-
	'CHRvar'(E),
	coca(try_double(M, set(A, J, K), G, set_disjoint(A, B, C, D), set(N, O, P), set_disjoint(N, Q, R, S), keep_first, ((s_delta(P, R, T), s_delta(S, O, U)), 'CHRkeep_heads_checking'(set(N, O, P), V, set(N, O, T), W)), (('CHRhead_not_kept'(W) -> set(N, O, T) ; true), set(Q, R, U)), anonymous("31"))),
	no_global_bindings(((s_delta(K, C, X), s_delta(D, J, Y)), 'CHRkeep_heads_checking'(set(A, J, K), I, set(A, J, X), Z)), (set(A, J, K), set_disjoint(A, B, C, D))),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("31"))),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__59'(H, I, set(A, J, K), L, M),
	(
	    'CHRhead_not_kept'(Z)
	->
	    set(A, J, X)
	;
	    true
	),
	set(B, C, Y).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__59'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__59'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__59'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__59' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__61'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__61'(['CHRset_3'(set(A, B, C), D, E, F)|G], H, set(A, I, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(equality), H, D, K, E),
	coca(try_double(L, set(A, I, J), F, set(A, B, C), set(M, N, O), set(M, P, Q), augmentation, ((s_union(N, P, R), s_intersection(O, Q, S)), 'CHRkeep_heads_checking'(set(M, N, O), T, set(M, P, Q), U, set(M, R, S), V)), 'CHRhead_not_kept'(V) -> set(M, R, S) ; true, equality)),
	no_global_bindings(((s_union(I, B, W), s_intersection(J, C, X)), 'CHRkeep_heads_checking'(set(A, I, J), H, set(A, B, C), D, set(A, W, X), Y)), (set(A, I, J), set(A, B, C))),
	!,
	coca(fired_rule(equality)),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__61'(G, H, set(A, I, J), K, L),
	(
	    'CHRhead_not_kept'(Y)
	->
	    set(A, W, X)
	;
	    true
	).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__61'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__61'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__61'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__61' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__63'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__63'(['CHRset_3'(set(A, B, C), D, E, F)|G], H, set(A, I, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('21'(equality), H, D, K, E),
	coca(try_double(L, set(A, I, J), F, set(A, B, C), set(M, N, O), set(M, P, Q), augmentation, ((s_union(P, N, R), s_intersection(Q, O, S)), 'CHRkeep_heads_checking'(set(M, P, Q), T, set(M, N, O), U, set(M, R, S), V)), 'CHRhead_not_kept'(V) -> set(M, R, S) ; true, equality)),
	no_global_bindings(((s_union(B, I, W), s_intersection(C, J, X)), 'CHRkeep_heads_checking'(set(A, B, C), D, set(A, I, J), H, set(A, W, X), Y)), (set(A, I, J), set(A, B, C))),
	!,
	coca(fired_rule(equality)),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__63'(G, H, set(A, I, J), K, L),
	(
	    'CHRhead_not_kept'(Y)
	->
	    set(A, W, X)
	;
	    true
	).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__63'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__63'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__63'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__63' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__65'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__65'(['CHRincl_2'(incl(A, B), C, D, E)|F], G, set(A, H, I), J, K) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(anonymous("5"), G, C, J, D),
	coca(try_double(K, set(A, H, I), E, incl(A, B), set(L, M, N), incl(L, O), augmentation, true, incl_set(L, M, N, O, P), anonymous("5"))),
	!,
	coca(fired_rule(anonymous("5"))),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__65'(F, G, set(A, H, I), J, K),
	incl_set(A, H, I, B, C).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__65'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__65'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__65'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__65' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__67'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__67'(['CHR#_2'(#(A, B), C, D, E)|F], G, set(A, H, I), J, K) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(anonymous("8"), G, C, J, D),
	coca(try_double(K, set(A, H, I), E, #(A, B), set(L, M, N), #(L, O), augmentation, true, (s_card(M, P), P =< O, s_card(N, Q), O =< Q), anonymous("8"))),
	!,
	coca(fired_rule(anonymous("8"))),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__67'(F, G, set(A, H, I), J, K),
	s_card(H, R),
	R =< B,
	s_card(I, S),
	B =< S.
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__67'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__67'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__67'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__67' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__69'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__69'(['CHRset_union_3'(set_union(A, B, C), D, E, F)|G], H, set(A, I, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("13"), H, D, K, E),
	coca(try_double(L, set(A, I, J), F, set_union(A, B, C), set(M, N, O), set_union(M, P, Q), augmentation, true, union_set(P, M, N, O, Q), anonymous("13"))),
	!,
	coca(fired_rule(anonymous("13"))),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__69'(G, H, set(A, I, J), K, L),
	union_set(B, A, I, J, C).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__69'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__69'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__69'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__69' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__71'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__71'(['CHRset_union_3'(set_union(A, B, C), D, E, F)|G], H, set(C, I, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("15"), H, D, K, E),
	coca(try_double(L, set(C, I, J), F, set_union(A, B, C), set(M, N, O), set_union(P, Q, M), augmentation, true, union_S(P, Q, N), anonymous("15"))),
	!,
	coca(fired_rule(anonymous("15"))),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__71'(G, H, set(C, I, J), K, L),
	union_S(A, B, I).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__71'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__71'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__71'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__71' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__73'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__73'(['CHRset_union_3'(set_union(A, B, C), D, E, F)|G], H, set(B, I, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("20"), H, D, K, E),
	coca(try_double(L, set(B, I, J), F, set_union(A, B, C), set(M, N, O), set_union(P, M, Q), augmentation, (is_set(P), s_union(P, N, R), s_union(P, O, S)), set(Q, R, S), anonymous("20"))),
	no_global_bindings((is_set(A), s_union(A, I, T), s_union(A, J, U)), (set(B, I, J), set_union(A, B, C))),
	!,
	coca(fired_rule(anonymous("20"))),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__73'(G, H, set(B, I, J), K, L),
	set(C, T, U).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__73'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__73'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__73'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__73' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__75'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__75'(['CHRset_union_3'(set_union(A, B, C), D, E, F)|G], H, set(A, I, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("21"), H, D, K, E),
	coca(try_double(L, set(A, I, J), F, set_union(A, B, C), set(M, N, O), set_union(M, P, Q), augmentation, (is_set(P), s_union(P, N, R), s_union(P, O, S)), set(Q, R, S), anonymous("21"))),
	no_global_bindings((is_set(B), s_union(B, I, T), s_union(B, J, U)), (set(A, I, J), set_union(A, B, C))),
	!,
	coca(fired_rule(anonymous("21"))),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__75'(G, H, set(A, I, J), K, L),
	set(C, T, U).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__75'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__75'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__75'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__75' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__77'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__77'(['CHRground_union_3'(ground_union(A, B, C), D, E, F)|G], H, set(A, I, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("25"), H, D, K, E),
	coca(try_double(L, set(A, I, J), F, ground_union(A, B, C), set(M, N, O), ground_union(M, P, Q), augmentation, true, (s_delta(Q, O, R), incl(R, P)), anonymous("25"))),
	!,
	coca(fired_rule(anonymous("25"))),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__77'(G, H, set(A, I, J), K, L),
	s_delta(C, J, S),
	incl(S, B).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__77'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__77'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__77'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__77' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__79'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__79'(['CHRground_union_3'(ground_union(A, B, C), D, E, F)|G], H, set(B, I, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("26"), H, D, K, E),
	coca(try_double(L, set(B, I, J), F, ground_union(A, B, C), set(M, N, O), ground_union(P, M, Q), augmentation, true, (s_delta(Q, O, R), incl(R, P)), anonymous("26"))),
	!,
	coca(fired_rule(anonymous("26"))),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__79'(G, H, set(B, I, J), K, L),
	s_delta(C, J, S),
	incl(S, A).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__79'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__79'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__79'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__79' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80'(set(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80__81'(G, D, set(A, B, C), E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80'(set(A, B, C), D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82'(set(A, B, C), D, E, F).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80' / 4, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80__81'(['CHRdisjoint_2'(disjoint(A, B), C, D, E)|F], G, set(A, H, I), J, K) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(anonymous("30"), G, C, J, D),
	coca(try_double(K, set(A, H, I), E, disjoint(A, B), set(L, M, N), disjoint(L, O), augmentation, true, set_disjoint(O, L, M, N), anonymous("30"))),
	!,
	coca(fired_rule(anonymous("30"))),
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80__81'(F, G, set(A, H, I), J, K),
	set_disjoint(B, A, H, I).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80__81'([A|B], C, D, E, F) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80__81'(B, C, D, E, F).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80__81'([], A, B, C, D) :-
	'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82'(B, A, C, D).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80__81' / 5, leash, notrace).
'CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82'(set(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, set(A, B, C)], 'CHRset_3'(set(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRset_3__36__38__40__42__44__46__48__50__52__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82' / 4, leash, notrace).
in(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, in(A, B))),
	'CHRin_2'(in(A, B), D, E, C).



%%% Rules handling for in / 2

'CHRin_2'(in(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRin_2'(in(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRin_2'(in(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRin_2__84'(F, [B], [G, H, I], J),
	coca(try_double(E, in(A, B), J, set(B, I, H), in(K, L), set(L, M, N), keep_second, ((nonvar(K), s_insertion(K, M, O)), 'CHRkeep_heads_checking'(set(L, M, N), P, set(L, O, N), Q)), (s_memberchk(K, N), ('CHRhead_not_kept'(Q) -> set(L, O, N) ; true)), belonging)),
	no_global_bindings(((nonvar(A), s_insertion(A, I, R)), 'CHRkeep_heads_checking'(set(B, I, H), G, set(B, R, H), S)), (in(A, B), set(B, I, H))),
	!,
	'CHRkill'(C),
	coca(fired_rule(belonging)),
	s_memberchk(A, H),
	(
	    'CHRhead_not_kept'(S)
	->
	    set(B, R, H)
	;
	    true
	).
'CHRin_2'(in(A, B), C, D, E) :-
	'CHRin_2__83'(in(A, B), C, D, E).
'CHRin_2__84'(['CHRset_3'(set(A, B, C), D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRin_2__84'([A|B], C, D, E) :-
	'CHRin_2__84'(B, C, D, E).
:- set_flag('CHRin_2__84' / 4, leash, notrace).
:- set_flag('CHRin_2' / 4, leash, notrace).
:- current_macro('CHRin_2' / 4, _27523, _27524, _27525) -> true ; define_macro('CHRin_2' / 4, tr_chr / 2, [write]).
'CHRin_2__83'(A, B, C, D) :-
	'CHRin_2__85'(A, B, C, D).
:- set_flag('CHRin_2__83' / 4, leash, notrace).
'CHRin_2__85'(in(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, in(A, B)], 'CHRin_2'(in(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRin_2__85' / 4, leash, notrace).
notin(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, notin(A, B))),
	'CHRnotin_2'(notin(A, B), D, E, C).



%%% Rules handling for notin / 2

'CHRnotin_2'(notin(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRnotin_2'(notin(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRnotin_2'(notin(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRnotin_2__87'(F, [B], [G, H, I], J),
	coca(try_double(E, notin(A, B), J, set(B, I, H), notin(K, L), set(L, M, N), keep_second, ((nonvar(K), list2set([K], O), s_delta(N, O, P)), 'CHRkeep_heads_checking'(set(L, M, N), Q, set(L, M, P), R)), (\+ s_memberchk(K, M), ('CHRhead_not_kept'(R) -> set(L, M, P) ; true)), not_belonging)),
	no_global_bindings(((nonvar(A), list2set([A], S), s_delta(H, S, T)), 'CHRkeep_heads_checking'(set(B, I, H), G, set(B, I, T), U)), (notin(A, B), set(B, I, H))),
	!,
	'CHRkill'(C),
	coca(fired_rule(not_belonging)),
	\+ s_memberchk(A, I),
	(
	    'CHRhead_not_kept'(U)
	->
	    set(B, I, T)
	;
	    true
	).
'CHRnotin_2'(notin(A, B), C, D, E) :-
	'CHRnotin_2__86'(notin(A, B), C, D, E).
'CHRnotin_2__87'(['CHRset_3'(set(A, B, C), D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRnotin_2__87'([A|B], C, D, E) :-
	'CHRnotin_2__87'(B, C, D, E).
:- set_flag('CHRnotin_2__87' / 4, leash, notrace).
:- set_flag('CHRnotin_2' / 4, leash, notrace).
:- current_macro('CHRnotin_2' / 4, _28743, _28744, _28745) -> true ; define_macro('CHRnotin_2' / 4, tr_chr / 2, [write]).
'CHRnotin_2__86'(A, B, C, D) :-
	'CHRnotin_2__88'(A, B, C, D).
:- set_flag('CHRnotin_2__86' / 4, leash, notrace).
'CHRnotin_2__88'(notin(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, notin(A, B)], 'CHRnotin_2'(notin(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRnotin_2__88' / 4, leash, notrace).
incl(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, incl(A, B))),
	'CHRincl_2'(incl(A, B), D, E, C).



%%% Rules handling for incl / 2

'CHRincl_2'(incl(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRincl_2'(incl(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRincl_2'(incl(A, B), C, D, E) ?-
	coca(try_rule(E, incl(A, B), anonymous("4"), incl(F, G), replacement, (is_set(F), is_set(G)), s_included(F, G))),
	no_global_bindings((is_set(A), is_set(B)), incl(A, B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("4"))),
	s_included(A, B).
'CHRincl_2'(incl(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRincl_2__90'(F, [B], [G, H, I], J),
	coca(try_double(E, incl(A, B), J, set(B, I, H), incl(K, L), set(L, M, N), keep_second, ((is_set(K), s_union(K, M, O)), 'CHRkeep_heads_checking'(set(L, M, N), P, set(L, O, N), Q)), 'CHRhead_not_kept'(Q) -> set(L, O, N) ; true, anonymous("2"))),
	no_global_bindings(((is_set(A), s_union(A, I, R)), 'CHRkeep_heads_checking'(set(B, I, H), G, set(B, R, H), S)), (incl(A, B), set(B, I, H))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("2"))),
	(
	    'CHRhead_not_kept'(S)
	->
	    set(B, R, H)
	;
	    true
	).
'CHRincl_2'(incl(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHRincl_2__91'(F, [A], [G, H, I], J),
	coca(try_double(E, incl(A, B), J, set(A, I, H), incl(K, L), set(K, M, N), keep_second, ((is_set(L), s_intersection(L, N, O)), 'CHRkeep_heads_checking'(set(K, M, N), P, set(K, M, O), Q)), 'CHRhead_not_kept'(Q) -> set(K, M, O) ; true, anonymous("3"))),
	no_global_bindings(((is_set(B), s_intersection(B, H, R)), 'CHRkeep_heads_checking'(set(A, I, H), G, set(A, I, R), S)), (incl(A, B), set(A, I, H))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("3"))),
	(
	    'CHRhead_not_kept'(S)
	->
	    set(A, I, R)
	;
	    true
	).
'CHRincl_2'(incl(A, B), C, D, E) :-
	'CHRincl_2__89'(incl(A, B), C, D, E).
'CHRincl_2__90'(['CHRset_3'(set(A, B, C), D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRincl_2__90'([A|B], C, D, E) :-
	'CHRincl_2__90'(B, C, D, E).
:- set_flag('CHRincl_2__90' / 4, leash, notrace).
'CHRincl_2__91'(['CHRset_3'(set(A, B, C), D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRincl_2__91'([A|B], C, D, E) :-
	'CHRincl_2__91'(B, C, D, E).
:- set_flag('CHRincl_2__91' / 4, leash, notrace).
:- set_flag('CHRincl_2' / 4, leash, notrace).
:- current_macro('CHRincl_2' / 4, _30663, _30664, _30665) -> true ; define_macro('CHRincl_2' / 4, tr_chr / 2, [write]).
'CHRincl_2__89'(A, B, C, D) :-
	'CHRincl_2__92'(A, B, C, D).
:- set_flag('CHRincl_2__89' / 4, leash, notrace).
'CHRincl_2__92'(incl(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHRincl_2__92__93'(F, C, incl(A, B), D, E).
'CHRincl_2__92'(incl(A, B), C, D, E) :-
	'CHRincl_2__92__94'(incl(A, B), C, D, E).
:- set_flag('CHRincl_2__92' / 4, leash, notrace).
'CHRincl_2__92__93'(['CHRset_3'(set(A, B, C), D, E, F)|G], H, incl(A, I), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("5"), H, D, J, E),
	coca(try_double(K, incl(A, I), F, set(A, B, C), incl(L, M), set(L, N, O), augmentation, true, incl_set(L, N, O, M, P), anonymous("5"))),
	!,
	coca(fired_rule(anonymous("5"))),
	'CHRincl_2__92__93'(G, H, incl(A, I), J, K),
	incl_set(A, B, C, I, H).
'CHRincl_2__92__93'([A|B], C, D, E, F) :-
	'CHRincl_2__92__93'(B, C, D, E, F).
'CHRincl_2__92__93'([], A, B, C, D) :-
	'CHRincl_2__92__94'(B, A, C, D).
:- set_flag('CHRincl_2__92__93' / 5, leash, notrace).
'CHRincl_2__92__94'(incl(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, incl(A, B)], 'CHRincl_2'(incl(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRincl_2__92__94' / 4, leash, notrace).
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
'CHR#_2'(#(A, B), C, D, E) ?-
	coca(try_rule(E, #(A, B), anonymous("11"), #(F, G), replacement, is_set(F), s_card(F, G))),
	no_global_bindings(is_set(A), #(A, B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("11"))),
	s_card(A, B).
'CHR#_2'(#(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHR#_2__96'(F, [A], [G], H),
	coca(try_double(E, #(A, B), H, set(A, G, I), #(J, K), set(J, L, M), replacement, s_card(L, K), J = L, anonymous("9"))),
	no_global_bindings(s_card(G, B), (#(A, B), set(A, G, I))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("9"))),
	A = G.
'CHR#_2'(#(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHR#_2__97'(F, [A], [G], H),
	coca(try_double(E, #(A, B), H, set(A, I, G), #(J, K), set(J, L, M), replacement, s_card(M, K), J = M, anonymous("10"))),
	no_global_bindings(s_card(G, B), (#(A, B), set(A, I, G))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("10"))),
	A = G.
'CHR#_2'(#(A, B), C, D, E) :-
	'CHR#_2__95'(#(A, B), C, D, E).
'CHR#_2__96'(['CHRset_3'(set(A, B, C), D, E, F)|G], [A], [H], I) ?-
	'CHRvar'(D),
	'CHRkill'(D),
	'CHR='([B], [H]),
	'CHR='(F, I).
'CHR#_2__96'([A|B], C, D, E) :-
	'CHR#_2__96'(B, C, D, E).
:- set_flag('CHR#_2__96' / 4, leash, notrace).
'CHR#_2__97'(['CHRset_3'(set(A, B, C), D, E, F)|G], [A], [H], I) ?-
	'CHRvar'(D),
	'CHRkill'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHR#_2__97'([A|B], C, D, E) :-
	'CHR#_2__97'(B, C, D, E).
:- set_flag('CHR#_2__97' / 4, leash, notrace).
:- set_flag('CHR#_2' / 4, leash, notrace).
:- current_macro('CHR#_2' / 4, _32996, _32997, _32998) -> true ; define_macro('CHR#_2' / 4, tr_chr / 2, [write]).
'CHR#_2__95'(A, B, C, D) :-
	'CHR#_2__98'(A, B, C, D).
:- set_flag('CHR#_2__95' / 4, leash, notrace).
'CHR#_2__98'(#(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHR#_2__98__99'(F, C, #(A, B), D, E).
'CHR#_2__98'(#(A, B), C, D, E) :-
	'CHR#_2__98__100'(#(A, B), C, D, E).
:- set_flag('CHR#_2__98' / 4, leash, notrace).
'CHR#_2__98__99'(['CHRset_3'(set(A, B, C), D, E, F)|G], H, #(A, I), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("8"), H, D, J, E),
	coca(try_double(K, #(A, I), F, set(A, B, C), #(L, M), set(L, N, O), augmentation, true, (s_card(N, P), P =< M, s_card(O, Q), M =< Q), anonymous("8"))),
	!,
	coca(fired_rule(anonymous("8"))),
	'CHR#_2__98__99'(G, H, #(A, I), J, K),
	s_card(B, R),
	R =< I,
	s_card(C, S),
	I =< S.
'CHR#_2__98__99'([A|B], C, D, E, F) :-
	'CHR#_2__98__99'(B, C, D, E, F).
'CHR#_2__98__99'([], A, B, C, D) :-
	'CHR#_2__98__100'(B, A, C, D).
:- set_flag('CHR#_2__98__99' / 5, leash, notrace).
'CHR#_2__98__100'(#(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, #(A, B)], 'CHR#_2'(#(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHR#_2__98__100' / 4, leash, notrace).
set_union(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, set_union(A, B, C))),
	'CHRset_union_3'(set_union(A, B, C), E, F, D).



%%% Rules handling for set_union / 3

'CHRset_union_3'(set_union(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHRset_union_3'(set_union(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHRset_union_3'(set_union(A, B, C), D, E, F) ?-
	coca(try_rule(F, set_union(A, B, C), anonymous("19"), set_union(G, H, I), replacement, (is_set(G), is_set(H)), s_union(G, H, I))),
	no_global_bindings((is_set(A), is_set(B)), set_union(A, B, C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("19"))),
	s_union(A, B, C).
'CHRset_union_3'(set_union(A, B, C), D, E, F) ?-
	coca(try_rule(F, set_union(A, B, C), anonymous("22"), set_union(G, H, I), replacement, is_set(I), ground_union(G, H, I))),
	no_global_bindings(is_set(C), set_union(A, B, C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("22"))),
	ground_union(A, B, C).
'CHRset_union_3'(set_union(A, B, C), D, E, F) :-
	'CHRset_union_3__101'(set_union(A, B, C), D, E, F).
:- set_flag('CHRset_union_3' / 4, leash, notrace).
:- current_macro('CHRset_union_3' / 4, _34584, _34585, _34586) -> true ; define_macro('CHRset_union_3' / 4, tr_chr / 2, [write]).
'CHRset_union_3__101'(set_union(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('CHRset_union_3__101', E),
	coca(try_rule(F, set_union(A, B, C), anonymous("12"), set_union(G, H, I), augmentation, true, (incl(G, I), incl(H, I)))),
	!,
	'CHRset_union_3__101__103'(set_union(A, B, C), D, E, F),
	coca(fired_rule(anonymous("12"))),
	incl(A, C),
	incl(B, C).
'CHRset_union_3__101'(A, B, C, D) ?-
	'CHRset_union_3__101__103'(A, B, C, D).
:- set_flag('CHRset_union_3__101' / 4, leash, notrace).
'CHRset_union_3__101__103'(A, B, C, D) :-
	'CHRset_union_3__102'(A, B, C, D).
:- set_flag('CHRset_union_3__101__103' / 4, leash, notrace).
'CHRset_union_3__102'(set_union(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_union_3__102__104'(G, D, set_union(A, B, C), E, F).
'CHRset_union_3__102'(set_union(A, B, C), D, E, F) :-
	'CHRset_union_3__102__105'(set_union(A, B, C), D, E, F).
:- set_flag('CHRset_union_3__102' / 4, leash, notrace).
'CHRset_union_3__102__104'(['CHRset_3'(set(A, B, C), D, E, F)|G], H, set_union(A, I, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("13"), H, D, K, E),
	coca(try_double(L, set_union(A, I, J), F, set(A, B, C), set_union(M, N, O), set(M, P, Q), augmentation, true, union_set(N, M, P, Q, O), anonymous("13"))),
	!,
	coca(fired_rule(anonymous("13"))),
	'CHRset_union_3__102__104'(G, H, set_union(A, I, J), K, L),
	union_set(I, A, B, C, J).
'CHRset_union_3__102__104'([A|B], C, D, E, F) :-
	'CHRset_union_3__102__104'(B, C, D, E, F).
'CHRset_union_3__102__104'([], A, B, C, D) :-
	'CHRset_union_3__102__105'(B, A, C, D).
:- set_flag('CHRset_union_3__102__104' / 5, leash, notrace).
'CHRset_union_3__102__105'(set_union(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(C, G),
	'CHRset_union_3__102__105__106'(G, D, set_union(A, B, C), E, F).
'CHRset_union_3__102__105'(set_union(A, B, C), D, E, F) :-
	'CHRset_union_3__102__105__107'(set_union(A, B, C), D, E, F).
:- set_flag('CHRset_union_3__102__105' / 4, leash, notrace).
'CHRset_union_3__102__105__106'(['CHRset_3'(set(A, B, C), D, E, F)|G], H, set_union(I, J, A), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("15"), H, D, K, E),
	coca(try_double(L, set_union(I, J, A), F, set(A, B, C), set_union(M, N, O), set(O, P, Q), augmentation, true, union_S(M, N, P), anonymous("15"))),
	!,
	coca(fired_rule(anonymous("15"))),
	'CHRset_union_3__102__105__106'(G, H, set_union(I, J, A), K, L),
	union_S(I, J, B).
'CHRset_union_3__102__105__106'([A|B], C, D, E, F) :-
	'CHRset_union_3__102__105__106'(B, C, D, E, F).
'CHRset_union_3__102__105__106'([], A, B, C, D) :-
	'CHRset_union_3__102__105__107'(B, A, C, D).
:- set_flag('CHRset_union_3__102__105__106' / 5, leash, notrace).
'CHRset_union_3__102__105__107'(set_union(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRset_union_3__102__105__107__108'(G, D, set_union(A, B, C), E, F).
'CHRset_union_3__102__105__107'(set_union(A, B, C), D, E, F) :-
	'CHRset_union_3__102__105__107__109'(set_union(A, B, C), D, E, F).
:- set_flag('CHRset_union_3__102__105__107' / 4, leash, notrace).
'CHRset_union_3__102__105__107__108'(['CHRset_3'(set(A, B, C), D, E, F)|G], H, set_union(I, A, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("20"), H, D, K, E),
	coca(try_double(L, set_union(I, A, J), F, set(A, B, C), set_union(M, N, O), set(N, P, Q), augmentation, (is_set(M), s_union(M, P, R), s_union(M, Q, S)), set(O, R, S), anonymous("20"))),
	no_global_bindings((is_set(I), s_union(I, B, T), s_union(I, C, U)), (set_union(I, A, J), set(A, B, C))),
	!,
	coca(fired_rule(anonymous("20"))),
	'CHRset_union_3__102__105__107__108'(G, H, set_union(I, A, J), K, L),
	set(J, T, U).
'CHRset_union_3__102__105__107__108'([A|B], C, D, E, F) :-
	'CHRset_union_3__102__105__107__108'(B, C, D, E, F).
'CHRset_union_3__102__105__107__108'([], A, B, C, D) :-
	'CHRset_union_3__102__105__107__109'(B, A, C, D).
:- set_flag('CHRset_union_3__102__105__107__108' / 5, leash, notrace).
'CHRset_union_3__102__105__107__109'(set_union(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRset_union_3__102__105__107__109__110'(G, D, set_union(A, B, C), E, F).
'CHRset_union_3__102__105__107__109'(set_union(A, B, C), D, E, F) :-
	'CHRset_union_3__102__105__107__109__111'(set_union(A, B, C), D, E, F).
:- set_flag('CHRset_union_3__102__105__107__109' / 4, leash, notrace).
'CHRset_union_3__102__105__107__109__110'(['CHRset_3'(set(A, B, C), D, E, F)|G], H, set_union(A, I, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("21"), H, D, K, E),
	coca(try_double(L, set_union(A, I, J), F, set(A, B, C), set_union(M, N, O), set(M, P, Q), augmentation, (is_set(N), s_union(N, P, R), s_union(N, Q, S)), set(O, R, S), anonymous("21"))),
	no_global_bindings((is_set(I), s_union(I, B, T), s_union(I, C, U)), (set_union(A, I, J), set(A, B, C))),
	!,
	coca(fired_rule(anonymous("21"))),
	'CHRset_union_3__102__105__107__109__110'(G, H, set_union(A, I, J), K, L),
	set(J, T, U).
'CHRset_union_3__102__105__107__109__110'([A|B], C, D, E, F) :-
	'CHRset_union_3__102__105__107__109__110'(B, C, D, E, F).
'CHRset_union_3__102__105__107__109__110'([], A, B, C, D) :-
	'CHRset_union_3__102__105__107__109__111'(B, A, C, D).
:- set_flag('CHRset_union_3__102__105__107__109__110' / 5, leash, notrace).
'CHRset_union_3__102__105__107__109__111'(set_union(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, set_union(A, B, C)], 'CHRset_union_3'(set_union(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRset_union_3__102__105__107__109__111' / 4, leash, notrace).
disjoint(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, disjoint(A, B))),
	'CHRdisjoint_2'(disjoint(A, B), D, E, C).



%%% Rules handling for disjoint / 2

'CHRdisjoint_2'(disjoint(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRdisjoint_2'(disjoint(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRdisjoint_2'(disjoint(A, B), C, D, E) ?-
	coca(try_rule(E, disjoint(A, B), anonymous("27"), disjoint(F, G), replacement, (is_set(F), is_set(G)), s_intersection(F, G, {}))),
	no_global_bindings((is_set(A), is_set(B)), disjoint(A, B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("27"))),
	s_intersection(A, B, {}).
'CHRdisjoint_2'(disjoint(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRdisjoint_2__113'(F, [B], [G, H, I], J),
	coca(try_double(E, disjoint(A, B), J, set(B, I, H), disjoint(K, L), set(L, M, N), keep_second, ((is_set(K), s_delta(N, K, O)), 'CHRkeep_heads_checking'(set(L, M, N), P, set(L, M, O), Q)), 'CHRhead_not_kept'(Q) -> set(L, M, O) ; true, anonymous("28"))),
	no_global_bindings(((is_set(A), s_delta(H, A, R)), 'CHRkeep_heads_checking'(set(B, I, H), G, set(B, I, R), S)), (disjoint(A, B), set(B, I, H))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("28"))),
	(
	    'CHRhead_not_kept'(S)
	->
	    set(B, I, R)
	;
	    true
	).
'CHRdisjoint_2'(disjoint(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHRdisjoint_2__114'(F, [A], [G, H, I], J),
	coca(try_double(E, disjoint(A, B), J, set(A, I, H), disjoint(K, L), set(K, M, N), keep_second, ((is_set(L), s_delta(N, L, O)), 'CHRkeep_heads_checking'(set(K, M, N), P, set(K, M, O), Q)), 'CHRhead_not_kept'(Q) -> set(K, M, O) ; true, anonymous("29"))),
	no_global_bindings(((is_set(B), s_delta(H, B, R)), 'CHRkeep_heads_checking'(set(A, I, H), G, set(A, I, R), S)), (disjoint(A, B), set(A, I, H))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("29"))),
	(
	    'CHRhead_not_kept'(S)
	->
	    set(A, I, R)
	;
	    true
	).
'CHRdisjoint_2'(disjoint(A, B), C, D, E) :-
	'CHRdisjoint_2__112'(disjoint(A, B), C, D, E).
'CHRdisjoint_2__113'(['CHRset_3'(set(A, B, C), D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRdisjoint_2__113'([A|B], C, D, E) :-
	'CHRdisjoint_2__113'(B, C, D, E).
:- set_flag('CHRdisjoint_2__113' / 4, leash, notrace).
'CHRdisjoint_2__114'(['CHRset_3'(set(A, B, C), D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRdisjoint_2__114'([A|B], C, D, E) :-
	'CHRdisjoint_2__114'(B, C, D, E).
:- set_flag('CHRdisjoint_2__114' / 4, leash, notrace).
:- set_flag('CHRdisjoint_2' / 4, leash, notrace).
:- current_macro('CHRdisjoint_2' / 4, _39330, _39331, _39332) -> true ; define_macro('CHRdisjoint_2' / 4, tr_chr / 2, [write]).
'CHRdisjoint_2__112'(A, B, C, D) :-
	'CHRdisjoint_2__115'(A, B, C, D).
:- set_flag('CHRdisjoint_2__112' / 4, leash, notrace).
'CHRdisjoint_2__115'(disjoint(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHRdisjoint_2__115__116'(F, C, disjoint(A, B), D, E).
'CHRdisjoint_2__115'(disjoint(A, B), C, D, E) :-
	'CHRdisjoint_2__115__117'(disjoint(A, B), C, D, E).
:- set_flag('CHRdisjoint_2__115' / 4, leash, notrace).
'CHRdisjoint_2__115__116'(['CHRset_3'(set(A, B, C), D, E, F)|G], H, disjoint(A, I), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("30"), H, D, J, E),
	coca(try_double(K, disjoint(A, I), F, set(A, B, C), disjoint(L, M), set(L, N, O), augmentation, true, set_disjoint(M, L, N, O), anonymous("30"))),
	!,
	coca(fired_rule(anonymous("30"))),
	'CHRdisjoint_2__115__116'(G, H, disjoint(A, I), J, K),
	set_disjoint(I, A, B, C).
'CHRdisjoint_2__115__116'([A|B], C, D, E, F) :-
	'CHRdisjoint_2__115__116'(B, C, D, E, F).
'CHRdisjoint_2__115__116'([], A, B, C, D) :-
	'CHRdisjoint_2__115__117'(B, A, C, D).
:- set_flag('CHRdisjoint_2__115__116' / 5, leash, notrace).
'CHRdisjoint_2__115__117'(disjoint(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, disjoint(A, B)], 'CHRdisjoint_2'(disjoint(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRdisjoint_2__115__117' / 4, leash, notrace).
incl_set(A, B, C, D, E) :-
	'CHRgen_num'(F),
	coca(add_one_constraint(F, incl_set(A, B, C, D, E))),
	'CHRincl_set_5'(incl_set(A, B, C, D, E), G, H, F).



%%% Rules handling for incl_set / 5

'CHRincl_set_5'(incl_set(A, B, C, D, E), F, G, H) :-
	(
	    'CHRnonvar'(F)
	;
	    'CHRalready_in'('CHRincl_set_5'(incl_set(A, B, C, D, E), F, G, H)),
	    coca(already_in)
	),
	!.
'CHRincl_set_5'(incl_set(A, B, C, D, E), F, G, H) ?-
	'CHRget_delayed_goals'(D, I),
	'CHRincl_set_5__119'(I, [D], [J], K),
	coca(try_double(H, incl_set(A, B, C, D, E), K, set(D, J, L), incl_set(M, N, O, P, Q), set(P, R, S), keep_second, s_included(O, R), Q = true, anonymous("6"))),
	no_global_bindings(s_included(C, J), (incl_set(A, B, C, D, E), set(D, J, L))),
	!,
	'CHRkill'(F),
	coca(fired_rule(anonymous("6"))),
	E = true.
'CHRincl_set_5'(incl_set(A, B, C, D, E), F, G, H) ?-
	'CHRget_delayed_goals'(D, I),
	'CHRincl_set_5__120'(I, [D], [J, K], L),
	coca(try_double(H, incl_set(A, B, C, D, E), L, set(D, K, J), incl_set(M, N, O, P, Q), set(P, R, S), keep_second, (\+ s_included(O, R), s_intersection(O, S, T), s_union(N, R, U)), (set(M, N, T), set(P, U, S)), anonymous("7"))),
	no_global_bindings((\+ s_included(C, K), s_intersection(C, J, V), s_union(B, K, W)), (incl_set(A, B, C, D, E), set(D, K, J))),
	!,
	'CHRkill'(F),
	coca(fired_rule(anonymous("7"))),
	set(A, B, V),
	set(D, W, J).
'CHRincl_set_5'(incl_set(A, B, C, D, E), F, G, H) :-
	'CHRincl_set_5__118'(incl_set(A, B, C, D, E), F, G, H).
'CHRincl_set_5__119'(['CHRset_3'(set(A, B, C), D, E, F)|G], [A], [H], I) ?-
	'CHRvar'(D),
	'CHR='([B], [H]),
	'CHR='(F, I).
'CHRincl_set_5__119'([A|B], C, D, E) :-
	'CHRincl_set_5__119'(B, C, D, E).
:- set_flag('CHRincl_set_5__119' / 4, leash, notrace).
'CHRincl_set_5__120'(['CHRset_3'(set(A, B, C), D, E, F)|G], [A], [H, I], J) ?-
	'CHRvar'(D),
	'CHR='([C, B], [H, I]),
	'CHR='(F, J).
'CHRincl_set_5__120'([A|B], C, D, E) :-
	'CHRincl_set_5__120'(B, C, D, E).
:- set_flag('CHRincl_set_5__120' / 4, leash, notrace).
:- set_flag('CHRincl_set_5' / 4, leash, notrace).
:- current_macro('CHRincl_set_5' / 4, _41626, _41627, _41628) -> true ; define_macro('CHRincl_set_5' / 4, tr_chr / 2, [write]).
'CHRincl_set_5__118'(A, B, C, D) :-
	'CHRincl_set_5__121'(A, B, C, D).
:- set_flag('CHRincl_set_5__118' / 4, leash, notrace).
'CHRincl_set_5__121'(incl_set(A, B, C, D, E), F, G, H) :-
	(
	    'CHRvar'(F)
	->
	    'CHRdelay'([F, incl_set(A, B, C, D, E)], 'CHRincl_set_5'(incl_set(A, B, C, D, E), F, G, H))
	;
	    true
	).
:- set_flag('CHRincl_set_5__121' / 4, leash, notrace).
set_set(A, B, C, D, E, F) :-
	'CHRgen_num'(G),
	coca(add_one_constraint(G, set_set(A, B, C, D, E, F))),
	'CHRset_set_6'(set_set(A, B, C, D, E, F), H, I, G).



%%% Rules handling for set_set / 6

'CHRset_set_6'(set_set(A, B, C, D, E, F), G, H, I) :-
	(
	    'CHRnonvar'(G)
	;
	    'CHRalready_in'('CHRset_set_6'(set_set(A, B, C, D, E, F), G, H, I)),
	    coca(already_in)
	),
	!.
'CHRset_set_6'(set_set(A, B, C, D, E, F), G, H, I) :-
	'CHRset_set_6__122'(set_set(A, B, C, D, E, F), G, H, I).
:- set_flag('CHRset_set_6' / 4, leash, notrace).
:- current_macro('CHRset_set_6' / 4, _42265, _42266, _42267) -> true ; define_macro('CHRset_set_6' / 4, tr_chr / 2, [write]).
'CHRset_set_6__122'(A, B, C, D) :-
	'CHRset_set_6__123'(A, B, C, D).
:- set_flag('CHRset_set_6__122' / 4, leash, notrace).
'CHRset_set_6__123'(set_set(A, B, C, D, E, F), G, H, I) :-
	(
	    'CHRvar'(G)
	->
	    'CHRdelay'([G, set_set(A, B, C, D, E, F)], 'CHRset_set_6'(set_set(A, B, C, D, E, F), G, H, I))
	;
	    true
	).
:- set_flag('CHRset_set_6__123' / 4, leash, notrace).
union_set(A, B, C, D, E) :-
	'CHRgen_num'(F),
	coca(add_one_constraint(F, union_set(A, B, C, D, E))),
	'CHRunion_set_5'(union_set(A, B, C, D, E), G, H, F).



%%% Rules handling for union_set / 5

'CHRunion_set_5'(union_set(A, B, C, D, E), F, G, H) :-
	(
	    'CHRnonvar'(F)
	;
	    'CHRalready_in'('CHRunion_set_5'(union_set(A, B, C, D, E), F, G, H)),
	    coca(already_in)
	),
	!.
'CHRunion_set_5'(union_set(A, B, C, D, E), F, G, H) ?-
	'CHRget_delayed_goals'(A, I),
	'CHRunion_set_5__125'(I, [A], [J, K], L),
	coca(try_double(H, union_set(A, B, C, D, E), L, set(A, K, J), union_set(M, N, O, P, Q), set(M, R, S), keep_second, (s_union(O, R, T), s_union(P, S, U)), set(Q, T, U), anonymous("14"))),
	no_global_bindings((s_union(C, K, V), s_union(D, J, W)), (union_set(A, B, C, D, E), set(A, K, J))),
	!,
	'CHRkill'(F),
	coca(fired_rule(anonymous("14"))),
	set(E, V, W).
'CHRunion_set_5'(union_set(A, B, C, D, E), F, G, H) :-
	'CHRunion_set_5__124'(union_set(A, B, C, D, E), F, G, H).
'CHRunion_set_5__125'(['CHRset_3'(set(A, B, C), D, E, F)|G], [A], [H, I], J) ?-
	'CHRvar'(D),
	'CHR='([C, B], [H, I]),
	'CHR='(F, J).
'CHRunion_set_5__125'([A|B], C, D, E) :-
	'CHRunion_set_5__125'(B, C, D, E).
:- set_flag('CHRunion_set_5__125' / 4, leash, notrace).
:- set_flag('CHRunion_set_5' / 4, leash, notrace).
:- current_macro('CHRunion_set_5' / 4, _43432, _43433, _43434) -> true ; define_macro('CHRunion_set_5' / 4, tr_chr / 2, [write]).
'CHRunion_set_5__124'(A, B, C, D) :-
	'CHRunion_set_5__126'(A, B, C, D).
:- set_flag('CHRunion_set_5__124' / 4, leash, notrace).
'CHRunion_set_5__126'(union_set(A, B, C, D, E), F, G, H) :-
	(
	    'CHRvar'(F)
	->
	    'CHRdelay'([F, union_set(A, B, C, D, E)], 'CHRunion_set_5'(union_set(A, B, C, D, E), F, G, H))
	;
	    true
	).
:- set_flag('CHRunion_set_5__126' / 4, leash, notrace).
set_disjoint(A, B, C, D) :-
	'CHRgen_num'(E),
	coca(add_one_constraint(E, set_disjoint(A, B, C, D))),
	'CHRset_disjoint_4'(set_disjoint(A, B, C, D), F, G, E).



%%% Rules handling for set_disjoint / 4

'CHRset_disjoint_4'(set_disjoint(A, B, C, D), E, F, G) :-
	(
	    'CHRnonvar'(E)
	;
	    'CHRalready_in'('CHRset_disjoint_4'(set_disjoint(A, B, C, D), E, F, G)),
	    coca(already_in)
	),
	!.
'CHRset_disjoint_4'(set_disjoint(A, B, C, D), E, F, G) ?-
	coca(try_rule(G, set_disjoint(A, B, C, D), anonymous("32"), set_disjoint(H, I, J, K), replacement, is_set(H) ; is_set(I), true)),
	no_global_bindings(is_set(A) ; is_set(B), set_disjoint(A, B, C, D)),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("32"))).
'CHRset_disjoint_4'(set_disjoint(A, B, C, D), E, F, G) ?-
	'CHRget_delayed_goals'(A, H),
	'CHRset_disjoint_4__128'(H, [A], [I, J, K], L),
	coca(try_double(G, set_disjoint(A, B, C, D), L, set(A, K, J), set_disjoint(M, N, O, P), set(M, Q, R), keep_second, ((s_delta(R, O, S), s_delta(P, Q, T)), 'CHRkeep_heads_checking'(set(M, Q, R), U, set(M, Q, S), V)), (('CHRhead_not_kept'(V) -> set(M, Q, S) ; true), set(N, O, T)), anonymous("31"))),
	no_global_bindings(((s_delta(J, C, W), s_delta(D, K, X)), 'CHRkeep_heads_checking'(set(A, K, J), I, set(A, K, W), Y)), (set_disjoint(A, B, C, D), set(A, K, J))),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("31"))),
	(
	    'CHRhead_not_kept'(Y)
	->
	    set(A, K, W)
	;
	    true
	),
	set(B, C, X).
'CHRset_disjoint_4'(set_disjoint(A, B, C, D), E, F, G) :-
	'CHRset_disjoint_4__127'(set_disjoint(A, B, C, D), E, F, G).
'CHRset_disjoint_4__128'(['CHRset_3'(set(A, B, C), D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRset_disjoint_4__128'([A|B], C, D, E) :-
	'CHRset_disjoint_4__128'(B, C, D, E).
:- set_flag('CHRset_disjoint_4__128' / 4, leash, notrace).
:- set_flag('CHRset_disjoint_4' / 4, leash, notrace).
:- current_macro('CHRset_disjoint_4' / 4, _44839, _44840, _44841) -> true ; define_macro('CHRset_disjoint_4' / 4, tr_chr / 2, [write]).
'CHRset_disjoint_4__127'(A, B, C, D) :-
	'CHRset_disjoint_4__129'(A, B, C, D).
:- set_flag('CHRset_disjoint_4__127' / 4, leash, notrace).
'CHRset_disjoint_4__129'(set_disjoint(A, B, C, D), E, F, G) :-
	(
	    'CHRvar'(E)
	->
	    'CHRdelay'([E, set_disjoint(A, B, C, D)], 'CHRset_disjoint_4'(set_disjoint(A, B, C, D), E, F, G))
	;
	    true
	).
:- set_flag('CHRset_disjoint_4__129' / 4, leash, notrace).
ground_union(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, ground_union(A, B, C))),
	'CHRground_union_3'(ground_union(A, B, C), E, F, D).



%%% Rules handling for ground_union / 3

'CHRground_union_3'(ground_union(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHRground_union_3'(ground_union(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHRground_union_3'(ground_union(A, B, C), D, E, F) ?-
	coca(try_rule(F, ground_union(A, B, C), anonymous("23"), ground_union(G, H, I), replacement, is_set(G), (s_delta(I, G, J), incl(J, H)))),
	no_global_bindings(is_set(A), ground_union(A, B, C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("23"))),
	s_delta(C, A, K),
	incl(K, B).
'CHRground_union_3'(ground_union(A, B, C), D, E, F) ?-
	coca(try_rule(F, ground_union(A, B, C), anonymous("24"), ground_union(G, H, I), replacement, is_set(H), (s_delta(I, H, J), incl(J, G)))),
	no_global_bindings(is_set(B), ground_union(A, B, C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("24"))),
	s_delta(C, B, K),
	incl(K, A).
'CHRground_union_3'(ground_union(A, B, C), D, E, F) :-
	'CHRground_union_3__130'(ground_union(A, B, C), D, E, F).
:- set_flag('CHRground_union_3' / 4, leash, notrace).
:- current_macro('CHRground_union_3' / 4, _45815, _45816, _45817) -> true ; define_macro('CHRground_union_3' / 4, tr_chr / 2, [write]).
'CHRground_union_3__130'(A, B, C, D) :-
	'CHRground_union_3__131'(A, B, C, D).
:- set_flag('CHRground_union_3__130' / 4, leash, notrace).
'CHRground_union_3__131'(ground_union(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRground_union_3__131__132'(G, D, ground_union(A, B, C), E, F).
'CHRground_union_3__131'(ground_union(A, B, C), D, E, F) :-
	'CHRground_union_3__131__133'(ground_union(A, B, C), D, E, F).
:- set_flag('CHRground_union_3__131' / 4, leash, notrace).
'CHRground_union_3__131__132'(['CHRset_3'(set(A, B, C), D, E, F)|G], H, ground_union(A, I, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("25"), H, D, K, E),
	coca(try_double(L, ground_union(A, I, J), F, set(A, B, C), ground_union(M, N, O), set(M, P, Q), augmentation, true, (s_delta(O, Q, R), incl(R, N)), anonymous("25"))),
	!,
	coca(fired_rule(anonymous("25"))),
	'CHRground_union_3__131__132'(G, H, ground_union(A, I, J), K, L),
	s_delta(J, C, S),
	incl(S, I).
'CHRground_union_3__131__132'([A|B], C, D, E, F) :-
	'CHRground_union_3__131__132'(B, C, D, E, F).
'CHRground_union_3__131__132'([], A, B, C, D) :-
	'CHRground_union_3__131__133'(B, A, C, D).
:- set_flag('CHRground_union_3__131__132' / 5, leash, notrace).
'CHRground_union_3__131__133'(ground_union(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRground_union_3__131__133__134'(G, D, ground_union(A, B, C), E, F).
'CHRground_union_3__131__133'(ground_union(A, B, C), D, E, F) :-
	'CHRground_union_3__131__133__135'(ground_union(A, B, C), D, E, F).
:- set_flag('CHRground_union_3__131__133' / 4, leash, notrace).
'CHRground_union_3__131__133__134'(['CHRset_3'(set(A, B, C), D, E, F)|G], H, ground_union(I, A, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("26"), H, D, K, E),
	coca(try_double(L, ground_union(I, A, J), F, set(A, B, C), ground_union(M, N, O), set(N, P, Q), augmentation, true, (s_delta(O, Q, R), incl(R, M)), anonymous("26"))),
	!,
	coca(fired_rule(anonymous("26"))),
	'CHRground_union_3__131__133__134'(G, H, ground_union(I, A, J), K, L),
	s_delta(J, C, S),
	incl(S, I).
'CHRground_union_3__131__133__134'([A|B], C, D, E, F) :-
	'CHRground_union_3__131__133__134'(B, C, D, E, F).
'CHRground_union_3__131__133__134'([], A, B, C, D) :-
	'CHRground_union_3__131__133__135'(B, A, C, D).
:- set_flag('CHRground_union_3__131__133__134' / 5, leash, notrace).
'CHRground_union_3__131__133__135'(ground_union(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, ground_union(A, B, C)], 'CHRground_union_3'(ground_union(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRground_union_3__131__133__135' / 4, leash, notrace).
union_S(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, union_S(A, B, C))),
	'CHRunion_S_3'(union_S(A, B, C), E, F, D).



%%% Rules handling for union_S / 3

'CHRunion_S_3'(union_S(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHRunion_S_3'(union_S(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHRunion_S_3'(union_S(A, B, C), D, E, F) ?-
	coca(try_rule(F, union_S(A, B, C), anonymous("18"), union_S(G, H, I), replacement, (nonvar(G), nonvar(H)), true)),
	no_global_bindings((nonvar(A), nonvar(B)), union_S(A, B, C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("18"))).
'CHRunion_S_3'(union_S(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(A, G),
	'CHRunion_S_3__137'(G, [A], [H], I),
	coca(try_double(F, union_S(A, B, C), I, set(A, J, H), union_S(K, L, M), set(K, N, O), keep_second, true, union_S_S1(L, K, O, M), anonymous("16"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("16"))),
	union_S_S1(B, A, H, C).
'CHRunion_S_3'(union_S(A, B, C), D, E, F) :-
	'CHRunion_S_3__136'(union_S(A, B, C), D, E, F).
'CHRunion_S_3__137'(['CHRset_3'(set(A, B, C), D, E, F)|G], [A], [H], I) ?-
	'CHRvar'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHRunion_S_3__137'([A|B], C, D, E) :-
	'CHRunion_S_3__137'(B, C, D, E).
:- set_flag('CHRunion_S_3__137' / 4, leash, notrace).
:- set_flag('CHRunion_S_3' / 4, leash, notrace).
:- current_macro('CHRunion_S_3' / 4, _48338, _48339, _48340) -> true ; define_macro('CHRunion_S_3' / 4, tr_chr / 2, [write]).
'CHRunion_S_3__136'(A, B, C, D) :-
	'CHRunion_S_3__138'(A, B, C, D).
:- set_flag('CHRunion_S_3__136' / 4, leash, notrace).
'CHRunion_S_3__138'(union_S(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, union_S(A, B, C)], 'CHRunion_S_3'(union_S(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRunion_S_3__138' / 4, leash, notrace).
union_S_S1(A, B, C, D) :-
	'CHRgen_num'(E),
	coca(add_one_constraint(E, union_S_S1(A, B, C, D))),
	'CHRunion_S_S1_4'(union_S_S1(A, B, C, D), F, G, E).



%%% Rules handling for union_S_S1 / 4

'CHRunion_S_S1_4'(union_S_S1(A, B, C, D), E, F, G) :-
	(
	    'CHRnonvar'(E)
	;
	    'CHRalready_in'('CHRunion_S_S1_4'(union_S_S1(A, B, C, D), E, F, G)),
	    coca(already_in)
	),
	!.
'CHRunion_S_S1_4'(union_S_S1(A, B, C, D), E, F, G) ?-
	'CHRget_delayed_goals'(A, H),
	'CHRunion_S_S1_4__140'(H, [A], [I], J),
	coca(try_double(G, union_S_S1(A, B, C, D), J, set(A, K, I), union_S_S1(L, M, N, O), set(L, P, Q), keep_second, true, (s_delta(O, N, R), incl(R, L), s_delta(O, Q, S), incl(S, M)), anonymous("17"))),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("17"))),
	s_delta(D, C, T),
	incl(T, A),
	s_delta(D, I, U),
	incl(U, B).
'CHRunion_S_S1_4'(union_S_S1(A, B, C, D), E, F, G) :-
	'CHRunion_S_S1_4__139'(union_S_S1(A, B, C, D), E, F, G).
'CHRunion_S_S1_4__140'(['CHRset_3'(set(A, B, C), D, E, F)|G], [A], [H], I) ?-
	'CHRvar'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHRunion_S_S1_4__140'([A|B], C, D, E) :-
	'CHRunion_S_S1_4__140'(B, C, D, E).
:- set_flag('CHRunion_S_S1_4__140' / 4, leash, notrace).
:- set_flag('CHRunion_S_S1_4' / 4, leash, notrace).
:- current_macro('CHRunion_S_S1_4' / 4, _49517, _49518, _49519) -> true ; define_macro('CHRunion_S_S1_4' / 4, tr_chr / 2, [write]).
'CHRunion_S_S1_4__139'(A, B, C, D) :-
	'CHRunion_S_S1_4__141'(A, B, C, D).
:- set_flag('CHRunion_S_S1_4__139' / 4, leash, notrace).
'CHRunion_S_S1_4__141'(union_S_S1(A, B, C, D), E, F, G) :-
	(
	    'CHRvar'(E)
	->
	    'CHRdelay'([E, union_S_S1(A, B, C, D)], 'CHRunion_S_S1_4'(union_S_S1(A, B, C, D), E, F, G))
	;
	    true
	).
:- set_flag('CHRunion_S_S1_4__141' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
