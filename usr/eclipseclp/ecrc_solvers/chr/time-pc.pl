
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
globalize(_819) :- suspend(true, 2, _819 - _834 -> inst), _834 = fired.
path1(1, _1143, _1146, [_1151], p - p, _1159) :- check_pp(_1143, _1146, _1151).
path1(1, _1190, _1193, [_1198], p - i, _1206) :- check_pi(_1190, _1193, _1198).
path1(1, _1237, _1240, [_1245], i - p, _1253) :- check_ip(_1240, _1237, _1245).
path1(1, _1284, _1287, [_1292], i - i, _1300) :- check_ii(_1284, _1287, _1292).



%%% Callables for path / 6

'CHRlabel_with'(path(A, B, C, D, E, F), G, H) ?-
	coca(try_clause(H, path(A, B, C, D, E, F), path(I, J, K, L, M, N), I > 1)),
	no_delayed_goals(A > 1),
	coca(clause_fired(H)),
	'CHR='(G, clause_path(A, B, C, D, E, F)).
arc(A, B, C, D) :-
	'CHRgen_num'(E),
	coca(add_one_constraint(E, arc(A, B, C, D))),
	'CHRarc_4'(arc(A, B, C, D), F, G, E).



%%% Rules handling for arc / 4

'CHRarc_4'(arc(A, B, C, D), E, F, G) :-
	'CHRnonvar'(E),
	!.
'CHRarc_4'(arc(A, B, C, D), E, F, G) ?-
	coca(try_rule(G, arc(A, B, C, D), add_path, arc(H, I, J, K), replacement, (ground(J), ground(K), length(J, L)), (globalize(H - I), path(L, H, I, J, K, 1)))),
	no_delayed_goals((ground(C), ground(D), length(C, M))),
	!,
	'CHRkill'(E),
	coca(fired_rule(add_path)),
	globalize(A - B),
	path(M, A, B, C, D, 1).
'CHRarc_4'(arc(A, B, C, D), E, F, G) :-
	'CHRarc_4__0'(arc(A, B, C, D), E, F, G).
:- set_flag('CHRarc_4' / 4, leash, notrace).
:- current_macro('CHRarc_4' / 4, _5867, _5868, _5869) -> true ; define_macro('CHRarc_4' / 4, tr_chr / 2, [write]).
'CHRarc_4__0'(A, B, C, D) :-
	'CHRarc_4__1'(A, B, C, D).
:- set_flag('CHRarc_4__0' / 4, leash, notrace).
'CHRarc_4__1'(arc(A, B, C, D), E, F, G) :-
	(
	    'CHRvar'(E)
	->
	    'CHRdelay'([E, arc(A, B, C, D)], 'CHRarc_4'(arc(A, B, C, D), E, F, G))
	;
	    true
	).
:- set_flag('CHRarc_4__1' / 4, leash, notrace).



%%% Prolog clauses for path / 6

clause_path(A, B, C, D, E, F) :-
	member(G, D),
	path(1, B, C, [G], E, F).
:- current_macro(clause_path / 6, _6139, _6140, _6141) -> true ; define_macro(clause_path / 6, tr_chr / 2, [write]).
path(A, B, C, D, E, F) :-
	'CHRgen_num'(G),
	coca(add_one_constraint(G, path(A, B, C, D, E, F))),
	'CHRpath_6'(path(A, B, C, D, E, F), H, I, G).



%%% Rules handling for path / 6

'CHRpath_6'(path(A, B, C, D, E, F), G, H, I) :-
	'CHRnonvar'(G),
	!.
'CHRpath_6'(path(A, B, C, D, E, F), G, H, I) ?-
	coca(try_rule(I, path(A, B, C, D, E, F), ground, path(J, K, L, M, N, O), replacement, ground(K - L - M - N), path1(J, K, L, M, N, O))),
	no_delayed_goals(ground(B - C - D - E)),
	!,
	'CHRkill'(G),
	coca(fired_rule(ground)),
	path1(A, B, C, D, E, F).
'CHRpath_6'(path(A, B, C, D, E, F), G, H, I) ?-
	coca(try_rule(I, path(A, B, C, D, E, F), empty, path(J, K, L, M, N, O), replacement, empty(J, M, N), fail)),
	no_delayed_goals(empty(A, D, E)),
	!,
	'CHRkill'(G),
	coca(fired_rule(empty)),
	fail.
'CHRpath_6'(path(A, B, C, D, E, F), G, H, I) ?-
	coca(try_rule(I, path(A, B, C, D, E, F), universal, path(J, K, L, M, N, O), replacement, universal(J, M, N), true)),
	no_delayed_goals(universal(A, D, E)),
	!,
	'CHRkill'(G),
	coca(fired_rule(universal)).
'CHRpath_6'(path(A, B, B, C, D, E), F, G, H) ?-
	coca(try_rule(H, path(A, B, B, C, D, E), equality, path(I, J, J, K, L, M), replacement, true, equality(K, L))),
	!,
	'CHRkill'(F),
	coca(fired_rule(equality)),
	equality(C, D).
'CHRpath_6'(path(1, A, B, C, D, E), F, G, H) ?-
	coca(try_rule(H, path(1, A, B, C, D, E), unify, path(1, I, J, K, L, M), replacement, (unique(K), equality(K, L)), I = J)),
	no_delayed_goals((unique(C), equality(C, D))),
	!,
	'CHRkill'(F),
	coca(fired_rule(unify)),
	A = B.
'CHRpath_6'(path(1, A, B, C, p - p, D), E, F, G) ?-
	coca(try_rule(G, path(1, A, B, C, p - p, D), findom_unique, path(1, H, I, J, p - p, K), replacement, (number(H), unique(J)), bind_value(H, I, J))),
	no_delayed_goals((number(A), unique(C))),
	!,
	'CHRkill'(E),
	coca(fired_rule(findom_unique)),
	bind_value(A, B, C).
'CHRpath_6'(path(A, B, C, D, p - p, E), F, G, H) ?-
	coca(try_rule(H, path(A, B, C, D, p - p, E), findom_x, path(I, J, K, L, p - p, M), replacement, (number(J), J =\= 0, shift_interval(J, L, N)), path(I, 0, K, N, p - p, M))),
	no_delayed_goals((number(B), B =\= 0, shift_interval(B, D, O))),
	!,
	'CHRkill'(F),
	coca(fired_rule(findom_x)),
	path(A, 0, C, O, p - p, E).
'CHRpath_6'(path(A, B, C, D, p - p, E), F, G, H) ?-
	coca(try_rule(H, path(A, B, C, D, p - p, E), findom_y, path(I, J, K, L, p - p, M), replacement, (number(K), equality([N], p - p), transl(L, O, [N], p - p - p), shift_interval(K, O, P)), path(I, 0, J, P, p - p, M))),
	no_delayed_goals((number(C), equality([Q], p - p), transl(D, R, [Q], p - p - p), shift_interval(C, R, S))),
	!,
	'CHRkill'(F),
	coca(fired_rule(findom_y)),
	path(A, 0, B, S, p - p, E).
'CHRpath_6'(path(A, B, C, D, E, F), G, H, I) :-
	'CHRpath_6__2'(path(A, B, C, D, E, F), G, H, I).
:- set_flag('CHRpath_6' / 4, leash, notrace).
:- current_macro('CHRpath_6' / 4, _7569, _7570, _7571) -> true ; define_macro('CHRpath_6' / 4, tr_chr / 2, [write]).
'CHRpath_6__2'(A, B, C, D) :-
	'CHRpath_6__3'(A, B, C, D).
:- set_flag('CHRpath_6__2' / 4, leash, notrace).
'CHRpath_6__3'(path(A, B, C, D, E - F, G), H, I, J) ?-
	'CHRvar'(H),
	!,
	'CHRget_delayed_goals'(F, K),
	'CHRpath_6__3__4'(K, H, path(A, B, C, D, E - F, G), I, J).
'CHRpath_6__3'(path(A, B, C, D, E, F), G, H, I) :-
	'CHRpath_6__3__5'(path(A, B, C, D, E, F), G, H, I).
:- set_flag('CHRpath_6__3' / 4, leash, notrace).
'CHRpath_6__3__4'(['CHRpath_6'(path(A, B, C, D, E - F, G), H, I, J)|K], L, path(M, B, C, N, E - F, O), P, Q) ?-
	'CHRvar'(H),
	'CHRcheck_and_mark_applied'('12'(intersect_xy_xy), L, H, P, I),
	coca(try_double(Q, path(M, B, C, N, E - F, O), J, path(A, B, C, D, E - F, G), path(R, S, T, U, V - W, X), path(Y, S, T, Z, V - W, A1), augmentation, ((intersection(U, Z, B1, V - W), length(B1, C1), D1 is min(X, A1)), 'CHRkeep_heads_checking'(path(R, S, T, U, V - W, X), E1, path(Y, S, T, Z, V - W, A1), F1, path(C1, S, T, B1, V - W, D1), G1)), 'CHRhead_not_kept'(G1) -> path(C1, S, T, B1, V - W, D1) ; true, intersect_xy_xy)),
	no_delayed_goals(((intersection(N, D, H1, E - F), length(H1, I1), J1 is min(O, G)), 'CHRkeep_heads_checking'(path(M, B, C, N, E - F, O), L, path(A, B, C, D, E - F, G), H, path(I1, B, C, H1, E - F, J1), K1))),
	!,
	coca(fired_rule(intersect_xy_xy)),
	'CHRpath_6__3__4'(K, L, path(M, B, C, N, E - F, O), P, Q),
	(
	    'CHRhead_not_kept'(K1)
	->
	    path(I1, B, C, H1, E - F, J1)
	;
	    true
	).
'CHRpath_6__3__4'([A|B], C, D, E, F) :-
	'CHRpath_6__3__4'(B, C, D, E, F).
'CHRpath_6__3__4'([], A, B, C, D) :-
	'CHRpath_6__3__5'(B, A, C, D).
:- set_flag('CHRpath_6__3__4' / 5, leash, notrace).
'CHRpath_6__3__5'(path(A, B, C, D, E - F, G), H, I, J) ?-
	'CHRvar'(H),
	!,
	'CHRget_delayed_goals'(F, K),
	'CHRpath_6__3__5__6'(K, H, path(A, B, C, D, E - F, G), I, J).
'CHRpath_6__3__5'(path(A, B, C, D, E, F), G, H, I) :-
	'CHRpath_6__3__5__7'(path(A, B, C, D, E, F), G, H, I).
:- set_flag('CHRpath_6__3__5' / 4, leash, notrace).
'CHRpath_6__3__5__6'(['CHRpath_6'(path(A, B, C, D, E - F, G), H, I, J)|K], L, path(M, B, C, N, E - F, O), P, Q) ?-
	'CHRvar'(H),
	'CHRcheck_and_mark_applied'('21'(intersect_xy_xy), L, H, P, I),
	coca(try_double(Q, path(M, B, C, N, E - F, O), J, path(A, B, C, D, E - F, G), path(R, S, T, U, V - W, X), path(Y, S, T, Z, V - W, A1), augmentation, ((intersection(Z, U, B1, V - W), length(B1, C1), D1 is min(A1, X)), 'CHRkeep_heads_checking'(path(Y, S, T, Z, V - W, A1), E1, path(R, S, T, U, V - W, X), F1, path(C1, S, T, B1, V - W, D1), G1)), 'CHRhead_not_kept'(G1) -> path(C1, S, T, B1, V - W, D1) ; true, intersect_xy_xy)),
	no_delayed_goals(((intersection(D, N, H1, E - F), length(H1, I1), J1 is min(G, O)), 'CHRkeep_heads_checking'(path(A, B, C, D, E - F, G), H, path(M, B, C, N, E - F, O), L, path(I1, B, C, H1, E - F, J1), K1))),
	!,
	coca(fired_rule(intersect_xy_xy)),
	'CHRpath_6__3__5__6'(K, L, path(M, B, C, N, E - F, O), P, Q),
	(
	    'CHRhead_not_kept'(K1)
	->
	    path(I1, B, C, H1, E - F, J1)
	;
	    true
	).
'CHRpath_6__3__5__6'([A|B], C, D, E, F) :-
	'CHRpath_6__3__5__6'(B, C, D, E, F).
'CHRpath_6__3__5__6'([], A, B, C, D) :-
	'CHRpath_6__3__5__7'(B, A, C, D).
:- set_flag('CHRpath_6__3__5__6' / 5, leash, notrace).
'CHRpath_6__3__5__7'(path(A, B, C, D, E - F, G), H, I, J) ?-
	'CHRvar'(H),
	!,
	'CHRget_delayed_goals'(F, K),
	'CHRpath_6__3__5__7__8'(K, H, path(A, B, C, D, E - F, G), I, J).
'CHRpath_6__3__5__7'(path(A, B, C, D, E, F), G, H, I) :-
	'CHRpath_6__3__5__7__9'(path(A, B, C, D, E, F), G, H, I).
:- set_flag('CHRpath_6__3__5__7' / 4, leash, notrace).
'CHRpath_6__3__5__7__8'(['CHRpath_6'(path(A, B, C, D, E - F, G), H, I, J)|K], L, path(M, C, B, N, F - E, O), P, Q) ?-
	'CHRvar'(H),
	'CHRcheck_and_mark_applied'('12'(intersect_yx_xy), L, H, P, I),
	coca(try_double(Q, path(M, C, B, N, F - E, O), J, path(A, B, C, D, E - F, G), path(R, S, T, U, V - W, X), path(Y, T, S, Z, W - V, A1), augmentation, ((equality([B1], W - W), transl(Z, C1, [B1], W - V - W), intersection(U, C1, D1, V - W), length(D1, E1), F1 is min(X, A1)), 'CHRkeep_heads_checking'(path(R, S, T, U, V - W, X), G1, path(Y, T, S, Z, W - V, A1), H1, path(E1, S, T, D1, V - W, F1), I1)), 'CHRhead_not_kept'(I1) -> path(E1, S, T, D1, V - W, F1) ; true, intersect_yx_xy)),
	no_delayed_goals(((equality([J1], E - E), transl(D, K1, [J1], E - F - E), intersection(N, K1, L1, F - E), length(L1, M1), N1 is min(O, G)), 'CHRkeep_heads_checking'(path(M, C, B, N, F - E, O), L, path(A, B, C, D, E - F, G), H, path(M1, C, B, L1, F - E, N1), O1))),
	!,
	coca(fired_rule(intersect_yx_xy)),
	'CHRpath_6__3__5__7__8'(K, L, path(M, C, B, N, F - E, O), P, Q),
	(
	    'CHRhead_not_kept'(O1)
	->
	    path(M1, C, B, L1, F - E, N1)
	;
	    true
	).
'CHRpath_6__3__5__7__8'([A|B], C, D, E, F) :-
	'CHRpath_6__3__5__7__8'(B, C, D, E, F).
'CHRpath_6__3__5__7__8'([], A, B, C, D) :-
	'CHRpath_6__3__5__7__9'(B, A, C, D).
:- set_flag('CHRpath_6__3__5__7__8' / 5, leash, notrace).
'CHRpath_6__3__5__7__9'(path(A, B, C, D, E - F, G), H, I, J) ?-
	'CHRvar'(H),
	!,
	'CHRget_delayed_goals'(F, K),
	'CHRpath_6__3__5__7__9__10'(K, H, path(A, B, C, D, E - F, G), I, J).
'CHRpath_6__3__5__7__9'(path(A, B, C, D, E, F), G, H, I) :-
	'CHRpath_6__3__5__7__9__11'(path(A, B, C, D, E, F), G, H, I).
:- set_flag('CHRpath_6__3__5__7__9' / 4, leash, notrace).
'CHRpath_6__3__5__7__9__10'(['CHRpath_6'(path(A, B, C, D, E - F, G), H, I, J)|K], L, path(M, C, B, N, F - E, O), P, Q) ?-
	'CHRvar'(H),
	'CHRcheck_and_mark_applied'('21'(intersect_yx_xy), L, H, P, I),
	coca(try_double(Q, path(M, C, B, N, F - E, O), J, path(A, B, C, D, E - F, G), path(R, S, T, U, V - W, X), path(Y, T, S, Z, W - V, A1), augmentation, ((equality([B1], V - V), transl(U, C1, [B1], V - W - V), intersection(Z, C1, D1, W - V), length(D1, E1), F1 is min(A1, X)), 'CHRkeep_heads_checking'(path(Y, T, S, Z, W - V, A1), G1, path(R, S, T, U, V - W, X), H1, path(E1, T, S, D1, W - V, F1), I1)), 'CHRhead_not_kept'(I1) -> path(E1, T, S, D1, W - V, F1) ; true, intersect_yx_xy)),
	no_delayed_goals(((equality([J1], F - F), transl(N, K1, [J1], F - E - F), intersection(D, K1, L1, E - F), length(L1, M1), N1 is min(G, O)), 'CHRkeep_heads_checking'(path(A, B, C, D, E - F, G), H, path(M, C, B, N, F - E, O), L, path(M1, B, C, L1, E - F, N1), O1))),
	!,
	coca(fired_rule(intersect_yx_xy)),
	'CHRpath_6__3__5__7__9__10'(K, L, path(M, C, B, N, F - E, O), P, Q),
	(
	    'CHRhead_not_kept'(O1)
	->
	    path(M1, B, C, L1, E - F, N1)
	;
	    true
	).
'CHRpath_6__3__5__7__9__10'([A|B], C, D, E, F) :-
	'CHRpath_6__3__5__7__9__10'(B, C, D, E, F).
'CHRpath_6__3__5__7__9__10'([], A, B, C, D) :-
	'CHRpath_6__3__5__7__9__11'(B, A, C, D).
:- set_flag('CHRpath_6__3__5__7__9__10' / 5, leash, notrace).
'CHRpath_6__3__5__7__9__11'(path(A, B, C, D, E - F, G), H, I, J) ?-
	'CHRvar'(H),
	!,
	'CHRget_delayed_goals'(F, K),
	'CHRpath_6__3__5__7__9__11__12'(K, H, path(A, B, C, D, E - F, G), I, J).
'CHRpath_6__3__5__7__9__11'(path(A, B, C, D, E, F), G, H, I) :-
	'CHRpath_6__3__5__7__9__11__13'(path(A, B, C, D, E, F), G, H, I).
:- set_flag('CHRpath_6__3__5__7__9__11' / 4, leash, notrace).
'CHRpath_6__3__5__7__9__11__12'(['CHRpath_6'(path(A, B, C, D, E - F, G), H, I, J)|K], L, path(M, N, B, O, P - E, Q), R, S) ?-
	'CHRvar'(H),
	'CHRcheck_and_mark_applied'('12'(propagate_xy_yz), L, H, R, I),
	coca(try_double(S, path(M, N, B, O, P - E, Q), J, path(A, B, C, D, E - F, G), path(T, U, V, W, X - Y, Z), path(A1, V, B1, C1, Y - D1, E1), augmentation, (nonground(V), E1 = 1, (Z = 1 -> U @< B1 ; true), transl(W, C1, F1, X - Y - D1), length(F1, G1), H1 is Z + E1), path(G1, U, B1, F1, X - D1, H1), propagate_xy_yz)),
	no_delayed_goals((nonground(B), G = 1, (Q = 1 -> N @< C ; true), transl(O, D, I1, P - E - F), length(I1, J1), K1 is Q + G)),
	!,
	coca(fired_rule(propagate_xy_yz)),
	'CHRpath_6__3__5__7__9__11__12'(K, L, path(M, N, B, O, P - E, Q), R, S),
	path(J1, N, C, I1, P - F, K1).
'CHRpath_6__3__5__7__9__11__12'([A|B], C, D, E, F) :-
	'CHRpath_6__3__5__7__9__11__12'(B, C, D, E, F).
'CHRpath_6__3__5__7__9__11__12'([], A, B, C, D) :-
	'CHRpath_6__3__5__7__9__11__13'(B, A, C, D).
:- set_flag('CHRpath_6__3__5__7__9__11__12' / 5, leash, notrace).
'CHRpath_6__3__5__7__9__11__13'(path(A, B, C, D, E - F, G), H, I, J) ?-
	'CHRvar'(H),
	!,
	'CHRget_delayed_goals'(E, K),
	'CHRpath_6__3__5__7__9__11__13__14'(K, H, path(A, B, C, D, E - F, G), I, J).
'CHRpath_6__3__5__7__9__11__13'(path(A, B, C, D, E, F), G, H, I) :-
	'CHRpath_6__3__5__7__9__11__13__15'(path(A, B, C, D, E, F), G, H, I).
:- set_flag('CHRpath_6__3__5__7__9__11__13' / 4, leash, notrace).
'CHRpath_6__3__5__7__9__11__13__14'(['CHRpath_6'(path(A, B, C, D, E - F, G), H, I, J)|K], L, path(M, C, N, O, F - P, Q), R, S) ?-
	'CHRvar'(H),
	'CHRcheck_and_mark_applied'('21'(propagate_xy_yz), L, H, R, I),
	coca(try_double(S, path(M, C, N, O, F - P, Q), J, path(A, B, C, D, E - F, G), path(T, U, V, W, X - Y, Z), path(A1, B1, U, C1, D1 - X, E1), augmentation, (nonground(U), Z = 1, (E1 = 1 -> B1 @< V ; true), transl(C1, W, F1, D1 - X - Y), length(F1, G1), H1 is E1 + Z), path(G1, B1, V, F1, D1 - Y, H1), propagate_xy_yz)),
	no_delayed_goals((nonground(C), Q = 1, (G = 1 -> B @< N ; true), transl(D, O, I1, E - F - P), length(I1, J1), K1 is G + Q)),
	!,
	coca(fired_rule(propagate_xy_yz)),
	'CHRpath_6__3__5__7__9__11__13__14'(K, L, path(M, C, N, O, F - P, Q), R, S),
	path(J1, B, N, I1, E - P, K1).
'CHRpath_6__3__5__7__9__11__13__14'([A|B], C, D, E, F) :-
	'CHRpath_6__3__5__7__9__11__13__14'(B, C, D, E, F).
'CHRpath_6__3__5__7__9__11__13__14'([], A, B, C, D) :-
	'CHRpath_6__3__5__7__9__11__13__15'(B, A, C, D).
:- set_flag('CHRpath_6__3__5__7__9__11__13__14' / 5, leash, notrace).
'CHRpath_6__3__5__7__9__11__13__15'(path(A, B, C, D, E - F, G), H, I, J) ?-
	'CHRvar'(H),
	!,
	'CHRget_delayed_goals'(E, K),
	'CHRpath_6__3__5__7__9__11__13__15__16'(K, H, path(A, B, C, D, E - F, G), I, J).
'CHRpath_6__3__5__7__9__11__13__15'(path(A, B, C, D, E, F), G, H, I) :-
	'CHRpath_6__3__5__7__9__11__13__15__17'(path(A, B, C, D, E, F), G, H, I).
:- set_flag('CHRpath_6__3__5__7__9__11__13__15' / 4, leash, notrace).
'CHRpath_6__3__5__7__9__11__13__15__16'(['CHRpath_6'(path(A, B, C, D, E - F, G), H, I, J)|K], L, path(M, B, N, O, E - P, Q), R, S) ?-
	'CHRvar'(H),
	'CHRcheck_and_mark_applied'('12'(propagate_xy_xz), L, H, R, I),
	coca(try_double(S, path(M, B, N, O, E - P, Q), J, path(A, B, C, D, E - F, G), path(T, U, V, W, X - Y, Z), path(A1, U, B1, C1, X - D1, E1), augmentation, (nonground(U), min(Z, E1, 1), V @< B1, transl(W, F1, C1, X - Y - D1), length(F1, G1), H1 is Z + E1), path(G1, V, B1, F1, Y - D1, H1), propagate_xy_xz)),
	no_delayed_goals((nonground(B), min(Q, G, 1), N @< C, transl(O, I1, D, E - P - F), length(I1, J1), K1 is Q + G)),
	!,
	coca(fired_rule(propagate_xy_xz)),
	'CHRpath_6__3__5__7__9__11__13__15__16'(K, L, path(M, B, N, O, E - P, Q), R, S),
	path(J1, N, C, I1, P - F, K1).
'CHRpath_6__3__5__7__9__11__13__15__16'([A|B], C, D, E, F) :-
	'CHRpath_6__3__5__7__9__11__13__15__16'(B, C, D, E, F).
'CHRpath_6__3__5__7__9__11__13__15__16'([], A, B, C, D) :-
	'CHRpath_6__3__5__7__9__11__13__15__17'(B, A, C, D).
:- set_flag('CHRpath_6__3__5__7__9__11__13__15__16' / 5, leash, notrace).
'CHRpath_6__3__5__7__9__11__13__15__17'(path(A, B, C, D, E - F, G), H, I, J) ?-
	'CHRvar'(H),
	!,
	'CHRget_delayed_goals'(E, K),
	'CHRpath_6__3__5__7__9__11__13__15__17__18'(K, H, path(A, B, C, D, E - F, G), I, J).
'CHRpath_6__3__5__7__9__11__13__15__17'(path(A, B, C, D, E, F), G, H, I) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19'(path(A, B, C, D, E, F), G, H, I).
:- set_flag('CHRpath_6__3__5__7__9__11__13__15__17' / 4, leash, notrace).
'CHRpath_6__3__5__7__9__11__13__15__17__18'(['CHRpath_6'(path(A, B, C, D, E - F, G), H, I, J)|K], L, path(M, B, N, O, E - P, Q), R, S) ?-
	'CHRvar'(H),
	'CHRcheck_and_mark_applied'('21'(propagate_xy_xz), L, H, R, I),
	coca(try_double(S, path(M, B, N, O, E - P, Q), J, path(A, B, C, D, E - F, G), path(T, U, V, W, X - Y, Z), path(A1, U, B1, C1, X - D1, E1), augmentation, (nonground(U), min(E1, Z, 1), B1 @< V, transl(C1, F1, W, X - D1 - Y), length(F1, G1), H1 is E1 + Z), path(G1, B1, V, F1, D1 - Y, H1), propagate_xy_xz)),
	no_delayed_goals((nonground(B), min(G, Q, 1), C @< N, transl(D, I1, O, E - F - P), length(I1, J1), K1 is G + Q)),
	!,
	coca(fired_rule(propagate_xy_xz)),
	'CHRpath_6__3__5__7__9__11__13__15__17__18'(K, L, path(M, B, N, O, E - P, Q), R, S),
	path(J1, C, N, I1, F - P, K1).
'CHRpath_6__3__5__7__9__11__13__15__17__18'([A|B], C, D, E, F) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__18'(B, C, D, E, F).
'CHRpath_6__3__5__7__9__11__13__15__17__18'([], A, B, C, D) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19'(B, A, C, D).
:- set_flag('CHRpath_6__3__5__7__9__11__13__15__17__18' / 5, leash, notrace).
'CHRpath_6__3__5__7__9__11__13__15__17__19'(path(A, B, C, D, E - F, G), H, I, J) ?-
	'CHRvar'(H),
	!,
	'CHRget_delayed_goals'(F, K),
	'CHRpath_6__3__5__7__9__11__13__15__17__19__20'(K, H, path(A, B, C, D, E - F, G), I, J).
'CHRpath_6__3__5__7__9__11__13__15__17__19'(path(A, B, C, D, E, F), G, H, I) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19__21'(path(A, B, C, D, E, F), G, H, I).
:- set_flag('CHRpath_6__3__5__7__9__11__13__15__17__19' / 4, leash, notrace).
'CHRpath_6__3__5__7__9__11__13__15__17__19__20'(['CHRpath_6'(path(A, B, C, D, E - F, G), H, I, J)|K], L, path(M, N, C, O, P - F, Q), R, S) ?-
	'CHRvar'(H),
	'CHRcheck_and_mark_applied'('12'(propagate_xy_zy), L, H, R, I),
	coca(try_double(S, path(M, N, C, O, P - F, Q), J, path(A, B, C, D, E - F, G), path(T, U, V, W, X - Y, Z), path(A1, B1, V, C1, D1 - Y, E1), augmentation, (nonground(V), min(Z, E1, 1), U @< B1, transl(F1, C1, W, X - D1 - Y), length(F1, G1), H1 is Z + E1), path(G1, U, B1, F1, X - D1, H1), propagate_xy_zy)),
	no_delayed_goals((nonground(C), min(Q, G, 1), N @< B, transl(I1, D, O, P - E - F), length(I1, J1), K1 is Q + G)),
	!,
	coca(fired_rule(propagate_xy_zy)),
	'CHRpath_6__3__5__7__9__11__13__15__17__19__20'(K, L, path(M, N, C, O, P - F, Q), R, S),
	path(J1, N, B, I1, P - E, K1).
'CHRpath_6__3__5__7__9__11__13__15__17__19__20'([A|B], C, D, E, F) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19__20'(B, C, D, E, F).
'CHRpath_6__3__5__7__9__11__13__15__17__19__20'([], A, B, C, D) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19__21'(B, A, C, D).
:- set_flag('CHRpath_6__3__5__7__9__11__13__15__17__19__20' / 5, leash, notrace).
'CHRpath_6__3__5__7__9__11__13__15__17__19__21'(path(A, B, C, D, E - F, G), H, I, J) ?-
	'CHRvar'(H),
	!,
	'CHRget_delayed_goals'(F, K),
	'CHRpath_6__3__5__7__9__11__13__15__17__19__21__22'(K, H, path(A, B, C, D, E - F, G), I, J).
'CHRpath_6__3__5__7__9__11__13__15__17__19__21'(path(A, B, C, D, E, F), G, H, I) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19__21__23'(path(A, B, C, D, E, F), G, H, I).
:- set_flag('CHRpath_6__3__5__7__9__11__13__15__17__19__21' / 4, leash, notrace).
'CHRpath_6__3__5__7__9__11__13__15__17__19__21__22'(['CHRpath_6'(path(A, B, C, D, E - F, G), H, I, J)|K], L, path(M, N, C, O, P - F, Q), R, S) ?-
	'CHRvar'(H),
	'CHRcheck_and_mark_applied'('21'(propagate_xy_zy), L, H, R, I),
	coca(try_double(S, path(M, N, C, O, P - F, Q), J, path(A, B, C, D, E - F, G), path(T, U, V, W, X - Y, Z), path(A1, B1, V, C1, D1 - Y, E1), augmentation, (nonground(V), min(E1, Z, 1), B1 @< U, transl(F1, W, C1, D1 - X - Y), length(F1, G1), H1 is E1 + Z), path(G1, B1, U, F1, D1 - X, H1), propagate_xy_zy)),
	no_delayed_goals((nonground(C), min(G, Q, 1), B @< N, transl(I1, O, D, E - P - F), length(I1, J1), K1 is G + Q)),
	!,
	coca(fired_rule(propagate_xy_zy)),
	'CHRpath_6__3__5__7__9__11__13__15__17__19__21__22'(K, L, path(M, N, C, O, P - F, Q), R, S),
	path(J1, B, N, I1, E - P, K1).
'CHRpath_6__3__5__7__9__11__13__15__17__19__21__22'([A|B], C, D, E, F) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19__21__22'(B, C, D, E, F).
'CHRpath_6__3__5__7__9__11__13__15__17__19__21__22'([], A, B, C, D) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19__21__23'(B, A, C, D).
:- set_flag('CHRpath_6__3__5__7__9__11__13__15__17__19__21__22' / 5, leash, notrace).
'CHRpath_6__3__5__7__9__11__13__15__17__19__21__23'(path(A, B, C, D, E, F), G, H, I) :-
	(
	    'CHRvar'(G)
	->
	    'CHRdelay'([G, path(A, B, C, D, E, F)], 'CHRpath_6'(path(A, B, C, D, E, F), G, H, I))
	;
	    true
	).
:- set_flag('CHRpath_6__3__5__7__9__11__13__15__17__19__21__23' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
