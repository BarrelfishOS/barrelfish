
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- ['math-ineq.pl'].
:- op(100, xfx, equals).
:- op(450, xfx, eqnonlin).
equals(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, equals(A, B))),
	'CHRequals_2'(equals(A, B), D, E, C).



%%% Rules handling for equals / 2

'CHRequals_2'(equals(A, B), C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHRequals_2'(equals([], A), B, C, D) ?-
	coca(try_rule(D, equals([], A), anonymous("0"), equals([], E), replacement, true, zero(E))),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("0"))),
	zero(A).
'CHRequals_2'(equals([A * B], C), D, E, F) ?-
	coca(try_rule(F, equals([A * B], C), anonymous("1"), equals([G * H], I), replacement, (nonground(G), nonzero(H)), is_div(I, H, G))),
	no_delayed_goals((nonground(A), nonzero(B))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("1"))),
	is_div(C, B, A).
'CHRequals_2'(equals(A, B), C, D, E) ?-
	coca(try_rule(E, equals(A, B), anonymous("2"), equals(F, G), replacement, simplifyable(H * I, F, J), (is_mul(H, I, K), L is K + G, equals(J, L)))),
	no_delayed_goals(simplifyable(M * N, A, O)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("2"))),
	is_mul(M, N, P),
	Q is P + B,
	equals(O, Q).
'CHRequals_2'(equals(A, B), C, D, E) ?-
	coca(try_rule(E, equals(A, B), anonymous("4"), equals(F, G), replacement, (all_slacks(F, H), sign(G, I), (I = 0 ; I = H)), (I = 0, all_zeroes(F)))),
	no_delayed_goals((all_slacks(A, J), sign(B, K), (K = 0 ; K = J))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("4"))),
	K = 0,
	all_zeroes(A).
'CHRequals_2'(equals([A * B|C], D), E, F, G) ?-
	'CHRget_delayed_goals'(A, H),
	'CHRequals_2__10'(H, [A], [I, J, K], L),
	coca(try_double(G, equals([A * B|C], D), L, equals([A * K|J], I), equals([M * N|O], P), equals([M * Q|R], S), keep_second, true, (is_div(N, Q, T), mult_const(eq0(S, R), T, U), add_eq0(eq0(P, O), U, eq0(V, W)), sort1(W, X), equals(X, V)), anonymous("3"))),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("3"))),
	is_div(B, K, Y),
	mult_const(eq0(I, J), Y, Z),
	add_eq0(eq0(D, C), Z, eq0(A1, B1)),
	sort1(B1, C1),
	equals(C1, A1).
'CHRequals_2'(equals(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(true, F),
	'CHRequals_2__11'(F, [], [G, H], I),
	coca(try_double(E, equals(A, B), I, equals(H, G), equals(J, K), equals(L, M), keep_second, (nonzero(M), nonzero(K), all_slacks(L), all_slacks(J)), (N = 0, O = 0, is_div(K, M, P), mult_const(eq0(N, L), P, Q), add_eq0(eq0(O, J), Q, eq0(R, S)), equals(S, R)), anonymous("5"))),
	no_delayed_goals((nonzero(G), nonzero(B), all_slacks(H), all_slacks(A))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("5"))),
	[T, U] = [0, 0],
	is_div(B, G, V),
	mult_const(eq0(T, H), V, W),
	add_eq0(eq0(U, A), W, eq0(X, Y)),
	equals(Y, X).
'CHRequals_2'(equals(A, B), C, D, E) :-
	'CHRequals_2__9'(equals(A, B), C, D, E).
'CHRequals_2__10'(['CHRequals_2'(equals([A * B|C], D), E, F, G)|H], [A], [I, J, K], L) ?-
	'CHRvar'(E),
	'CHR='([D, C, B], [I, J, K]),
	'CHR='(G, L).
'CHRequals_2__10'([A|B], C, D, E) :-
	'CHRequals_2__10'(B, C, D, E).
:- set_flag('CHRequals_2__10' / 4, leash, notrace).
'CHRequals_2__11'(['CHRequals_2'(equals(A, B), C, D, E)|F], [], [G, H], I) ?-
	'CHRvar'(C),
	'CHR='([B, A], [G, H]),
	'CHR='(E, I).
'CHRequals_2__11'([A|B], C, D, E) :-
	'CHRequals_2__11'(B, C, D, E).
:- set_flag('CHRequals_2__11' / 4, leash, notrace).
:- set_flag('CHRequals_2' / 4, leash, notrace).
:- current_macro('CHRequals_2' / 4, _3838, _3839, _3840) -> true ; define_macro('CHRequals_2' / 4, tr_chr / 2, [write]).
'CHRequals_2__9'(A, B, C, D) :-
	'CHRequals_2__12'(A, B, C, D).
:- set_flag('CHRequals_2__9' / 4, leash, notrace).
'CHRequals_2__12'(equals([A * B|C], D), E, F, G) ?-
	'CHRvar'(E),
	!,
	'CHRget_delayed_goals'(A, H),
	'CHRequals_2__12__13'(H, E, equals([A * B|C], D), F, G).
'CHRequals_2__12'(equals(A, B), C, D, E) :-
	'CHRequals_2__12__14'(equals(A, B), C, D, E).
:- set_flag('CHRequals_2__12' / 4, leash, notrace).
'CHRequals_2__12__13'(['CHRequals_2'(equals([A * B|C], D), E, F, G)|H], I, equals([A * J|K], L), M, N) ?-
	'CHRvar'(E),
	coca(try_double(N, equals([A * J|K], L), G, equals([A * B|C], D), equals([O * P|Q], R), equals([O * S|T], U), keep_first, true, (is_div(S, P, V), mult_const(eq0(R, Q), V, W), add_eq0(eq0(U, T), W, eq0(X, Y)), sort1(Y, Z), equals(Z, X)), anonymous("3"))),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("3"))),
	'CHRequals_2__12__13'(H, I, equals([A * J|K], L), M, N),
	is_div(B, J, A1),
	mult_const(eq0(L, K), A1, B1),
	add_eq0(eq0(D, C), B1, eq0(C1, D1)),
	sort1(D1, E1),
	equals(E1, C1).
'CHRequals_2__12__13'([A|B], C, D, E, F) :-
	'CHRequals_2__12__13'(B, C, D, E, F).
'CHRequals_2__12__13'([], A, B, C, D) :-
	'CHRequals_2__12__14'(B, A, C, D).
:- set_flag('CHRequals_2__12__13' / 5, leash, notrace).
'CHRequals_2__12__14'(equals(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(true, F),
	'CHRequals_2__12__14__15'(F, C, equals(A, B), D, E).
'CHRequals_2__12__14'(equals(A, B), C, D, E) :-
	'CHRequals_2__12__14__16'(equals(A, B), C, D, E).
:- set_flag('CHRequals_2__12__14' / 4, leash, notrace).
'CHRequals_2__12__14__15'(['CHRequals_2'(equals(A, B), C, D, E)|F], G, equals(H, I), J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, equals(H, I), E, equals(A, B), equals(L, M), equals(N, O), keep_first, (nonzero(M), nonzero(O), all_slacks(L), all_slacks(N)), (P = 0, Q = 0, is_div(O, M, R), mult_const(eq0(P, L), R, S), add_eq0(eq0(Q, N), S, eq0(T, U)), equals(U, T)), anonymous("5"))),
	no_delayed_goals((nonzero(I), nonzero(B), all_slacks(H), all_slacks(A))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("5"))),
	'CHRequals_2__12__14__15'(F, G, equals(H, I), J, K),
	[V, W] = [0, 0],
	is_div(B, I, X),
	mult_const(eq0(V, H), X, Y),
	add_eq0(eq0(W, A), Y, eq0(Z, A1)),
	equals(A1, Z).
'CHRequals_2__12__14__15'([A|B], C, D, E, F) :-
	'CHRequals_2__12__14__15'(B, C, D, E, F).
'CHRequals_2__12__14__15'([], A, B, C, D) :-
	'CHRequals_2__12__14__16'(B, A, C, D).
:- set_flag('CHRequals_2__12__14__15' / 5, leash, notrace).
'CHRequals_2__12__14__16'(equals(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, equals(A, B)], 'CHRequals_2'(equals(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRequals_2__12__14__16' / 4, leash, notrace).
eqnonlin(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, eqnonlin(A, B))),
	'CHReqnonlin_2'(eqnonlin(A, B), D, E, C).



%%% Rules handling for eqnonlin / 2

'CHReqnonlin_2'(eqnonlin(A, B), C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHReqnonlin_2'(eqnonlin(A, B), C, D, E) ?-
	coca(try_rule(E, eqnonlin(A, B), anonymous("6"), eqnonlin(F, G), replacement, ground(G), (H is G, F =:= H))),
	no_delayed_goals(ground(B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("6"))),
	I is B,
	A =:= I.
'CHReqnonlin_2'(eqnonlin(A, B * C), D, E, F) ?-
	coca(try_rule(F, eqnonlin(A, B * C), anonymous("7"), eqnonlin(G, H * I), replacement, ground(H), (J is H, G =:= J * I))),
	no_delayed_goals(ground(B)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("7"))),
	K is B,
	A =:= K * C.
'CHReqnonlin_2'(eqnonlin(A, B * C), D, E, F) ?-
	coca(try_rule(F, eqnonlin(A, B * C), anonymous("8"), eqnonlin(G, H * I), replacement, ground(I), (J is I, G =:= J * H))),
	no_delayed_goals(ground(C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("8"))),
	K is C,
	A =:= K * B.
'CHReqnonlin_2'(eqnonlin(A, B), C, D, E) :-
	'CHReqnonlin_2__17'(eqnonlin(A, B), C, D, E).
:- set_flag('CHReqnonlin_2' / 4, leash, notrace).
:- current_macro('CHReqnonlin_2' / 4, _6469, _6470, _6471) -> true ; define_macro('CHReqnonlin_2' / 4, tr_chr / 2, [write]).
'CHReqnonlin_2__17'(A, B, C, D) :-
	'CHReqnonlin_2__18'(A, B, C, D).
:- set_flag('CHReqnonlin_2__17' / 4, leash, notrace).
'CHReqnonlin_2__18'(eqnonlin(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, eqnonlin(A, B)], 'CHReqnonlin_2'(eqnonlin(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHReqnonlin_2__18' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
