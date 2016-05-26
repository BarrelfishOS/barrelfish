
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
	coca(try_rule(D, equals([], A), empty, equals([], E), replacement, true, zero(E))),
	!,
	'CHRkill'(B),
	coca(fired_rule(empty)),
	zero(A).
'CHRequals_2'(equals([A * B], C), D, E, F) ?-
	coca(try_rule(F, equals([A * B], C), unify, equals([G * H], I), replacement, nonground(G), is_div(I, H, G))),
	no_delayed_goals(nonground(A)),
	!,
	'CHRkill'(D),
	coca(fired_rule(unify)),
	is_div(C, B, A).
'CHRequals_2'(equals(A, B), C, D, E) ?-
	coca(try_rule(E, equals(A, B), simplify, equals(F, G), replacement, (delete(H * I, F, J), ground(H)), (is_mul(H, I, K), L is K + G, equals(J, L)))),
	no_delayed_goals((delete(M * N, A, O), ground(M))),
	!,
	'CHRkill'(C),
	coca(fired_rule(simplify)),
	is_mul(M, N, P),
	Q is P + B,
	equals(O, Q).
'CHRequals_2'(equals(A, B), C, D, E) ?-
	coca(try_rule(E, equals(A, B), slack, equals(F, G), replacement, (all_slacks(F, H), sign(G, I), (I = 0 ; I = H)), (I = 0, all_zeroes(F)))),
	no_delayed_goals((all_slacks(A, J), sign(B, K), (K = 0 ; K = J))),
	!,
	'CHRkill'(C),
	coca(fired_rule(slack)),
	K = 0,
	all_zeroes(A).
'CHRequals_2'(equals(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(true, F),
	'CHRequals_2__1'(F, [], [G, H, I, J], K),
	coca(try_double(E, equals(A, B), K, equals([J * I|H], G), equals(L, M), equals([N * O|P], Q), keep_second, (delete(R * S, L, T), N == R), (is_div(S, O, U), mult_const(eq0(Q, P), U, V), add_eq0(eq0(M, T), V, eq0(W, X)), equals(X, W)), eliminate)),
	no_delayed_goals((delete(Y * Z, A, A1), J == Y)),
	!,
	'CHRkill'(C),
	coca(fired_rule(eliminate)),
	is_div(Z, I, B1),
	mult_const(eq0(G, H), B1, C1),
	add_eq0(eq0(B, A1), C1, eq0(D1, E1)),
	equals(E1, D1).
'CHRequals_2'(equals(A, B), C, D, E) :-
	'CHRequals_2__0'(equals(A, B), C, D, E).
'CHRequals_2__1'(['CHRequals_2'(equals([A * B|C], D), E, F, G)|H], [], [I, J, K, L], M) ?-
	'CHRvar'(E),
	'CHR='([D, C, B, A], [I, J, K, L]),
	'CHR='(G, M).
'CHRequals_2__1'([A|B], C, D, E) :-
	'CHRequals_2__1'(B, C, D, E).
:- set_flag('CHRequals_2__1' / 4, leash, notrace).
:- set_flag('CHRequals_2' / 4, leash, notrace).
:- current_macro('CHRequals_2' / 4, _2987, _2988, _2989) -> true ; define_macro('CHRequals_2' / 4, tr_chr / 2, [write]).
'CHRequals_2__0'(A, B, C, D) :-
	'CHRequals_2__2'(A, B, C, D).
:- set_flag('CHRequals_2__0' / 4, leash, notrace).
'CHRequals_2__2'(equals([A * B|C], D), E, F, G) ?-
	'CHRvar'(E),
	!,
	'CHRget_delayed_goals'(true, H),
	'CHRequals_2__2__3'(H, E, equals([A * B|C], D), F, G).
'CHRequals_2__2'(equals(A, B), C, D, E) :-
	'CHRequals_2__2__4'(equals(A, B), C, D, E).
:- set_flag('CHRequals_2__2' / 4, leash, notrace).
'CHRequals_2__2__3'(['CHRequals_2'(equals(A, B), C, D, E)|F], G, equals([H * I|J], K), L, M) ?-
	'CHRvar'(C),
	coca(try_double(M, equals([H * I|J], K), E, equals(A, B), equals([N * O|P], Q), equals(R, S), keep_first, (delete(T * U, R, V), N == T), (is_div(U, O, W), mult_const(eq0(Q, P), W, X), add_eq0(eq0(S, V), X, eq0(Y, Z)), equals(Z, Y)), eliminate)),
	no_delayed_goals((delete(A1 * B1, A, C1), H == A1)),
	!,
	'CHRkill'(C),
	coca(fired_rule(eliminate)),
	'CHRequals_2__2__3'(F, G, equals([H * I|J], K), L, M),
	is_div(B1, I, D1),
	mult_const(eq0(K, J), D1, E1),
	add_eq0(eq0(B, C1), E1, eq0(F1, G1)),
	equals(G1, F1).
'CHRequals_2__2__3'([A|B], C, D, E, F) :-
	'CHRequals_2__2__3'(B, C, D, E, F).
'CHRequals_2__2__3'([], A, B, C, D) :-
	'CHRequals_2__2__4'(B, A, C, D).
:- set_flag('CHRequals_2__2__3' / 5, leash, notrace).
'CHRequals_2__2__4'(equals(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, equals(A, B)], 'CHRequals_2'(equals(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRequals_2__2__4' / 4, leash, notrace).
eqnonlin(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, eqnonlin(A, B))),
	'CHReqnonlin_2'(eqnonlin(A, B), D, E, C).



%%% Rules handling for eqnonlin / 2

'CHReqnonlin_2'(eqnonlin(A, B), C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHReqnonlin_2'(eqnonlin(A, B), C, D, E) ?-
	coca(try_rule(E, eqnonlin(A, B), linearize, eqnonlin(F, G), replacement, ground(G), (H is G, F =:= H))),
	no_delayed_goals(ground(B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(linearize)),
	I is B,
	A =:= I.
'CHReqnonlin_2'(eqnonlin(A, B * C), D, E, F) ?-
	coca(try_rule(F, eqnonlin(A, B * C), linearize, eqnonlin(G, H * I), replacement, ground(H), (J is H, G =:= J * I))),
	no_delayed_goals(ground(B)),
	!,
	'CHRkill'(D),
	coca(fired_rule(linearize)),
	K is B,
	A =:= K * C.
'CHReqnonlin_2'(eqnonlin(A, B * C), D, E, F) ?-
	coca(try_rule(F, eqnonlin(A, B * C), linearize, eqnonlin(G, H * I), replacement, ground(I), (J is I, G =:= J * H))),
	no_delayed_goals(ground(C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(linearize)),
	K is C,
	A =:= K * B.
'CHReqnonlin_2'(eqnonlin(A, B), C, D, E) :-
	'CHReqnonlin_2__5'(eqnonlin(A, B), C, D, E).
:- set_flag('CHReqnonlin_2' / 4, leash, notrace).
:- current_macro('CHReqnonlin_2' / 4, _4837, _4838, _4839) -> true ; define_macro('CHReqnonlin_2' / 4, tr_chr / 2, [write]).
'CHReqnonlin_2__5'(A, B, C, D) :-
	'CHReqnonlin_2__6'(A, B, C, D).
:- set_flag('CHReqnonlin_2__5' / 4, leash, notrace).
'CHReqnonlin_2__6'(eqnonlin(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, eqnonlin(A, B)], 'CHReqnonlin_2'(eqnonlin(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHReqnonlin_2__6' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
