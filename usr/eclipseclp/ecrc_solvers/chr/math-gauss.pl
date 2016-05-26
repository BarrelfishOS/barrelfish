
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- local (=:=) / 2.
:- op(100, xfx, equals).
:- ['math-utilities'].
A =:= B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A =:= B)),
	'CHR=:=_2'(A =:= B, D, E, C).



%%% Rules handling for =:= / 2

'CHR=:=_2'(A =:= B, C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHR=:=_2'(A =:= B, C, D, E)),
	    coca(already_in)
	),
	!.
'CHR=:=_2'(A =:= B, C, D, E) ?-
	coca(try_rule(E, A =:= B, normalize, F =:= G, replacement, true, (normalize(F, G, H, I), equals(H, I)))),
	!,
	'CHRkill'(C),
	coca(fired_rule(normalize)),
	normalize(A, B, J, K),
	equals(J, K).
'CHR=:=_2'(A =:= B, C, D, E) :-
	'CHR=:=_2__0'(A =:= B, C, D, E).
:- set_flag('CHR=:=_2' / 4, leash, notrace).
:- current_macro('CHR=:=_2' / 4, _1353, _1354, _1355) -> true ; define_macro('CHR=:=_2' / 4, tr_chr / 2, [write]).
'CHR=:=_2__0'(A, B, C, D) :-
	'CHR=:=_2__1'(A, B, C, D).
:- set_flag('CHR=:=_2__0' / 4, leash, notrace).
'CHR=:=_2__1'(A =:= B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A =:= B], 'CHR=:=_2'(A =:= B, C, D, E))
	;
	    true
	).
:- set_flag('CHR=:=_2__1' / 4, leash, notrace).
equals(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, equals(A, B))),
	'CHRequals_2'(equals(A, B), D, E, C).



%%% Rules handling for equals / 2

'CHRequals_2'(equals(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRequals_2'(equals(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRequals_2'(equals(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(true, F),
	'CHRequals_2__3'(F, [], [G, H, I, J], K),
	coca(try_double(E, equals(A, B), K, equals([J * I|H], G), equals(L, M), equals([N * O|P], Q), keep_second, delete(N * R, L, S), (is_div(R, O, T), mult_const(eq0(Q, P), T, eq0(U, V)), add_eq0(eq0(M, S), eq0(U, V), eq0(W, X)), equals(X, W)), eliminate)),
	no_global_bindings(delete(J * Y, A, Z), (equals(A, B), equals([J * I|H], G))),
	!,
	'CHRkill'(C),
	coca(fired_rule(eliminate)),
	is_div(Y, I, A1),
	mult_const(eq0(G, H), A1, eq0(B1, C1)),
	add_eq0(eq0(B, Z), eq0(B1, C1), eq0(D1, E1)),
	equals(E1, D1).
'CHRequals_2'(equals(A, B), C, D, E) :-
	'CHRequals_2__2'(equals(A, B), C, D, E).
'CHRequals_2__3'(['CHRequals_2'(equals([A * B|C], D), E, F, G)|H], [], [I, J, K, L], M) ?-
	'CHRvar'(E),
	'CHR='([D, C, B, A], [I, J, K, L]),
	'CHR='(G, M).
'CHRequals_2__3'([A|B], C, D, E) :-
	'CHRequals_2__3'(B, C, D, E).
:- set_flag('CHRequals_2__3' / 4, leash, notrace).
:- set_flag('CHRequals_2' / 4, leash, notrace).
:- current_macro('CHRequals_2' / 4, _2599, _2600, _2601) -> true ; define_macro('CHRequals_2' / 4, tr_chr / 2, [write]).
'CHRequals_2__2'(A, B, C, D) :-
	'CHRequals_2__4'(A, B, C, D).
:- set_flag('CHRequals_2__2' / 4, leash, notrace).
'CHRequals_2__4'(equals([A * B|C], D), E, F, G) ?-
	'CHRvar'(E),
	!,
	'CHRget_delayed_goals'(true, H),
	'CHRequals_2__4__5'(H, E, equals([A * B|C], D), F, G).
'CHRequals_2__4'(equals(A, B), C, D, E) :-
	'CHRequals_2__4__6'(equals(A, B), C, D, E).
:- set_flag('CHRequals_2__4' / 4, leash, notrace).
'CHRequals_2__4__5'(['CHRequals_2'(equals(A, B), C, D, E)|F], G, equals([H * I|J], K), L, M) ?-
	'CHRvar'(C),
	coca(try_double(M, equals([H * I|J], K), E, equals(A, B), equals([N * O|P], Q), equals(R, S), keep_first, delete(N * T, R, U), (is_div(T, O, V), mult_const(eq0(Q, P), V, eq0(W, X)), add_eq0(eq0(S, U), eq0(W, X), eq0(Y, Z)), equals(Z, Y)), eliminate)),
	no_global_bindings(delete(H * A1, A, B1), (equals([H * I|J], K), equals(A, B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(eliminate)),
	'CHRequals_2__4__5'(F, G, equals([H * I|J], K), L, M),
	is_div(A1, I, C1),
	mult_const(eq0(K, J), C1, eq0(D1, E1)),
	add_eq0(eq0(B, B1), eq0(D1, E1), eq0(F1, G1)),
	equals(G1, F1).
'CHRequals_2__4__5'([A|B], C, D, E, F) :-
	'CHRequals_2__4__5'(B, C, D, E, F).
'CHRequals_2__4__5'([], A, B, C, D) :-
	'CHRequals_2__4__6'(B, A, C, D).
:- set_flag('CHRequals_2__4__5' / 5, leash, notrace).
'CHRequals_2__4__6'(equals(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, equals(A, B)], 'CHRequals_2'(equals(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRequals_2__4__6' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
