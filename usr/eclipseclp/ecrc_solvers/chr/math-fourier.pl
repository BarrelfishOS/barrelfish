
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- local (<) / 2, (>) / 2, (=\=) / 2, (>=) / 2, (=<) / 2, (=:=) / 2.
:- ['math-utilities'].
:- op(450, xfx, eqnonlin).
inequation(>).
inequation(>=).
combine(=:=, _3916, _3916) :- inequation(_3916).
combine(_3933, =:=, _3933) :- inequation(_3933).
combine(>, _3949, >).
combine(_3961, >, >).
combine(>=, >=, >=).
same_poly([], [], _3984).
same_poly([_4029 * _4023|_4010], [_4028 * _4024|_4009], _4008) :- _4029 == _4028, _4013 is _4008 - _4024 / _4023, zero(_4013), same_poly(_4010, _4009, _4008).
stronger(_4101, _4123, _4116, _4087, _4122, _4112) :- call_kernel(_4123 =:= _4122) -> not (_4116 = (>=), _4112 = (>)), _4075 is abs(_4101) + 1 / abs(_4101), _4074 is abs(_4087) + 1 / abs(_4087), call_kernel(_4075 =< _4074) ; true.
A < B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A < B)),
	'CHR<_2'(A < B, D, E, C).



%%% Rules handling for < / 2

'CHR<_2'(A < B, C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHR<_2'(A < B, C, D, E)),
	    coca(already_in)
	),
	!.
'CHR<_2'(A < B, C, D, E) ?-
	coca(try_rule(E, A < B, anonymous("2"), F < G, replacement, (ground(F), ground(G)), call_kernel(F < G))),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("2"))),
	call_kernel(A < B).
'CHR<_2'(A < B, C, D, E) ?-
	coca(try_rule(E, A < B, anonymous("7"), F < G, replacement, true, (normalize(G, F, H, I), eq(H, I, >)))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("7"))),
	normalize(B, A, J, K),
	eq(J, K, >).
'CHR<_2'(A < B, C, D, E) :-
	'CHR<_2__23'(A < B, C, D, E).
:- set_flag('CHR<_2' / 4, leash, notrace).
:- current_macro('CHR<_2' / 4, _5026, _5027, _5028) -> true ; define_macro('CHR<_2' / 4, tr_chr / 2, [write]).
'CHR<_2__23'(A, B, C, D) :-
	'CHR<_2__24'(A, B, C, D).
:- set_flag('CHR<_2__23' / 4, leash, notrace).
'CHR<_2__24'(A < B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A < B], 'CHR<_2'(A < B, C, D, E))
	;
	    true
	).
:- set_flag('CHR<_2__24' / 4, leash, notrace).
A > B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A > B)),
	'CHR>_2'(A > B, D, E, C).



%%% Rules handling for > / 2

'CHR>_2'(A > B, C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHR>_2'(A > B, C, D, E)),
	    coca(already_in)
	),
	!.
'CHR>_2'(A > B, C, D, E) ?-
	coca(try_rule(E, A > B, anonymous("3"), F > G, replacement, (ground(F), ground(G)), call_kernel(F > G))),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("3"))),
	call_kernel(A > B).
'CHR>_2'(A > B, C, D, E) ?-
	coca(try_rule(E, A > B, anonymous("8"), F > G, replacement, true, (normalize(F, G, H, I), eq(H, I, >)))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("8"))),
	normalize(A, B, J, K),
	eq(J, K, >).
'CHR>_2'(A > B, C, D, E) :-
	'CHR>_2__25'(A > B, C, D, E).
:- set_flag('CHR>_2' / 4, leash, notrace).
:- current_macro('CHR>_2' / 4, _5970, _5971, _5972) -> true ; define_macro('CHR>_2' / 4, tr_chr / 2, [write]).
'CHR>_2__25'(A, B, C, D) :-
	'CHR>_2__26'(A, B, C, D).
:- set_flag('CHR>_2__25' / 4, leash, notrace).
'CHR>_2__26'(A > B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A > B], 'CHR>_2'(A > B, C, D, E))
	;
	    true
	).
:- set_flag('CHR>_2__26' / 4, leash, notrace).
A =\= B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A =\= B)),
	'CHR=\\=_2'(A =\= B, D, E, C).



%%% Rules handling for =\= / 2

'CHR=\\=_2'(A =\= B, C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHR=\\=_2'(A =\= B, C, D, E)),
	    coca(already_in)
	),
	!.
'CHR=\\=_2'(A =\= B, C, D, E) ?-
	coca(try_rule(E, A =\= B, anonymous("4"), F =\= G, replacement, (ground(F), ground(G)), call_kernel(F =\= G))),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("4"))),
	call_kernel(A =\= B).
'CHR=\\=_2'(A =\= B, C, D, E) ?-
	coca(try_rule(E, A =\= B, anonymous("9"), F =\= G, replacement, true, (F + H =:= G, call_kernel(H =\= 0)))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("9"))),
	A + I =:= B,
	call_kernel(I =\= 0).
'CHR=\\=_2'(A =\= B, C, D, E) :-
	'CHR=\\=_2__27'(A =\= B, C, D, E).
:- set_flag('CHR=\\=_2' / 4, leash, notrace).
:- current_macro('CHR=\\=_2' / 4, _6916, _6917, _6918) -> true ; define_macro('CHR=\\=_2' / 4, tr_chr / 2, [write]).
'CHR=\\=_2__27'(A, B, C, D) :-
	'CHR=\\=_2__28'(A, B, C, D).
:- set_flag('CHR=\\=_2__27' / 4, leash, notrace).
'CHR=\\=_2__28'(A =\= B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A =\= B], 'CHR=\\=_2'(A =\= B, C, D, E))
	;
	    true
	).
:- set_flag('CHR=\\=_2__28' / 4, leash, notrace).
A >= B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A >= B)),
	'CHR>=_2'(A >= B, D, E, C).



%%% Rules handling for >= / 2

'CHR>=_2'(A >= B, C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHR>=_2'(A >= B, C, D, E)),
	    coca(already_in)
	),
	!.
'CHR>=_2'(A >= B, C, D, E) ?-
	coca(try_rule(E, A >= B, anonymous("1"), F >= G, replacement, (ground(F), ground(G)), call_kernel(F >= G))),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("1"))),
	call_kernel(A >= B).
'CHR>=_2'(A >= B, C, D, E) ?-
	coca(try_rule(E, A >= B, anonymous("6"), F >= G, replacement, true, (normalize(F, G, H, I), eq(H, I, >=)))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("6"))),
	normalize(A, B, J, K),
	eq(J, K, >=).
'CHR>=_2'(A >= B, C, D, E) :-
	'CHR>=_2__29'(A >= B, C, D, E).
:- set_flag('CHR>=_2' / 4, leash, notrace).
:- current_macro('CHR>=_2' / 4, _7863, _7864, _7865) -> true ; define_macro('CHR>=_2' / 4, tr_chr / 2, [write]).
'CHR>=_2__29'(A, B, C, D) :-
	'CHR>=_2__30'(A, B, C, D).
:- set_flag('CHR>=_2__29' / 4, leash, notrace).
'CHR>=_2__30'(A >= B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A >= B], 'CHR>=_2'(A >= B, C, D, E))
	;
	    true
	).
:- set_flag('CHR>=_2__30' / 4, leash, notrace).
A =< B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A =< B)),
	'CHR=<_2'(A =< B, D, E, C).



%%% Rules handling for =< / 2

'CHR=<_2'(A =< B, C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHR=<_2'(A =< B, C, D, E)),
	    coca(already_in)
	),
	!.
'CHR=<_2'(A =< B, C, D, E) ?-
	coca(try_rule(E, A =< B, anonymous("0"), F =< G, replacement, (ground(F), ground(G)), call_kernel(F =< G))),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("0"))),
	call_kernel(A =< B).
'CHR=<_2'(A =< B, C, D, E) ?-
	coca(try_rule(E, A =< B, anonymous("5"), F =< G, replacement, true, (normalize(G, F, H, I), eq(H, I, >=)))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("5"))),
	normalize(B, A, J, K),
	eq(J, K, >=).
'CHR=<_2'(A =< B, C, D, E) :-
	'CHR=<_2__31'(A =< B, C, D, E).
:- set_flag('CHR=<_2' / 4, leash, notrace).
:- current_macro('CHR=<_2' / 4, _8807, _8808, _8809) -> true ; define_macro('CHR=<_2' / 4, tr_chr / 2, [write]).
'CHR=<_2__31'(A, B, C, D) :-
	'CHR=<_2__32'(A, B, C, D).
:- set_flag('CHR=<_2__31' / 4, leash, notrace).
'CHR=<_2__32'(A =< B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A =< B], 'CHR=<_2'(A =< B, C, D, E))
	;
	    true
	).
:- set_flag('CHR=<_2__32' / 4, leash, notrace).
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
	coca(try_rule(E, A =:= B, anonymous("10"), F =:= G, replacement, (ground(F), ground(G)), (H is F - G, zero(H)))),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("10"))),
	I is A - B,
	zero(I).
'CHR=:=_2'(A =:= B, C, D, E) ?-
	coca(try_rule(E, A =:= B, anonymous("11"), F =:= G, replacement, (var(F), ground(G)), F is G)),
	no_delayed_goals((var(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("11"))),
	A is B.
'CHR=:=_2'(A =:= B, C, D, E) ?-
	coca(try_rule(E, A =:= B, anonymous("12"), F =:= G, replacement, (var(G), ground(F)), G is F)),
	no_delayed_goals((var(B), ground(A))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("12"))),
	B is A.
'CHR=:=_2'(A =:= B, C, D, E) ?-
	coca(try_rule(E, A =:= B, anonymous("13"), F =:= G, replacement, (free(F), var(G)), F = G)),
	no_delayed_goals((free(A), var(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("13"))),
	A = B.
'CHR=:=_2'(A =:= B, C, D, E) ?-
	coca(try_rule(E, A =:= B, anonymous("14"), F =:= G, replacement, (free(G), var(F)), G = F)),
	no_delayed_goals((free(B), var(A))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("14"))),
	B = A.
'CHR=:=_2'(A =:= B, C, D, E) ?-
	coca(try_rule(E, A =:= B, anonymous("15"), F =:= G, replacement, true, (normalize(F, G, H, I), eq(H, I, =:=)))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("15"))),
	normalize(A, B, J, K),
	eq(J, K, =:=).
'CHR=:=_2'(A =:= B, C, D, E) :-
	'CHR=:=_2__33'(A =:= B, C, D, E).
:- set_flag('CHR=:=_2' / 4, leash, notrace).
:- current_macro('CHR=:=_2' / 4, _10411, _10412, _10413) -> true ; define_macro('CHR=:=_2' / 4, tr_chr / 2, [write]).
'CHR=:=_2__33'(A, B, C, D) :-
	'CHR=:=_2__34'(A, B, C, D).
:- set_flag('CHR=:=_2__33' / 4, leash, notrace).
'CHR=:=_2__34'(A =:= B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A =:= B], 'CHR=:=_2'(A =:= B, C, D, E))
	;
	    true
	).
:- set_flag('CHR=:=_2__34' / 4, leash, notrace).
eq(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, eq(A, B, C))),
	'CHReq_3'(eq(A, B, C), E, F, D).



%%% Rules handling for eq / 3

'CHReq_3'(eq(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHReq_3'(eq(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHReq_3'(eq([], A, =:=), B, C, D) ?-
	coca(try_rule(D, eq([], A, =:=), anonymous("16"), eq([], E, =:=), replacement, true, zero(E))),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("16"))),
	zero(A).
'CHReq_3'(eq([], A, >=), B, C, D) ?-
	coca(try_rule(D, eq([], A, >=), anonymous("17"), eq([], E, >=), replacement, true, call_kernel(E >= 0))),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("17"))),
	call_kernel(A >= 0).
'CHReq_3'(eq([], A, >), B, C, D) ?-
	coca(try_rule(D, eq([], A, >), anonymous("18"), eq([], E, >), replacement, true, call_kernel(E > 0))),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("18"))),
	call_kernel(A > 0).
'CHReq_3'(eq([A * B], C, =:=), D, E, F) ?-
	coca(try_rule(F, eq([A * B], C, =:=), anonymous("19"), eq([G * H], I, =:=), replacement, (nonground(G), nonzero(H)), is_div(I, H, G))),
	no_delayed_goals((nonground(A), nonzero(B))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("19"))),
	is_div(C, B, A).
'CHReq_3'(eq(A, B, C), D, E, F) ?-
	coca(try_rule(F, eq(A, B, C), simplify, eq(G, H, I), replacement, simplifyable(J * K, G, L), (is_mul(J, K, M), N is M + H, eq(L, N, I)))),
	no_delayed_goals(simplifyable(O * P, A, Q)),
	!,
	'CHRkill'(D),
	coca(fired_rule(simplify)),
	is_mul(O, P, R),
	S is R + B,
	eq(Q, S, C).
'CHReq_3'(eq([A * B|C], D, E), F, G, H) ?-
	'CHRget_delayed_goals'(A, I),
	'CHReq_3__36'(I, [A], [J, K, L, M], N),
	coca(try_double(H, eq([A * B|C], D, E), N, eq([A * M|L], K, J), eq([O * P|Q], R, S), eq([O * T|U], V, W), replacement, (inequation(S), inequation(W), X is T / P, call_kernel(X < 0), Y is R * X, call_kernel(Y >= V), same_poly(Q, U, X)), (Z is Y - V, zero(Z), S = (>=), W = (>=), eq([O * P|Q], R, =:=)), opp_poly)),
	no_delayed_goals((inequation(E), inequation(J), A1 is M / B, call_kernel(A1 < 0), B1 is D * A1, call_kernel(B1 >= K), same_poly(C, L, A1))),
	!,
	'CHRkill'(F),
	coca(fired_rule(opp_poly)),
	C1 is B1 - K,
	zero(C1),
	[E, J] = [>=, >=],
	eq([A * B|C], D, =:=).
'CHReq_3'(eq([A * B|C], D, E), F, G, H) ?-
	'CHRget_delayed_goals'(A, I),
	'CHReq_3__37'(I, [A], [J, K, L, M], N),
	coca(try_double(H, eq([A * B|C], D, E), N, eq([A * M|L], K, J), eq([O * P|Q], R, S), eq([O * T|U], V, W), replacement, (inequation(W), inequation(S), X is P / T, call_kernel(X < 0), Y is V * X, call_kernel(Y >= R), same_poly(U, Q, X)), (Z is Y - R, zero(Z), W = (>=), S = (>=), eq([O * T|U], V, =:=)), opp_poly)),
	no_delayed_goals((inequation(J), inequation(E), A1 is B / M, call_kernel(A1 < 0), B1 is K * A1, call_kernel(B1 >= D), same_poly(L, C, A1))),
	!,
	'CHRkill'(F),
	coca(fired_rule(opp_poly)),
	C1 is B1 - D,
	zero(C1),
	[J, E] = [>=, >=],
	eq([A * M|L], K, =:=).
'CHReq_3'(eq([A * B|C], D, E), F, G, H) ?-
	'CHRget_delayed_goals'(A, I),
	'CHReq_3__38'(I, [A], [J, K, L, M], N),
	coca(try_double(H, eq([A * B|C], D, E), N, eq([A * M|L], K, J), eq([O * P|Q], R, S), eq([O * T|U], V, W), keep_second, (inequation(W), inequation(S), X is P / T, call_kernel(X > 0), Y is V * X, call_kernel(Y =< R), stronger(T, Y, W, P, R, S), same_poly(U, Q, X)), true, red_poly)),
	no_delayed_goals((inequation(J), inequation(E), Z is B / M, call_kernel(Z > 0), A1 is K * Z, call_kernel(A1 =< D), stronger(M, A1, J, B, D, E), same_poly(L, C, Z))),
	!,
	'CHRkill'(F),
	coca(fired_rule(red_poly)).
'CHReq_3'(eq(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(true, G),
	'CHReq_3__39'(G, [], [H, I, J, K], L),
	coca(try_double(F, eq(A, B, C), L, eq([K * J|I], H, =:=), eq(M, N, O), eq([P * Q|R], S, =:=), keep_second, (var(P), extract(P * T, M, U)), (is_div(T, Q, V), mult_const(eq0(S, R), V, W), add_eq0(eq0(N, U), W, eq0(X, Y)), sort1(Y, Z), eq(Z, X, O)), eliminate)),
	no_delayed_goals((var(K), extract(K * A1, A, B1))),
	!,
	'CHRkill'(D),
	coca(fired_rule(eliminate)),
	is_div(A1, J, C1),
	mult_const(eq0(H, I), C1, D1),
	add_eq0(eq0(B, B1), D1, eq0(E1, F1)),
	sort1(F1, G1),
	eq(G1, E1, C).
'CHReq_3'(eq(A, B, C), D, E, F) :-
	'CHReq_3__35'(eq(A, B, C), D, E, F).
'CHReq_3__36'(['CHReq_3'(eq([A * B|C], D, E), F, G, H)|I], [A], [J, K, L, M], N) ?-
	'CHRvar'(F),
	'CHRkill'(F),
	'CHR='([E, D, C, B], [J, K, L, M]),
	'CHR='(H, N).
'CHReq_3__36'([A|B], C, D, E) :-
	'CHReq_3__36'(B, C, D, E).
:- set_flag('CHReq_3__36' / 4, leash, notrace).
'CHReq_3__37'(['CHReq_3'(eq([A * B|C], D, E), F, G, H)|I], [A], [J, K, L, M], N) ?-
	'CHRvar'(F),
	'CHRkill'(F),
	'CHR='([E, D, C, B], [J, K, L, M]),
	'CHR='(H, N).
'CHReq_3__37'([A|B], C, D, E) :-
	'CHReq_3__37'(B, C, D, E).
:- set_flag('CHReq_3__37' / 4, leash, notrace).
'CHReq_3__38'(['CHReq_3'(eq([A * B|C], D, E), F, G, H)|I], [A], [J, K, L, M], N) ?-
	'CHRvar'(F),
	'CHR='([E, D, C, B], [J, K, L, M]),
	'CHR='(H, N).
'CHReq_3__38'([A|B], C, D, E) :-
	'CHReq_3__38'(B, C, D, E).
:- set_flag('CHReq_3__38' / 4, leash, notrace).
'CHReq_3__39'(['CHReq_3'(eq([A * B|C], D, =:=), E, F, G)|H], [], [I, J, K, L], M) ?-
	'CHRvar'(E),
	'CHR='([D, C, B, A], [I, J, K, L]),
	'CHR='(G, M).
'CHReq_3__39'([A|B], C, D, E) :-
	'CHReq_3__39'(B, C, D, E).
:- set_flag('CHReq_3__39' / 4, leash, notrace).
:- set_flag('CHReq_3' / 4, leash, notrace).
:- current_macro('CHReq_3' / 4, _14762, _14763, _14764) -> true ; define_macro('CHReq_3' / 4, tr_chr / 2, [write]).
'CHReq_3__35'(A, B, C, D) :-
	'CHReq_3__40'(A, B, C, D).
:- set_flag('CHReq_3__35' / 4, leash, notrace).
'CHReq_3__40'(eq([A * B|C], D, E), F, G, H) ?-
	'CHRvar'(F),
	!,
	'CHRget_delayed_goals'(A, I),
	'CHReq_3__40__41'(I, F, eq([A * B|C], D, E), G, H).
'CHReq_3__40'(eq(A, B, C), D, E, F) :-
	'CHReq_3__40__42'(eq(A, B, C), D, E, F).
:- set_flag('CHReq_3__40' / 4, leash, notrace).
'CHReq_3__40__41'(['CHReq_3'(eq([A * B|C], D, E), F, G, H)|I], J, eq([A * K|L], M, N), O, P) ?-
	'CHRvar'(F),
	coca(try_double(P, eq([A * K|L], M, N), H, eq([A * B|C], D, E), eq([Q * R|S], T, U), eq([Q * V|W], X, Y), keep_first, (inequation(U), inequation(Y), Z is V / R, call_kernel(Z > 0), A1 is T * Z, call_kernel(A1 =< X), stronger(R, A1, U, V, X, Y), same_poly(S, W, Z)), true, red_poly)),
	no_delayed_goals((inequation(N), inequation(E), B1 is B / K, call_kernel(B1 > 0), C1 is M * B1, call_kernel(C1 =< D), stronger(K, C1, N, B, D, E), same_poly(L, C, B1))),
	!,
	'CHRkill'(F),
	coca(fired_rule(red_poly)),
	'CHReq_3__40__41'(I, J, eq([A * K|L], M, N), O, P).
'CHReq_3__40__41'([A|B], C, D, E, F) :-
	'CHReq_3__40__41'(B, C, D, E, F).
'CHReq_3__40__41'([], A, B, C, D) :-
	'CHReq_3__40__42'(B, A, C, D).
:- set_flag('CHReq_3__40__41' / 5, leash, notrace).
'CHReq_3__40__42'(eq([A * B|C], D, =:=), E, F, G) ?-
	'CHRvar'(E),
	!,
	'CHRget_delayed_goals'(true, H),
	'CHReq_3__40__42__43'(H, E, eq([A * B|C], D, =:=), F, G).
'CHReq_3__40__42'(eq(A, B, C), D, E, F) :-
	'CHReq_3__40__42__44'(eq(A, B, C), D, E, F).
:- set_flag('CHReq_3__40__42' / 4, leash, notrace).
'CHReq_3__40__42__43'(['CHReq_3'(eq(A, B, C), D, E, F)|G], H, eq([I * J|K], L, =:=), M, N) ?-
	'CHRvar'(D),
	coca(try_double(N, eq([I * J|K], L, =:=), F, eq(A, B, C), eq([O * P|Q], R, =:=), eq(S, T, U), keep_first, (var(O), extract(O * V, S, W)), (is_div(V, P, X), mult_const(eq0(R, Q), X, Y), add_eq0(eq0(T, W), Y, eq0(Z, A1)), sort1(A1, B1), eq(B1, Z, U)), eliminate)),
	no_delayed_goals((var(I), extract(I * C1, A, D1))),
	!,
	'CHRkill'(D),
	coca(fired_rule(eliminate)),
	'CHReq_3__40__42__43'(G, H, eq([I * J|K], L, =:=), M, N),
	is_div(C1, J, E1),
	mult_const(eq0(L, K), E1, F1),
	add_eq0(eq0(B, D1), F1, eq0(G1, H1)),
	sort1(H1, I1),
	eq(I1, G1, C).
'CHReq_3__40__42__43'([A|B], C, D, E, F) :-
	'CHReq_3__40__42__43'(B, C, D, E, F).
'CHReq_3__40__42__43'([], A, B, C, D) :-
	'CHReq_3__40__42__44'(B, A, C, D).
:- set_flag('CHReq_3__40__42__43' / 5, leash, notrace).
'CHReq_3__40__42__44'(eq([A * B|C], D, E), F, G, H) ?-
	'CHRvar'(F),
	!,
	'CHRget_delayed_goals'(true, I),
	'CHReq_3__40__42__44__45'(I, F, eq([A * B|C], D, E), G, H).
'CHReq_3__40__42__44'(eq(A, B, C), D, E, F) :-
	'CHReq_3__40__42__44__46'(eq(A, B, C), D, E, F).
:- set_flag('CHReq_3__40__42__44' / 4, leash, notrace).
'CHReq_3__40__42__44__45'(['CHReq_3'(eq(A, B, C), D, E, F)|G], H, eq([I * J|K], L, M), N, O) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(propagate), H, D, N, E),
	coca(try_double(O, eq([I * J|K], L, M), F, eq(A, B, C), eq([P * Q|R], S, T), eq(U, V, W), augmentation, (inequation(T), var(P), extract(P * X, U, Y), Z is Q * X, call_kernel(Z < 0), combine(T, W, A1)), (is_div(X, Q, B1), mult_const(eq0(S, R), B1, C1), add_eq0(eq0(V, Y), C1, eq0(D1, E1)), sort1(E1, F1), eq(F1, D1, A1)), propagate)),
	no_delayed_goals((inequation(M), var(I), extract(I * G1, A, H1), I1 is J * G1, call_kernel(I1 < 0), combine(M, C, J1))),
	!,
	coca(fired_rule(propagate)),
	'CHReq_3__40__42__44__45'(G, H, eq([I * J|K], L, M), N, O),
	is_div(G1, J, K1),
	mult_const(eq0(L, K), K1, L1),
	add_eq0(eq0(B, H1), L1, eq0(M1, N1)),
	sort1(N1, O1),
	eq(O1, M1, J1).
'CHReq_3__40__42__44__45'([A|B], C, D, E, F) :-
	'CHReq_3__40__42__44__45'(B, C, D, E, F).
'CHReq_3__40__42__44__45'([], A, B, C, D) :-
	'CHReq_3__40__42__44__46'(B, A, C, D).
:- set_flag('CHReq_3__40__42__44__45' / 5, leash, notrace).
'CHReq_3__40__42__44__46'(eq(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(true, G),
	'CHReq_3__40__42__44__46__47'(G, D, eq(A, B, C), E, F).
'CHReq_3__40__42__44__46'(eq(A, B, C), D, E, F) :-
	'CHReq_3__40__42__44__46__48'(eq(A, B, C), D, E, F).
:- set_flag('CHReq_3__40__42__44__46' / 4, leash, notrace).
'CHReq_3__40__42__44__46__47'(['CHReq_3'(eq([A * B|C], D, E), F, G, H)|I], J, eq(K, L, M), N, O) ?-
	'CHRvar'(F),
	'CHRcheck_and_mark_applied'('21'(propagate), J, F, N, G),
	coca(try_double(O, eq(K, L, M), H, eq([A * B|C], D, E), eq(P, Q, R), eq([S * T|U], V, W), augmentation, (inequation(W), var(S), extract(S * X, P, Y), Z is T * X, call_kernel(Z < 0), combine(W, R, A1)), (is_div(X, T, B1), mult_const(eq0(V, U), B1, C1), add_eq0(eq0(Q, Y), C1, eq0(D1, E1)), sort1(E1, F1), eq(F1, D1, A1)), propagate)),
	no_delayed_goals((inequation(E), var(A), extract(A * G1, K, H1), I1 is B * G1, call_kernel(I1 < 0), combine(E, M, J1))),
	!,
	coca(fired_rule(propagate)),
	'CHReq_3__40__42__44__46__47'(I, J, eq(K, L, M), N, O),
	is_div(G1, B, K1),
	mult_const(eq0(D, C), K1, L1),
	add_eq0(eq0(L, H1), L1, eq0(M1, N1)),
	sort1(N1, O1),
	eq(O1, M1, J1).
'CHReq_3__40__42__44__46__47'([A|B], C, D, E, F) :-
	'CHReq_3__40__42__44__46__47'(B, C, D, E, F).
'CHReq_3__40__42__44__46__47'([], A, B, C, D) :-
	'CHReq_3__40__42__44__46__48'(B, A, C, D).
:- set_flag('CHReq_3__40__42__44__46__47' / 5, leash, notrace).
'CHReq_3__40__42__44__46__48'(eq(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, eq(A, B, C)], 'CHReq_3'(eq(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHReq_3__40__42__44__46__48' / 4, leash, notrace).
eqnonlin(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, eqnonlin(A, B))),
	'CHReqnonlin_2'(eqnonlin(A, B), D, E, C).



%%% Rules handling for eqnonlin / 2

'CHReqnonlin_2'(eqnonlin(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHReqnonlin_2'(eqnonlin(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHReqnonlin_2'(eqnonlin(A, B), C, D, E) ?-
	coca(try_rule(E, eqnonlin(A, B), anonymous("20"), eqnonlin(F, G), replacement, ground(G), (H is G, F =:= H))),
	no_delayed_goals(ground(B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("20"))),
	I is B,
	A =:= I.
'CHReqnonlin_2'(eqnonlin(A, B * C), D, E, F) ?-
	coca(try_rule(F, eqnonlin(A, B * C), anonymous("21"), eqnonlin(G, H * I), replacement, ground(H), (J is H, G =:= J * I))),
	no_delayed_goals(ground(B)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("21"))),
	K is B,
	A =:= K * C.
'CHReqnonlin_2'(eqnonlin(A, B * C), D, E, F) ?-
	coca(try_rule(F, eqnonlin(A, B * C), anonymous("22"), eqnonlin(G, H * I), replacement, ground(I), (J is I, G =:= J * H))),
	no_delayed_goals(ground(C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("22"))),
	K is C,
	A =:= K * B.
'CHReqnonlin_2'(eqnonlin(A, B), C, D, E) :-
	'CHReqnonlin_2__49'(eqnonlin(A, B), C, D, E).
:- set_flag('CHReqnonlin_2' / 4, leash, notrace).
:- current_macro('CHReqnonlin_2' / 4, _19038, _19039, _19040) -> true ; define_macro('CHReqnonlin_2' / 4, tr_chr / 2, [write]).
'CHReqnonlin_2__49'(A, B, C, D) :-
	'CHReqnonlin_2__50'(A, B, C, D).
:- set_flag('CHReqnonlin_2__49' / 4, leash, notrace).
'CHReqnonlin_2__50'(eqnonlin(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, eqnonlin(A, B)], 'CHReqnonlin_2'(eqnonlin(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHReqnonlin_2__50' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
