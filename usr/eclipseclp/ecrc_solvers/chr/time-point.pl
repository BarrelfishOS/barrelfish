
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
handler time - point.
:- op(700, xfy, =<+).
:- op(700, xfy, =<*).
start(_343) :- =<*(0, =<*(_343, 0)).
=<+(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, =<+(A, B))),
	'CHR=<+_2'(=<+(A, B), D, E, C).



%%% Rules handling for =<+ / 2

'CHR=<+_2'(=<+(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHR=<+_2'(=<+(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHR=<+_2'(=<+(A, =<+(B, C)), D, E, F) ?-
	coca(try_rule(F, =<+(A, =<+(B, C)), ground, =<+(G, =<+(H, I)), replacement, ground(G - H - I), (G =< H, H =< I))),
	no_delayed_goals(ground(A - B - C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(ground)),
	A =< B,
	B =< C.
'CHR=<+_2'(=<+(A, B), C, D, E) :-
	'CHR=<+_2__0'(=<+(A, B), C, D, E).
:- set_flag('CHR=<+_2' / 4, leash, notrace).
:- current_macro('CHR=<+_2' / 4, _2109, _2110, _2111) -> true ; define_macro('CHR=<+_2' / 4, tr_chr / 2, [write]).
'CHR=<+_2__0'(A, B, C, D) :-
	'CHR=<+_2__1'(A, B, C, D).
:- set_flag('CHR=<+_2__0' / 4, leash, notrace).
'CHR=<+_2__1'(=<+(A, =<+(B - C, D)), E, F, G) ?-
	'CHRvar'(E),
	!,
	'CHRget_delayed_goals'(B, H),
	'CHR=<+_2__1__2'(H, E, =<+(A, =<+(B - C, D)), F, G).
'CHR=<+_2__1'(=<+(A, B), C, D, E) :-
	'CHR=<+_2__1__3'(=<+(A, B), C, D, E).
:- set_flag('CHR=<+_2__1' / 4, leash, notrace).
'CHR=<+_2__1__2'(['CHR=<*_2'(=<*(A, =<*(B, C)), D, E, F)|G], H, =<+(I, =<+(B - J, K)), L, M) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(propagate_forward, H, D, L, E),
	coca(try_double(M, =<+(I, =<+(B - J, K)), F, =<*(A, =<*(B, C)), =<+(N, =<+(O - P, Q)), =<*(R, =<*(O, S)), augmentation, true, (T is R + N, U is S + Q, =<*(T, =<*(P, U))), propagate_forward)),
	!,
	coca(fired_rule(propagate_forward)),
	'CHR=<+_2__1__2'(G, H, =<+(I, =<+(B - J, K)), L, M),
	V is A + I,
	W is C + K,
	=<*(V, =<*(J, W)).
'CHR=<+_2__1__2'([A|B], C, D, E, F) :-
	'CHR=<+_2__1__2'(B, C, D, E, F).
'CHR=<+_2__1__2'([], A, B, C, D) :-
	'CHR=<+_2__1__3'(B, A, C, D).
:- set_flag('CHR=<+_2__1__2' / 5, leash, notrace).
'CHR=<+_2__1__3'(=<+(A, =<+(B - C, D)), E, F, G) ?-
	'CHRvar'(E),
	!,
	'CHRget_delayed_goals'(C, H),
	'CHR=<+_2__1__3__4'(H, E, =<+(A, =<+(B - C, D)), F, G).
'CHR=<+_2__1__3'(=<+(A, B), C, D, E) :-
	'CHR=<+_2__1__3__5'(=<+(A, B), C, D, E).
:- set_flag('CHR=<+_2__1__3' / 4, leash, notrace).
'CHR=<+_2__1__3__4'(['CHR=<*_2'(=<*(A, =<*(B, C)), D, E, F)|G], H, =<+(I, =<+(J - B, K)), L, M) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(propagate_backward, H, D, L, E),
	coca(try_double(M, =<+(I, =<+(J - B, K)), F, =<*(A, =<*(B, C)), =<+(N, =<+(O - P, Q)), =<*(R, =<*(P, S)), augmentation, true, (T is R - Q, U is S - N, =<*(T, =<*(O, U))), propagate_backward)),
	!,
	coca(fired_rule(propagate_backward)),
	'CHR=<+_2__1__3__4'(G, H, =<+(I, =<+(J - B, K)), L, M),
	V is A - K,
	W is C - I,
	=<*(V, =<*(J, W)).
'CHR=<+_2__1__3__4'([A|B], C, D, E, F) :-
	'CHR=<+_2__1__3__4'(B, C, D, E, F).
'CHR=<+_2__1__3__4'([], A, B, C, D) :-
	'CHR=<+_2__1__3__5'(B, A, C, D).
:- set_flag('CHR=<+_2__1__3__4' / 5, leash, notrace).
'CHR=<+_2__1__3__5'(=<+(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, =<+(A, B)], 'CHR=<+_2'(=<+(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHR=<+_2__1__3__5' / 4, leash, notrace).
=<*(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, =<*(A, B))),
	'CHR=<*_2'(=<*(A, B), D, E, C).



%%% Rules handling for =<* / 2

'CHR=<*_2'(=<*(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHR=<*_2'(=<*(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHR=<*_2'(=<*(A, =<*(B, C)), D, E, F) ?-
	coca(try_rule(F, =<*(A, =<*(B, C)), ground, =<*(G, =<*(H, I)), replacement, ground(G - H - I), (G =< H, H =< I))),
	no_delayed_goals(ground(A - B - C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(ground)),
	A =< B,
	B =< C.
'CHR=<*_2'(=<*(A, =<*(B, C)), D, E, F) ?-
	coca(try_rule(F, =<*(A, =<*(B, C)), inconsistent, =<*(G, =<*(H, I)), replacement, G > I, fail)),
	no_delayed_goals(A > C),
	!,
	'CHRkill'(D),
	coca(fired_rule(inconsistent)),
	fail.
'CHR=<*_2'(=<*(A, B), C, D, E) :-
	'CHR=<*_2__6'(=<*(A, B), C, D, E).
:- set_flag('CHR=<*_2' / 4, leash, notrace).
:- current_macro('CHR=<*_2' / 4, _3919, _3920, _3921) -> true ; define_macro('CHR=<*_2' / 4, tr_chr / 2, [write]).
'CHR=<*_2__6'(A, B, C, D) :-
	'CHR=<*_2__7'(A, B, C, D).
:- set_flag('CHR=<*_2__6' / 4, leash, notrace).
'CHR=<*_2__7'(=<*(A, =<*(B, C)), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR=<*_2__7__8'(G, D, =<*(A, =<*(B, C)), E, F).
'CHR=<*_2__7'(=<*(A, B), C, D, E) :-
	'CHR=<*_2__7__9'(=<*(A, B), C, D, E).
:- set_flag('CHR=<*_2__7' / 4, leash, notrace).
'CHR=<*_2__7__8'(['CHR=<*_2'(=<*(A, =<*(B, C)), D, E, F)|G], H, =<*(I, =<*(B, J)), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(intersect), H, D, K, E),
	coca(try_double(L, =<*(I, =<*(B, J)), F, =<*(A, =<*(B, C)), =<*(M, =<*(N, O)), =<*(P, =<*(N, Q)), augmentation, ((R is max(M, P), S is min(O, Q)), 'CHRkeep_heads_checking'(=<*(M, =<*(N, O)), T, =<*(P, =<*(N, Q)), U, =<*(R, =<*(N, S)), V)), 'CHRhead_not_kept'(V) -> =<*(R, =<*(N, S)) ; true, intersect)),
	no_delayed_goals(((W is max(I, A), X is min(J, C)), 'CHRkeep_heads_checking'(=<*(I, =<*(B, J)), H, =<*(A, =<*(B, C)), D, =<*(W, =<*(B, X)), Y))),
	!,
	coca(fired_rule(intersect)),
	'CHR=<*_2__7__8'(G, H, =<*(I, =<*(B, J)), K, L),
	(
	    'CHRhead_not_kept'(Y)
	->
	    =<*(W, =<*(B, X))
	;
	    true
	).
'CHR=<*_2__7__8'([A|B], C, D, E, F) :-
	'CHR=<*_2__7__8'(B, C, D, E, F).
'CHR=<*_2__7__8'([], A, B, C, D) :-
	'CHR=<*_2__7__9'(B, A, C, D).
:- set_flag('CHR=<*_2__7__8' / 5, leash, notrace).
'CHR=<*_2__7__9'(=<*(A, =<*(B, C)), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR=<*_2__7__9__10'(G, D, =<*(A, =<*(B, C)), E, F).
'CHR=<*_2__7__9'(=<*(A, B), C, D, E) :-
	'CHR=<*_2__7__9__11'(=<*(A, B), C, D, E).
:- set_flag('CHR=<*_2__7__9' / 4, leash, notrace).
'CHR=<*_2__7__9__10'(['CHR=<*_2'(=<*(A, =<*(B, C)), D, E, F)|G], H, =<*(I, =<*(B, J)), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('21'(intersect), H, D, K, E),
	coca(try_double(L, =<*(I, =<*(B, J)), F, =<*(A, =<*(B, C)), =<*(M, =<*(N, O)), =<*(P, =<*(N, Q)), augmentation, ((R is max(P, M), S is min(Q, O)), 'CHRkeep_heads_checking'(=<*(P, =<*(N, Q)), T, =<*(M, =<*(N, O)), U, =<*(R, =<*(N, S)), V)), 'CHRhead_not_kept'(V) -> =<*(R, =<*(N, S)) ; true, intersect)),
	no_delayed_goals(((W is max(A, I), X is min(C, J)), 'CHRkeep_heads_checking'(=<*(A, =<*(B, C)), D, =<*(I, =<*(B, J)), H, =<*(W, =<*(B, X)), Y))),
	!,
	coca(fired_rule(intersect)),
	'CHR=<*_2__7__9__10'(G, H, =<*(I, =<*(B, J)), K, L),
	(
	    'CHRhead_not_kept'(Y)
	->
	    =<*(W, =<*(B, X))
	;
	    true
	).
'CHR=<*_2__7__9__10'([A|B], C, D, E, F) :-
	'CHR=<*_2__7__9__10'(B, C, D, E, F).
'CHR=<*_2__7__9__10'([], A, B, C, D) :-
	'CHR=<*_2__7__9__11'(B, A, C, D).
:- set_flag('CHR=<*_2__7__9__10' / 5, leash, notrace).
'CHR=<*_2__7__9__11'(=<*(A, =<*(B, C)), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR=<*_2__7__9__11__12'(G, D, =<*(A, =<*(B, C)), E, F).
'CHR=<*_2__7__9__11'(=<*(A, B), C, D, E) :-
	'CHR=<*_2__7__9__11__13'(=<*(A, B), C, D, E).
:- set_flag('CHR=<*_2__7__9__11' / 4, leash, notrace).
'CHR=<*_2__7__9__11__12'(['CHR=<+_2'(=<+(A, =<+(B - C, D)), E, F, G)|H], I, =<*(J, =<*(B, K)), L, M) ?-
	'CHRvar'(E),
	'CHRcheck_and_mark_applied'(propagate_forward, I, E, L, F),
	coca(try_double(M, =<*(J, =<*(B, K)), G, =<+(A, =<+(B - C, D)), =<*(N, =<*(O, P)), =<+(Q, =<+(O - R, S)), augmentation, true, (T is N + Q, U is P + S, =<*(T, =<*(R, U))), propagate_forward)),
	!,
	coca(fired_rule(propagate_forward)),
	'CHR=<*_2__7__9__11__12'(H, I, =<*(J, =<*(B, K)), L, M),
	V is J + A,
	W is K + D,
	=<*(V, =<*(C, W)).
'CHR=<*_2__7__9__11__12'([A|B], C, D, E, F) :-
	'CHR=<*_2__7__9__11__12'(B, C, D, E, F).
'CHR=<*_2__7__9__11__12'([], A, B, C, D) :-
	'CHR=<*_2__7__9__11__13'(B, A, C, D).
:- set_flag('CHR=<*_2__7__9__11__12' / 5, leash, notrace).
'CHR=<*_2__7__9__11__13'(=<*(A, =<*(B, C)), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR=<*_2__7__9__11__13__14'(G, D, =<*(A, =<*(B, C)), E, F).
'CHR=<*_2__7__9__11__13'(=<*(A, B), C, D, E) :-
	'CHR=<*_2__7__9__11__13__15'(=<*(A, B), C, D, E).
:- set_flag('CHR=<*_2__7__9__11__13' / 4, leash, notrace).
'CHR=<*_2__7__9__11__13__14'(['CHR=<+_2'(=<+(A, =<+(B - C, D)), E, F, G)|H], I, =<*(J, =<*(C, K)), L, M) ?-
	'CHRvar'(E),
	'CHRcheck_and_mark_applied'(propagate_backward, I, E, L, F),
	coca(try_double(M, =<*(J, =<*(C, K)), G, =<+(A, =<+(B - C, D)), =<*(N, =<*(O, P)), =<+(Q, =<+(R - O, S)), augmentation, true, (T is N - S, U is P - Q, =<*(T, =<*(R, U))), propagate_backward)),
	!,
	coca(fired_rule(propagate_backward)),
	'CHR=<*_2__7__9__11__13__14'(H, I, =<*(J, =<*(C, K)), L, M),
	V is J - D,
	W is K - A,
	=<*(V, =<*(B, W)).
'CHR=<*_2__7__9__11__13__14'([A|B], C, D, E, F) :-
	'CHR=<*_2__7__9__11__13__14'(B, C, D, E, F).
'CHR=<*_2__7__9__11__13__14'([], A, B, C, D) :-
	'CHR=<*_2__7__9__11__13__15'(B, A, C, D).
:- set_flag('CHR=<*_2__7__9__11__13__14' / 5, leash, notrace).
'CHR=<*_2__7__9__11__13__15'(=<*(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, =<*(A, B)], 'CHR=<*_2'(=<*(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHR=<*_2__7__9__11__13__15' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
