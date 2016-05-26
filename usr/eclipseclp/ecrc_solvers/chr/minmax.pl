
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- op(700, xfx, lss).
:- op(700, xfx, grt).
:- op(700, xfx, neq).
:- op(700, xfx, geq).
:- op(700, xfx, leq).
geq(_490, _494) :- leq(_494, _490).
grt(_507, _511) :- lss(_511, _507).



%%% Callables for minimum / 3

'CHRlabel_with'(minimum(A, B, C), D, E) ?-
	coca(try_clause(E, minimum(A, B, C), minimum(F, G, H), true)),
	coca(clause_fired(E)),
	'CHR='(D, clause_minimum(A, B, C)).



%%% Callables for maximum / 3

'CHRlabel_with'(maximum(A, B, C), D, E) ?-
	coca(try_clause(E, maximum(A, B, C), maximum(F, G, H), true)),
	coca(clause_fired(E)),
	'CHR='(D, clause_maximum(A, B, C)).
leq(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, leq(A, B))),
	'CHRleq_2'(leq(A, B), D, E, C).



%%% Rules handling for leq / 2

'CHRleq_2'(leq(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRleq_2'(leq(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRleq_2'(leq(A, B), C, D, E) ?-
	coca(try_rule(E, leq(A, B), built_in, leq(F, G), replacement, (ground(F), ground(G)), F @=< G)),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(built_in)),
	A @=< B.
'CHRleq_2'(leq(A, A), B, C, D) ?-
	coca(try_rule(D, leq(A, A), reflexivity, leq(E, E), replacement, true, true)),
	!,
	'CHRkill'(B),
	coca(fired_rule(reflexivity)).
'CHRleq_2'(leq(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRleq_2__1'(F, [B, A], [], G),
	coca(try_double(E, leq(A, B), G, leq(B, A), leq(H, I), leq(I, H), replacement, true, H = I, antisymmetry)),
	!,
	'CHRkill'(C),
	coca(fired_rule(antisymmetry)),
	A = B.
'CHRleq_2'(leq(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRleq_2__2'(F, [B, A], [], G),
	coca(try_double(E, leq(A, B), G, leq(B, A), leq(H, I), leq(I, H), replacement, true, I = H, antisymmetry)),
	!,
	'CHRkill'(C),
	coca(fired_rule(antisymmetry)),
	B = A.
'CHRleq_2'(leq(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRleq_2__3'(F, [B, A], [], G),
	coca(try_double(E, leq(A, B), G, neq(A, B), leq(H, I), neq(H, I), replacement, true, lss(H, I), simplification)),
	!,
	'CHRkill'(C),
	coca(fired_rule(simplification)),
	lss(A, B).
'CHRleq_2'(leq(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRleq_2__4'(F, [B, A], [], G),
	coca(try_double(E, leq(A, B), G, neq(B, A), leq(H, I), neq(I, H), replacement, true, lss(H, I), simplification)),
	!,
	'CHRkill'(C),
	coca(fired_rule(simplification)),
	lss(A, B).
'CHRleq_2'(leq(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRleq_2__5'(F, [B, A], [], G),
	coca(try_double(E, leq(A, B), G, lss(A, B), leq(H, I), lss(H, I), keep_second, true, true, subsumption)),
	!,
	'CHRkill'(C),
	coca(fired_rule(subsumption)).
'CHRleq_2'(leq(A, B), C, D, E) :-
	'CHRleq_2__0'(leq(A, B), C, D, E).
'CHRleq_2__1'(['CHRleq_2'(leq(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRleq_2__1'([A|B], C, D, E) :-
	'CHRleq_2__1'(B, C, D, E).
:- set_flag('CHRleq_2__1' / 4, leash, notrace).
'CHRleq_2__2'(['CHRleq_2'(leq(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRleq_2__2'([A|B], C, D, E) :-
	'CHRleq_2__2'(B, C, D, E).
:- set_flag('CHRleq_2__2' / 4, leash, notrace).
'CHRleq_2__3'(['CHRneq_2'(neq(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRleq_2__3'([A|B], C, D, E) :-
	'CHRleq_2__3'(B, C, D, E).
:- set_flag('CHRleq_2__3' / 4, leash, notrace).
'CHRleq_2__4'(['CHRneq_2'(neq(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRleq_2__4'([A|B], C, D, E) :-
	'CHRleq_2__4'(B, C, D, E).
:- set_flag('CHRleq_2__4' / 4, leash, notrace).
'CHRleq_2__5'(['CHRlss_2'(lss(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRleq_2__5'([A|B], C, D, E) :-
	'CHRleq_2__5'(B, C, D, E).
:- set_flag('CHRleq_2__5' / 4, leash, notrace).
:- set_flag('CHRleq_2' / 4, leash, notrace).
:- current_macro('CHRleq_2' / 4, _11018, _11019, _11020) -> true ; define_macro('CHRleq_2' / 4, tr_chr / 2, [write]).
'CHRleq_2__0'(A, B, C, D) :-
	'CHRleq_2__6'(A, B, C, D).
:- set_flag('CHRleq_2__0' / 4, leash, notrace).
'CHRleq_2__6'(leq(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRleq_2__6__7'(F, C, leq(A, B), D, E).
'CHRleq_2__6'(leq(A, B), C, D, E) :-
	'CHRleq_2__6__8'(leq(A, B), C, D, E).
:- set_flag('CHRleq_2__6' / 4, leash, notrace).
'CHRleq_2__6__7'(['CHRminimum_3'(minimum(A, B, C), D, E, F)|G], H, leq(B, A), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, leq(B, A), F, minimum(A, B, C), leq(K, L), minimum(L, K, M), keep_first, true, K = M, min_leq)),
	!,
	'CHRkill'(D),
	coca(fired_rule(min_leq)),
	'CHRleq_2__6__7'(G, H, leq(B, A), I, J),
	B = C.
'CHRleq_2__6__7'([A|B], C, D, E, F) :-
	'CHRleq_2__6__7'(B, C, D, E, F).
'CHRleq_2__6__7'([], A, B, C, D) :-
	'CHRleq_2__6__8'(B, A, C, D).
:- set_flag('CHRleq_2__6__7' / 5, leash, notrace).
'CHRleq_2__6__8'(leq(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRleq_2__6__8__9'(F, C, leq(A, B), D, E).
'CHRleq_2__6__8'(leq(A, B), C, D, E) :-
	'CHRleq_2__6__8__10'(leq(A, B), C, D, E).
:- set_flag('CHRleq_2__6__8' / 4, leash, notrace).
'CHRleq_2__6__8__9'(['CHRminimum_3'(minimum(A, B, C), D, E, F)|G], H, leq(A, B), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, leq(A, B), F, minimum(A, B, C), leq(K, L), minimum(K, L, M), keep_first, true, K = M, min_leq)),
	!,
	'CHRkill'(D),
	coca(fired_rule(min_leq)),
	'CHRleq_2__6__8__9'(G, H, leq(A, B), I, J),
	A = C.
'CHRleq_2__6__8__9'([A|B], C, D, E, F) :-
	'CHRleq_2__6__8__9'(B, C, D, E, F).
'CHRleq_2__6__8__9'([], A, B, C, D) :-
	'CHRleq_2__6__8__10'(B, A, C, D).
:- set_flag('CHRleq_2__6__8__9' / 5, leash, notrace).
'CHRleq_2__6__8__10'(leq(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRleq_2__6__8__10__11'(F, C, leq(A, B), D, E).
'CHRleq_2__6__8__10'(leq(A, B), C, D, E) :-
	'CHRleq_2__6__8__10__12'(leq(A, B), C, D, E).
:- set_flag('CHRleq_2__6__8__10' / 4, leash, notrace).
'CHRleq_2__6__8__10__11'(['CHRmaximum_3'(maximum(A, B, C), D, E, F)|G], H, leq(B, A), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, leq(B, A), F, maximum(A, B, C), leq(K, L), maximum(L, K, M), keep_first, true, L = M, max_leq)),
	!,
	'CHRkill'(D),
	coca(fired_rule(max_leq)),
	'CHRleq_2__6__8__10__11'(G, H, leq(B, A), I, J),
	A = C.
'CHRleq_2__6__8__10__11'([A|B], C, D, E, F) :-
	'CHRleq_2__6__8__10__11'(B, C, D, E, F).
'CHRleq_2__6__8__10__11'([], A, B, C, D) :-
	'CHRleq_2__6__8__10__12'(B, A, C, D).
:- set_flag('CHRleq_2__6__8__10__11' / 5, leash, notrace).
'CHRleq_2__6__8__10__12'(leq(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRleq_2__6__8__10__12__13'(F, C, leq(A, B), D, E).
'CHRleq_2__6__8__10__12'(leq(A, B), C, D, E) :-
	'CHRleq_2__6__8__10__12__14'(leq(A, B), C, D, E).
:- set_flag('CHRleq_2__6__8__10__12' / 4, leash, notrace).
'CHRleq_2__6__8__10__12__13'(['CHRmaximum_3'(maximum(A, B, C), D, E, F)|G], H, leq(A, B), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, leq(A, B), F, maximum(A, B, C), leq(K, L), maximum(K, L, M), keep_first, true, L = M, max_leq)),
	!,
	'CHRkill'(D),
	coca(fired_rule(max_leq)),
	'CHRleq_2__6__8__10__12__13'(G, H, leq(A, B), I, J),
	B = C.
'CHRleq_2__6__8__10__12__13'([A|B], C, D, E, F) :-
	'CHRleq_2__6__8__10__12__13'(B, C, D, E, F).
'CHRleq_2__6__8__10__12__13'([], A, B, C, D) :-
	'CHRleq_2__6__8__10__12__14'(B, A, C, D).
:- set_flag('CHRleq_2__6__8__10__12__13' / 5, leash, notrace).
'CHRleq_2__6__8__10__12__14'(leq(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRleq_2__6__8__10__12__14__15'(F, C, leq(A, B), D, E).
'CHRleq_2__6__8__10__12__14'(leq(A, B), C, D, E) :-
	'CHRleq_2__6__8__10__12__14__16'(leq(A, B), C, D, E).
:- set_flag('CHRleq_2__6__8__10__12__14' / 4, leash, notrace).
'CHRleq_2__6__8__10__12__14__15'(['CHRleq_2'(leq(A, B), C, D, E)|F], G, leq(H, A), I, J) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'('12'(transitivity), G, C, I, D),
	coca(try_double(J, leq(H, A), E, leq(A, B), leq(K, L), leq(L, M), augmentation, (K \== L, L \== M, K \== M), leq(K, M), transitivity)),
	no_delayed_goals((H \== A, A \== B, H \== B)),
	!,
	coca(fired_rule(transitivity)),
	'CHRleq_2__6__8__10__12__14__15'(F, G, leq(H, A), I, J),
	leq(H, B).
'CHRleq_2__6__8__10__12__14__15'([A|B], C, D, E, F) :-
	'CHRleq_2__6__8__10__12__14__15'(B, C, D, E, F).
'CHRleq_2__6__8__10__12__14__15'([], A, B, C, D) :-
	'CHRleq_2__6__8__10__12__14__16'(B, A, C, D).
:- set_flag('CHRleq_2__6__8__10__12__14__15' / 5, leash, notrace).
'CHRleq_2__6__8__10__12__14__16'(leq(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHRleq_2__6__8__10__12__14__16__17'(F, C, leq(A, B), D, E).
'CHRleq_2__6__8__10__12__14__16'(leq(A, B), C, D, E) :-
	'CHRleq_2__6__8__10__12__14__16__18'(leq(A, B), C, D, E).
:- set_flag('CHRleq_2__6__8__10__12__14__16' / 4, leash, notrace).
'CHRleq_2__6__8__10__12__14__16__17'(['CHRleq_2'(leq(A, B), C, D, E)|F], G, leq(B, H), I, J) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'('21'(transitivity), G, C, I, D),
	coca(try_double(J, leq(B, H), E, leq(A, B), leq(K, L), leq(M, K), augmentation, (M \== K, K \== L, M \== L), leq(M, L), transitivity)),
	no_delayed_goals((A \== B, B \== H, A \== H)),
	!,
	coca(fired_rule(transitivity)),
	'CHRleq_2__6__8__10__12__14__16__17'(F, G, leq(B, H), I, J),
	leq(A, H).
'CHRleq_2__6__8__10__12__14__16__17'([A|B], C, D, E, F) :-
	'CHRleq_2__6__8__10__12__14__16__17'(B, C, D, E, F).
'CHRleq_2__6__8__10__12__14__16__17'([], A, B, C, D) :-
	'CHRleq_2__6__8__10__12__14__16__18'(B, A, C, D).
:- set_flag('CHRleq_2__6__8__10__12__14__16__17' / 5, leash, notrace).
'CHRleq_2__6__8__10__12__14__16__18'(leq(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRleq_2__6__8__10__12__14__16__18__19'(F, C, leq(A, B), D, E).
'CHRleq_2__6__8__10__12__14__16__18'(leq(A, B), C, D, E) :-
	'CHRleq_2__6__8__10__12__14__16__18__20'(leq(A, B), C, D, E).
:- set_flag('CHRleq_2__6__8__10__12__14__16__18' / 4, leash, notrace).
'CHRleq_2__6__8__10__12__14__16__18__19'(['CHRlss_2'(lss(A, B), C, D, E)|F], G, leq(H, A), I, J) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(transitivity, G, C, I, D),
	coca(try_double(J, leq(H, A), E, lss(A, B), leq(K, L), lss(L, M), augmentation, (K \== L, L \== M), lss(K, M), transitivity)),
	no_delayed_goals((H \== A, A \== B)),
	!,
	coca(fired_rule(transitivity)),
	'CHRleq_2__6__8__10__12__14__16__18__19'(F, G, leq(H, A), I, J),
	lss(H, B).
'CHRleq_2__6__8__10__12__14__16__18__19'([A|B], C, D, E, F) :-
	'CHRleq_2__6__8__10__12__14__16__18__19'(B, C, D, E, F).
'CHRleq_2__6__8__10__12__14__16__18__19'([], A, B, C, D) :-
	'CHRleq_2__6__8__10__12__14__16__18__20'(B, A, C, D).
:- set_flag('CHRleq_2__6__8__10__12__14__16__18__19' / 5, leash, notrace).
'CHRleq_2__6__8__10__12__14__16__18__20'(leq(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHRleq_2__6__8__10__12__14__16__18__20__21'(F, C, leq(A, B), D, E).
'CHRleq_2__6__8__10__12__14__16__18__20'(leq(A, B), C, D, E) :-
	'CHRleq_2__6__8__10__12__14__16__18__20__22'(leq(A, B), C, D, E).
:- set_flag('CHRleq_2__6__8__10__12__14__16__18__20' / 4, leash, notrace).
'CHRleq_2__6__8__10__12__14__16__18__20__21'(['CHRlss_2'(lss(A, B), C, D, E)|F], G, leq(B, H), I, J) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(transitivity, G, C, I, D),
	coca(try_double(J, leq(B, H), E, lss(A, B), leq(K, L), lss(M, K), augmentation, (M \== K, K \== L), lss(M, L), transitivity)),
	no_delayed_goals((A \== B, B \== H)),
	!,
	coca(fired_rule(transitivity)),
	'CHRleq_2__6__8__10__12__14__16__18__20__21'(F, G, leq(B, H), I, J),
	lss(A, H).
'CHRleq_2__6__8__10__12__14__16__18__20__21'([A|B], C, D, E, F) :-
	'CHRleq_2__6__8__10__12__14__16__18__20__21'(B, C, D, E, F).
'CHRleq_2__6__8__10__12__14__16__18__20__21'([], A, B, C, D) :-
	'CHRleq_2__6__8__10__12__14__16__18__20__22'(B, A, C, D).
:- set_flag('CHRleq_2__6__8__10__12__14__16__18__20__21' / 5, leash, notrace).
'CHRleq_2__6__8__10__12__14__16__18__20__22'(leq(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, leq(A, B)], 'CHRleq_2'(leq(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRleq_2__6__8__10__12__14__16__18__20__22' / 4, leash, notrace).
lss(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, lss(A, B))),
	'CHRlss_2'(lss(A, B), D, E, C).



%%% Rules handling for lss / 2

'CHRlss_2'(lss(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRlss_2'(lss(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRlss_2'(lss(A, B), C, D, E) ?-
	coca(try_rule(E, lss(A, B), built_in, lss(F, G), replacement, (ground(F), ground(G)), F @< G)),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(built_in)),
	A @< B.
'CHRlss_2'(lss(A, A), B, C, D) ?-
	coca(try_rule(D, lss(A, A), irreflexivity, lss(E, E), replacement, true, fail)),
	!,
	'CHRkill'(B),
	coca(fired_rule(irreflexivity)),
	fail.
'CHRlss_2'(lss(A, B), C, D, E) :-
	'CHRlss_2__23'(lss(A, B), C, D, E).
:- set_flag('CHRlss_2' / 4, leash, notrace).
:- current_macro('CHRlss_2' / 4, _15495, _15496, _15497) -> true ; define_macro('CHRlss_2' / 4, tr_chr / 2, [write]).
'CHRlss_2__23'(A, B, C, D) :-
	'CHRlss_2__24'(A, B, C, D).
:- set_flag('CHRlss_2__23' / 4, leash, notrace).
'CHRlss_2__24'(lss(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRlss_2__24__25'(F, C, lss(A, B), D, E).
'CHRlss_2__24'(lss(A, B), C, D, E) :-
	'CHRlss_2__24__26'(lss(A, B), C, D, E).
:- set_flag('CHRlss_2__24' / 4, leash, notrace).
'CHRlss_2__24__25'(['CHRleq_2'(leq(A, B), C, D, E)|F], G, lss(A, B), H, I) ?-
	'CHRvar'(C),
	coca(try_double(I, lss(A, B), E, leq(A, B), lss(J, K), leq(J, K), keep_first, true, true, subsumption)),
	!,
	'CHRkill'(C),
	coca(fired_rule(subsumption)),
	'CHRlss_2__24__25'(F, G, lss(A, B), H, I).
'CHRlss_2__24__25'([A|B], C, D, E, F) :-
	'CHRlss_2__24__25'(B, C, D, E, F).
'CHRlss_2__24__25'([], A, B, C, D) :-
	'CHRlss_2__24__26'(B, A, C, D).
:- set_flag('CHRlss_2__24__25' / 5, leash, notrace).
'CHRlss_2__24__26'(lss(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRlss_2__24__26__27'(F, C, lss(A, B), D, E).
'CHRlss_2__24__26'(lss(A, B), C, D, E) :-
	'CHRlss_2__24__26__28'(lss(A, B), C, D, E).
:- set_flag('CHRlss_2__24__26' / 4, leash, notrace).
'CHRlss_2__24__26__27'(['CHRneq_2'(neq(A, B), C, D, E)|F], G, lss(A, B), H, I) ?-
	'CHRvar'(C),
	coca(try_double(I, lss(A, B), E, neq(A, B), lss(J, K), neq(J, K), keep_first, true, true, subsumption)),
	!,
	'CHRkill'(C),
	coca(fired_rule(subsumption)),
	'CHRlss_2__24__26__27'(F, G, lss(A, B), H, I).
'CHRlss_2__24__26__27'([A|B], C, D, E, F) :-
	'CHRlss_2__24__26__27'(B, C, D, E, F).
'CHRlss_2__24__26__27'([], A, B, C, D) :-
	'CHRlss_2__24__26__28'(B, A, C, D).
:- set_flag('CHRlss_2__24__26__27' / 5, leash, notrace).
'CHRlss_2__24__26__28'(lss(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRlss_2__24__26__28__29'(F, C, lss(A, B), D, E).
'CHRlss_2__24__26__28'(lss(A, B), C, D, E) :-
	'CHRlss_2__24__26__28__30'(lss(A, B), C, D, E).
:- set_flag('CHRlss_2__24__26__28' / 4, leash, notrace).
'CHRlss_2__24__26__28__29'(['CHRneq_2'(neq(A, B), C, D, E)|F], G, lss(B, A), H, I) ?-
	'CHRvar'(C),
	coca(try_double(I, lss(B, A), E, neq(A, B), lss(J, K), neq(K, J), keep_first, true, true, subsumption)),
	!,
	'CHRkill'(C),
	coca(fired_rule(subsumption)),
	'CHRlss_2__24__26__28__29'(F, G, lss(B, A), H, I).
'CHRlss_2__24__26__28__29'([A|B], C, D, E, F) :-
	'CHRlss_2__24__26__28__29'(B, C, D, E, F).
'CHRlss_2__24__26__28__29'([], A, B, C, D) :-
	'CHRlss_2__24__26__28__30'(B, A, C, D).
:- set_flag('CHRlss_2__24__26__28__29' / 5, leash, notrace).
'CHRlss_2__24__26__28__30'(lss(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRlss_2__24__26__28__30__31'(F, C, lss(A, B), D, E).
'CHRlss_2__24__26__28__30'(lss(A, B), C, D, E) :-
	'CHRlss_2__24__26__28__30__32'(lss(A, B), C, D, E).
:- set_flag('CHRlss_2__24__26__28__30' / 4, leash, notrace).
'CHRlss_2__24__26__28__30__31'(['CHRminimum_3'(minimum(A, B, C), D, E, F)|G], H, lss(C, A), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, lss(C, A), F, minimum(A, B, C), lss(K, L), minimum(L, M, K), keep_first, true, M = K, min_lss)),
	!,
	'CHRkill'(D),
	coca(fired_rule(min_lss)),
	'CHRlss_2__24__26__28__30__31'(G, H, lss(C, A), I, J),
	B = C.
'CHRlss_2__24__26__28__30__31'([A|B], C, D, E, F) :-
	'CHRlss_2__24__26__28__30__31'(B, C, D, E, F).
'CHRlss_2__24__26__28__30__31'([], A, B, C, D) :-
	'CHRlss_2__24__26__28__30__32'(B, A, C, D).
:- set_flag('CHRlss_2__24__26__28__30__31' / 5, leash, notrace).
'CHRlss_2__24__26__28__30__32'(lss(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRlss_2__24__26__28__30__32__33'(F, C, lss(A, B), D, E).
'CHRlss_2__24__26__28__30__32'(lss(A, B), C, D, E) :-
	'CHRlss_2__24__26__28__30__32__34'(lss(A, B), C, D, E).
:- set_flag('CHRlss_2__24__26__28__30__32' / 4, leash, notrace).
'CHRlss_2__24__26__28__30__32__33'(['CHRminimum_3'(minimum(A, B, C), D, E, F)|G], H, lss(C, B), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, lss(C, B), F, minimum(A, B, C), lss(K, L), minimum(M, L, K), keep_first, true, M = K, min_lss)),
	!,
	'CHRkill'(D),
	coca(fired_rule(min_lss)),
	'CHRlss_2__24__26__28__30__32__33'(G, H, lss(C, B), I, J),
	A = C.
'CHRlss_2__24__26__28__30__32__33'([A|B], C, D, E, F) :-
	'CHRlss_2__24__26__28__30__32__33'(B, C, D, E, F).
'CHRlss_2__24__26__28__30__32__33'([], A, B, C, D) :-
	'CHRlss_2__24__26__28__30__32__34'(B, A, C, D).
:- set_flag('CHRlss_2__24__26__28__30__32__33' / 5, leash, notrace).
'CHRlss_2__24__26__28__30__32__34'(lss(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRlss_2__24__26__28__30__32__34__35'(F, C, lss(A, B), D, E).
'CHRlss_2__24__26__28__30__32__34'(lss(A, B), C, D, E) :-
	'CHRlss_2__24__26__28__30__32__34__36'(lss(A, B), C, D, E).
:- set_flag('CHRlss_2__24__26__28__30__32__34' / 4, leash, notrace).
'CHRlss_2__24__26__28__30__32__34__35'(['CHRmaximum_3'(maximum(A, B, C), D, E, F)|G], H, lss(A, C), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, lss(A, C), F, maximum(A, B, C), lss(K, L), maximum(K, M, L), keep_first, true, M = L, max_lss)),
	!,
	'CHRkill'(D),
	coca(fired_rule(max_lss)),
	'CHRlss_2__24__26__28__30__32__34__35'(G, H, lss(A, C), I, J),
	B = C.
'CHRlss_2__24__26__28__30__32__34__35'([A|B], C, D, E, F) :-
	'CHRlss_2__24__26__28__30__32__34__35'(B, C, D, E, F).
'CHRlss_2__24__26__28__30__32__34__35'([], A, B, C, D) :-
	'CHRlss_2__24__26__28__30__32__34__36'(B, A, C, D).
:- set_flag('CHRlss_2__24__26__28__30__32__34__35' / 5, leash, notrace).
'CHRlss_2__24__26__28__30__32__34__36'(lss(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRlss_2__24__26__28__30__32__34__36__37'(F, C, lss(A, B), D, E).
'CHRlss_2__24__26__28__30__32__34__36'(lss(A, B), C, D, E) :-
	'CHRlss_2__24__26__28__30__32__34__36__38'(lss(A, B), C, D, E).
:- set_flag('CHRlss_2__24__26__28__30__32__34__36' / 4, leash, notrace).
'CHRlss_2__24__26__28__30__32__34__36__37'(['CHRmaximum_3'(maximum(A, B, C), D, E, F)|G], H, lss(B, C), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, lss(B, C), F, maximum(A, B, C), lss(K, L), maximum(M, K, L), keep_first, true, M = L, max_lss)),
	!,
	'CHRkill'(D),
	coca(fired_rule(max_lss)),
	'CHRlss_2__24__26__28__30__32__34__36__37'(G, H, lss(B, C), I, J),
	A = C.
'CHRlss_2__24__26__28__30__32__34__36__37'([A|B], C, D, E, F) :-
	'CHRlss_2__24__26__28__30__32__34__36__37'(B, C, D, E, F).
'CHRlss_2__24__26__28__30__32__34__36__37'([], A, B, C, D) :-
	'CHRlss_2__24__26__28__30__32__34__36__38'(B, A, C, D).
:- set_flag('CHRlss_2__24__26__28__30__32__34__36__37' / 5, leash, notrace).
'CHRlss_2__24__26__28__30__32__34__36__38'(lss(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRlss_2__24__26__28__30__32__34__36__38__39'(F, C, lss(A, B), D, E).
'CHRlss_2__24__26__28__30__32__34__36__38'(lss(A, B), C, D, E) :-
	'CHRlss_2__24__26__28__30__32__34__36__38__40'(lss(A, B), C, D, E).
:- set_flag('CHRlss_2__24__26__28__30__32__34__36__38' / 4, leash, notrace).
'CHRlss_2__24__26__28__30__32__34__36__38__39'(['CHRlss_2'(lss(A, B), C, D, E)|F], G, lss(H, A), I, J) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'('12'(transitivity), G, C, I, D),
	coca(try_double(J, lss(H, A), E, lss(A, B), lss(K, L), lss(L, M), augmentation, (K \== L, L \== M), lss(K, M), transitivity)),
	no_delayed_goals((H \== A, A \== B)),
	!,
	coca(fired_rule(transitivity)),
	'CHRlss_2__24__26__28__30__32__34__36__38__39'(F, G, lss(H, A), I, J),
	lss(H, B).
'CHRlss_2__24__26__28__30__32__34__36__38__39'([A|B], C, D, E, F) :-
	'CHRlss_2__24__26__28__30__32__34__36__38__39'(B, C, D, E, F).
'CHRlss_2__24__26__28__30__32__34__36__38__39'([], A, B, C, D) :-
	'CHRlss_2__24__26__28__30__32__34__36__38__40'(B, A, C, D).
:- set_flag('CHRlss_2__24__26__28__30__32__34__36__38__39' / 5, leash, notrace).
'CHRlss_2__24__26__28__30__32__34__36__38__40'(lss(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHRlss_2__24__26__28__30__32__34__36__38__40__41'(F, C, lss(A, B), D, E).
'CHRlss_2__24__26__28__30__32__34__36__38__40'(lss(A, B), C, D, E) :-
	'CHRlss_2__24__26__28__30__32__34__36__38__40__42'(lss(A, B), C, D, E).
:- set_flag('CHRlss_2__24__26__28__30__32__34__36__38__40' / 4, leash, notrace).
'CHRlss_2__24__26__28__30__32__34__36__38__40__41'(['CHRlss_2'(lss(A, B), C, D, E)|F], G, lss(B, H), I, J) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'('21'(transitivity), G, C, I, D),
	coca(try_double(J, lss(B, H), E, lss(A, B), lss(K, L), lss(M, K), augmentation, (M \== K, K \== L), lss(M, L), transitivity)),
	no_delayed_goals((A \== B, B \== H)),
	!,
	coca(fired_rule(transitivity)),
	'CHRlss_2__24__26__28__30__32__34__36__38__40__41'(F, G, lss(B, H), I, J),
	lss(A, H).
'CHRlss_2__24__26__28__30__32__34__36__38__40__41'([A|B], C, D, E, F) :-
	'CHRlss_2__24__26__28__30__32__34__36__38__40__41'(B, C, D, E, F).
'CHRlss_2__24__26__28__30__32__34__36__38__40__41'([], A, B, C, D) :-
	'CHRlss_2__24__26__28__30__32__34__36__38__40__42'(B, A, C, D).
:- set_flag('CHRlss_2__24__26__28__30__32__34__36__38__40__41' / 5, leash, notrace).
'CHRlss_2__24__26__28__30__32__34__36__38__40__42'(lss(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHRlss_2__24__26__28__30__32__34__36__38__40__42__43'(F, C, lss(A, B), D, E).
'CHRlss_2__24__26__28__30__32__34__36__38__40__42'(lss(A, B), C, D, E) :-
	'CHRlss_2__24__26__28__30__32__34__36__38__40__42__44'(lss(A, B), C, D, E).
:- set_flag('CHRlss_2__24__26__28__30__32__34__36__38__40__42' / 4, leash, notrace).
'CHRlss_2__24__26__28__30__32__34__36__38__40__42__43'(['CHRleq_2'(leq(A, B), C, D, E)|F], G, lss(B, H), I, J) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(transitivity, G, C, I, D),
	coca(try_double(J, lss(B, H), E, leq(A, B), lss(K, L), leq(M, K), augmentation, (M \== K, K \== L), lss(M, L), transitivity)),
	no_delayed_goals((A \== B, B \== H)),
	!,
	coca(fired_rule(transitivity)),
	'CHRlss_2__24__26__28__30__32__34__36__38__40__42__43'(F, G, lss(B, H), I, J),
	lss(A, H).
'CHRlss_2__24__26__28__30__32__34__36__38__40__42__43'([A|B], C, D, E, F) :-
	'CHRlss_2__24__26__28__30__32__34__36__38__40__42__43'(B, C, D, E, F).
'CHRlss_2__24__26__28__30__32__34__36__38__40__42__43'([], A, B, C, D) :-
	'CHRlss_2__24__26__28__30__32__34__36__38__40__42__44'(B, A, C, D).
:- set_flag('CHRlss_2__24__26__28__30__32__34__36__38__40__42__43' / 5, leash, notrace).
'CHRlss_2__24__26__28__30__32__34__36__38__40__42__44'(lss(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRlss_2__24__26__28__30__32__34__36__38__40__42__44__45'(F, C, lss(A, B), D, E).
'CHRlss_2__24__26__28__30__32__34__36__38__40__42__44'(lss(A, B), C, D, E) :-
	'CHRlss_2__24__26__28__30__32__34__36__38__40__42__44__46'(lss(A, B), C, D, E).
:- set_flag('CHRlss_2__24__26__28__30__32__34__36__38__40__42__44' / 4, leash, notrace).
'CHRlss_2__24__26__28__30__32__34__36__38__40__42__44__45'(['CHRleq_2'(leq(A, B), C, D, E)|F], G, lss(H, A), I, J) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(transitivity, G, C, I, D),
	coca(try_double(J, lss(H, A), E, leq(A, B), lss(K, L), leq(L, M), augmentation, (K \== L, L \== M), lss(K, M), transitivity)),
	no_delayed_goals((H \== A, A \== B)),
	!,
	coca(fired_rule(transitivity)),
	'CHRlss_2__24__26__28__30__32__34__36__38__40__42__44__45'(F, G, lss(H, A), I, J),
	lss(H, B).
'CHRlss_2__24__26__28__30__32__34__36__38__40__42__44__45'([A|B], C, D, E, F) :-
	'CHRlss_2__24__26__28__30__32__34__36__38__40__42__44__45'(B, C, D, E, F).
'CHRlss_2__24__26__28__30__32__34__36__38__40__42__44__45'([], A, B, C, D) :-
	'CHRlss_2__24__26__28__30__32__34__36__38__40__42__44__46'(B, A, C, D).
:- set_flag('CHRlss_2__24__26__28__30__32__34__36__38__40__42__44__45' / 5, leash, notrace).
'CHRlss_2__24__26__28__30__32__34__36__38__40__42__44__46'(lss(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, lss(A, B)], 'CHRlss_2'(lss(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRlss_2__24__26__28__30__32__34__36__38__40__42__44__46' / 4, leash, notrace).
neq(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, neq(A, B))),
	'CHRneq_2'(neq(A, B), D, E, C).



%%% Rules handling for neq / 2

'CHRneq_2'(neq(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRneq_2'(neq(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRneq_2'(neq(A, B), C, D, E) ?-
	coca(try_rule(E, neq(A, B), built_in, neq(F, G), replacement, F ~= G, true)),
	no_delayed_goals(A ~= B),
	!,
	'CHRkill'(C),
	coca(fired_rule(built_in)).
'CHRneq_2'(neq(A, A), B, C, D) ?-
	coca(try_rule(D, neq(A, A), irreflexivity, neq(E, E), replacement, true, fail)),
	!,
	'CHRkill'(B),
	coca(fired_rule(irreflexivity)),
	fail.
'CHRneq_2'(neq(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneq_2__48'(F, [B, A], [], G),
	coca(try_double(E, neq(A, B), G, leq(A, B), neq(H, I), leq(H, I), replacement, true, lss(H, I), simplification)),
	!,
	'CHRkill'(C),
	coca(fired_rule(simplification)),
	lss(A, B).
'CHRneq_2'(neq(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneq_2__49'(F, [B, A], [], G),
	coca(try_double(E, neq(A, B), G, leq(B, A), neq(H, I), leq(I, H), replacement, true, lss(I, H), simplification)),
	!,
	'CHRkill'(C),
	coca(fired_rule(simplification)),
	lss(B, A).
'CHRneq_2'(neq(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneq_2__50'(F, [B, A], [], G),
	coca(try_double(E, neq(A, B), G, neq(B, A), neq(H, I), neq(I, H), keep_second, true, true, subsumption)),
	!,
	'CHRkill'(C),
	coca(fired_rule(subsumption)).
'CHRneq_2'(neq(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneq_2__51'(F, [B, A], [], G),
	coca(try_double(E, neq(A, B), G, lss(A, B), neq(H, I), lss(H, I), keep_second, true, true, subsumption)),
	!,
	'CHRkill'(C),
	coca(fired_rule(subsumption)).
'CHRneq_2'(neq(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneq_2__52'(F, [B, A], [], G),
	coca(try_double(E, neq(A, B), G, lss(B, A), neq(H, I), lss(I, H), keep_second, true, true, subsumption)),
	!,
	'CHRkill'(C),
	coca(fired_rule(subsumption)).
'CHRneq_2'(neq(A, B), C, D, E) :-
	'CHRneq_2__47'(neq(A, B), C, D, E).
'CHRneq_2__48'(['CHRleq_2'(leq(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRneq_2__48'([A|B], C, D, E) :-
	'CHRneq_2__48'(B, C, D, E).
:- set_flag('CHRneq_2__48' / 4, leash, notrace).
'CHRneq_2__49'(['CHRleq_2'(leq(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRneq_2__49'([A|B], C, D, E) :-
	'CHRneq_2__49'(B, C, D, E).
:- set_flag('CHRneq_2__49' / 4, leash, notrace).
'CHRneq_2__50'(['CHRneq_2'(neq(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRneq_2__50'([A|B], C, D, E) :-
	'CHRneq_2__50'(B, C, D, E).
:- set_flag('CHRneq_2__50' / 4, leash, notrace).
'CHRneq_2__51'(['CHRlss_2'(lss(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRneq_2__51'([A|B], C, D, E) :-
	'CHRneq_2__51'(B, C, D, E).
:- set_flag('CHRneq_2__51' / 4, leash, notrace).
'CHRneq_2__52'(['CHRlss_2'(lss(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRneq_2__52'([A|B], C, D, E) :-
	'CHRneq_2__52'(B, C, D, E).
:- set_flag('CHRneq_2__52' / 4, leash, notrace).
:- set_flag('CHRneq_2' / 4, leash, notrace).
:- current_macro('CHRneq_2' / 4, _23023, _23024, _23025) -> true ; define_macro('CHRneq_2' / 4, tr_chr / 2, [write]).
'CHRneq_2__47'(A, B, C, D) :-
	'CHRneq_2__53'(A, B, C, D).
:- set_flag('CHRneq_2__47' / 4, leash, notrace).
'CHRneq_2__53'(neq(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRneq_2__53__54'(F, C, neq(A, B), D, E).
'CHRneq_2__53'(neq(A, B), C, D, E) :-
	'CHRneq_2__53__55'(neq(A, B), C, D, E).
:- set_flag('CHRneq_2__53' / 4, leash, notrace).
'CHRneq_2__53__54'(['CHRneq_2'(neq(A, B), C, D, E)|F], G, neq(B, A), H, I) ?-
	'CHRvar'(C),
	coca(try_double(I, neq(B, A), E, neq(A, B), neq(J, K), neq(K, J), keep_first, true, true, subsumption)),
	!,
	'CHRkill'(C),
	coca(fired_rule(subsumption)),
	'CHRneq_2__53__54'(F, G, neq(B, A), H, I).
'CHRneq_2__53__54'([A|B], C, D, E, F) :-
	'CHRneq_2__53__54'(B, C, D, E, F).
'CHRneq_2__53__54'([], A, B, C, D) :-
	'CHRneq_2__53__55'(B, A, C, D).
:- set_flag('CHRneq_2__53__54' / 5, leash, notrace).
'CHRneq_2__53__55'(neq(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, neq(A, B)], 'CHRneq_2'(neq(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRneq_2__53__55' / 4, leash, notrace).



%%% Prolog clauses for minimum / 3

clause_minimum(A, B, C) :-
	(
	    leq(A, B),
	    C = A
	;
	    lss(B, A),
	    C = B
	).
:- current_macro(clause_minimum / 3, _23727, _23728, _23729) -> true ; define_macro(clause_minimum / 3, tr_chr / 2, [write]).
minimum(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, minimum(A, B, C))),
	'CHRminimum_3'(minimum(A, B, C), E, F, D).



%%% Rules handling for minimum / 3

'CHRminimum_3'(minimum(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHRminimum_3'(minimum(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHRminimum_3'(minimum(A, B, C), D, E, F) ?-
	coca(try_rule(F, minimum(A, B, C), built_in, minimum(G, H, I), replacement, (ground(G), ground(H)), G @=< H -> I = G ; I = H)),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(D),
	coca(fired_rule(built_in)),
	(
	    A @=< B
	->
	    C = A
	;
	    C = B
	).
'CHRminimum_3'(minimum(A, B, C), D, E, F) ?-
	coca(try_rule(F, minimum(A, B, C), built_in, minimum(G, H, I), replacement, I ~= G, (I = H, lss(H, G)))),
	no_delayed_goals(C ~= A),
	!,
	'CHRkill'(D),
	coca(fired_rule(built_in)),
	C = B,
	lss(B, A).
'CHRminimum_3'(minimum(A, B, C), D, E, F) ?-
	coca(try_rule(F, minimum(A, B, C), built_in, minimum(G, H, I), replacement, I ~= H, (I = G, lss(G, H)))),
	no_delayed_goals(C ~= B),
	!,
	'CHRkill'(D),
	coca(fired_rule(built_in)),
	C = A,
	lss(A, B).
'CHRminimum_3'(minimum(A, A, B), C, D, E) ?-
	coca(try_rule(E, minimum(A, A, B), min_eq, minimum(F, F, G), replacement, true, F = G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(min_eq)),
	A = B.
'CHRminimum_3'(minimum(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRminimum_3__57'(G, [B, A], [], H),
	coca(try_double(F, minimum(A, B, C), H, leq(B, A), minimum(I, J, K), leq(J, I), keep_second, true, J = K, min_leq)),
	!,
	'CHRkill'(D),
	coca(fired_rule(min_leq)),
	B = C.
'CHRminimum_3'(minimum(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRminimum_3__58'(G, [B, A], [], H),
	coca(try_double(F, minimum(A, B, C), H, leq(A, B), minimum(I, J, K), leq(I, J), keep_second, true, I = K, min_leq)),
	!,
	'CHRkill'(D),
	coca(fired_rule(min_leq)),
	A = C.
'CHRminimum_3'(minimum(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRminimum_3__59'(G, [C, A], [], H),
	coca(try_double(F, minimum(A, B, C), H, lss(C, A), minimum(I, J, K), lss(K, I), keep_second, true, J = K, min_lss)),
	!,
	'CHRkill'(D),
	coca(fired_rule(min_lss)),
	B = C.
'CHRminimum_3'(minimum(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRminimum_3__60'(G, [C, B], [], H),
	coca(try_double(F, minimum(A, B, C), H, lss(C, B), minimum(I, J, K), lss(K, J), keep_second, true, I = K, min_lss)),
	!,
	'CHRkill'(D),
	coca(fired_rule(min_lss)),
	A = C.
'CHRminimum_3'(minimum(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRminimum_3__61'(G, [B, A], [H], I),
	coca(try_double(F, minimum(A, B, C), I, minimum(A, B, H), minimum(J, K, L), minimum(J, K, M), keep_second, true, L = M, functional)),
	!,
	'CHRkill'(D),
	coca(fired_rule(functional)),
	C = H.
'CHRminimum_3'(minimum(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRminimum_3__62'(G, [B, A], [H], I),
	coca(try_double(F, minimum(A, B, C), I, minimum(B, A, H), minimum(J, K, L), minimum(K, J, M), keep_second, true, L = M, functional)),
	!,
	'CHRkill'(D),
	coca(fired_rule(functional)),
	C = H.
'CHRminimum_3'(minimum(A, B, C), D, E, F) :-
	'CHRminimum_3__56'(minimum(A, B, C), D, E, F).
'CHRminimum_3__57'(['CHRleq_2'(leq(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRminimum_3__57'([A|B], C, D, E) :-
	'CHRminimum_3__57'(B, C, D, E).
:- set_flag('CHRminimum_3__57' / 4, leash, notrace).
'CHRminimum_3__58'(['CHRleq_2'(leq(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRminimum_3__58'([A|B], C, D, E) :-
	'CHRminimum_3__58'(B, C, D, E).
:- set_flag('CHRminimum_3__58' / 4, leash, notrace).
'CHRminimum_3__59'(['CHRlss_2'(lss(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRminimum_3__59'([A|B], C, D, E) :-
	'CHRminimum_3__59'(B, C, D, E).
:- set_flag('CHRminimum_3__59' / 4, leash, notrace).
'CHRminimum_3__60'(['CHRlss_2'(lss(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRminimum_3__60'([A|B], C, D, E) :-
	'CHRminimum_3__60'(B, C, D, E).
:- set_flag('CHRminimum_3__60' / 4, leash, notrace).
'CHRminimum_3__61'(['CHRminimum_3'(minimum(A, B, C), D, E, F)|G], [B, A], [H], I) ?-
	'CHRvar'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHRminimum_3__61'([A|B], C, D, E) :-
	'CHRminimum_3__61'(B, C, D, E).
:- set_flag('CHRminimum_3__61' / 4, leash, notrace).
'CHRminimum_3__62'(['CHRminimum_3'(minimum(A, B, C), D, E, F)|G], [A, B], [H], I) ?-
	'CHRvar'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHRminimum_3__62'([A|B], C, D, E) :-
	'CHRminimum_3__62'(B, C, D, E).
:- set_flag('CHRminimum_3__62' / 4, leash, notrace).
:- set_flag('CHRminimum_3' / 4, leash, notrace).
:- current_macro('CHRminimum_3' / 4, _26729, _26730, _26731) -> true ; define_macro('CHRminimum_3' / 4, tr_chr / 2, [write]).
'CHRminimum_3__56'(minimum(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('CHRminimum_3__56', E),
	coca(try_rule(F, minimum(A, B, C), propagation, minimum(G, H, I), augmentation, G \== H, (leq(I, G), leq(I, H)))),
	no_delayed_goals(A \== B),
	!,
	'CHRminimum_3__56__64'(minimum(A, B, C), D, E, F),
	coca(fired_rule(propagation)),
	leq(C, A),
	leq(C, B).
'CHRminimum_3__56'(A, B, C, D) ?-
	'CHRminimum_3__56__64'(A, B, C, D).
:- set_flag('CHRminimum_3__56' / 4, leash, notrace).
'CHRminimum_3__56__64'(A, B, C, D) :-
	'CHRminimum_3__63'(A, B, C, D).
:- set_flag('CHRminimum_3__56__64' / 4, leash, notrace).
'CHRminimum_3__63'(minimum(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRminimum_3__63__65'(G, D, minimum(A, B, C), E, F).
'CHRminimum_3__63'(minimum(A, B, C), D, E, F) :-
	'CHRminimum_3__63__66'(minimum(A, B, C), D, E, F).
:- set_flag('CHRminimum_3__63' / 4, leash, notrace).
'CHRminimum_3__63__65'(['CHRminimum_3'(minimum(A, B, C), D, E, F)|G], H, minimum(A, B, I), J, K) ?-
	'CHRvar'(D),
	coca(try_double(K, minimum(A, B, I), F, minimum(A, B, C), minimum(L, M, N), minimum(L, M, O), keep_first, true, O = N, functional)),
	!,
	'CHRkill'(D),
	coca(fired_rule(functional)),
	'CHRminimum_3__63__65'(G, H, minimum(A, B, I), J, K),
	C = I.
'CHRminimum_3__63__65'([A|B], C, D, E, F) :-
	'CHRminimum_3__63__65'(B, C, D, E, F).
'CHRminimum_3__63__65'([], A, B, C, D) :-
	'CHRminimum_3__63__66'(B, A, C, D).
:- set_flag('CHRminimum_3__63__65' / 5, leash, notrace).
'CHRminimum_3__63__66'(minimum(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRminimum_3__63__66__67'(G, D, minimum(A, B, C), E, F).
'CHRminimum_3__63__66'(minimum(A, B, C), D, E, F) :-
	'CHRminimum_3__63__66__68'(minimum(A, B, C), D, E, F).
:- set_flag('CHRminimum_3__63__66' / 4, leash, notrace).
'CHRminimum_3__63__66__67'(['CHRminimum_3'(minimum(A, B, C), D, E, F)|G], H, minimum(B, A, I), J, K) ?-
	'CHRvar'(D),
	coca(try_double(K, minimum(B, A, I), F, minimum(A, B, C), minimum(L, M, N), minimum(M, L, O), keep_first, true, O = N, functional)),
	!,
	'CHRkill'(D),
	coca(fired_rule(functional)),
	'CHRminimum_3__63__66__67'(G, H, minimum(B, A, I), J, K),
	C = I.
'CHRminimum_3__63__66__67'([A|B], C, D, E, F) :-
	'CHRminimum_3__63__66__67'(B, C, D, E, F).
'CHRminimum_3__63__66__67'([], A, B, C, D) :-
	'CHRminimum_3__63__66__68'(B, A, C, D).
:- set_flag('CHRminimum_3__63__66__67' / 5, leash, notrace).
'CHRminimum_3__63__66__68'(minimum(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, minimum(A, B, C)], 'CHRminimum_3'(minimum(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRminimum_3__63__66__68' / 4, leash, notrace).



%%% Prolog clauses for maximum / 3

clause_maximum(A, B, C) :-
	(
	    leq(A, B),
	    C = B
	;
	    lss(B, A),
	    C = A
	).
:- current_macro(clause_maximum / 3, _28145, _28146, _28147) -> true ; define_macro(clause_maximum / 3, tr_chr / 2, [write]).
maximum(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, maximum(A, B, C))),
	'CHRmaximum_3'(maximum(A, B, C), E, F, D).



%%% Rules handling for maximum / 3

'CHRmaximum_3'(maximum(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHRmaximum_3'(maximum(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHRmaximum_3'(maximum(A, B, C), D, E, F) ?-
	coca(try_rule(F, maximum(A, B, C), built_in, maximum(G, H, I), replacement, (ground(G), ground(H)), H @=< G -> I = G ; I = H)),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(D),
	coca(fired_rule(built_in)),
	(
	    B @=< A
	->
	    C = A
	;
	    C = B
	).
'CHRmaximum_3'(maximum(A, B, C), D, E, F) ?-
	coca(try_rule(F, maximum(A, B, C), built_in, maximum(G, H, I), replacement, I ~= G, (I = H, lss(G, H)))),
	no_delayed_goals(C ~= A),
	!,
	'CHRkill'(D),
	coca(fired_rule(built_in)),
	C = B,
	lss(A, B).
'CHRmaximum_3'(maximum(A, B, C), D, E, F) ?-
	coca(try_rule(F, maximum(A, B, C), built_in, maximum(G, H, I), replacement, I ~= H, (I = G, lss(H, G)))),
	no_delayed_goals(C ~= B),
	!,
	'CHRkill'(D),
	coca(fired_rule(built_in)),
	C = A,
	lss(B, A).
'CHRmaximum_3'(maximum(A, A, B), C, D, E) ?-
	coca(try_rule(E, maximum(A, A, B), max_eq, maximum(F, F, G), replacement, true, F = G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(max_eq)),
	A = B.
'CHRmaximum_3'(maximum(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRmaximum_3__70'(G, [B, A], [], H),
	coca(try_double(F, maximum(A, B, C), H, leq(B, A), maximum(I, J, K), leq(J, I), keep_second, true, I = K, max_leq)),
	!,
	'CHRkill'(D),
	coca(fired_rule(max_leq)),
	A = C.
'CHRmaximum_3'(maximum(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRmaximum_3__71'(G, [B, A], [], H),
	coca(try_double(F, maximum(A, B, C), H, leq(A, B), maximum(I, J, K), leq(I, J), keep_second, true, J = K, max_leq)),
	!,
	'CHRkill'(D),
	coca(fired_rule(max_leq)),
	B = C.
'CHRmaximum_3'(maximum(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRmaximum_3__72'(G, [C, A], [], H),
	coca(try_double(F, maximum(A, B, C), H, lss(A, C), maximum(I, J, K), lss(I, K), keep_second, true, J = K, max_lss)),
	!,
	'CHRkill'(D),
	coca(fired_rule(max_lss)),
	B = C.
'CHRmaximum_3'(maximum(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRmaximum_3__73'(G, [C, B], [], H),
	coca(try_double(F, maximum(A, B, C), H, lss(B, C), maximum(I, J, K), lss(J, K), keep_second, true, I = K, max_lss)),
	!,
	'CHRkill'(D),
	coca(fired_rule(max_lss)),
	A = C.
'CHRmaximum_3'(maximum(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRmaximum_3__74'(G, [B, A], [H], I),
	coca(try_double(F, maximum(A, B, C), I, maximum(A, B, H), maximum(J, K, L), maximum(J, K, M), keep_second, true, L = M, functional)),
	!,
	'CHRkill'(D),
	coca(fired_rule(functional)),
	C = H.
'CHRmaximum_3'(maximum(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRmaximum_3__75'(G, [B, A], [H], I),
	coca(try_double(F, maximum(A, B, C), I, maximum(B, A, H), maximum(J, K, L), maximum(K, J, M), keep_second, true, L = M, functional)),
	!,
	'CHRkill'(D),
	coca(fired_rule(functional)),
	C = H.
'CHRmaximum_3'(maximum(A, B, C), D, E, F) :-
	'CHRmaximum_3__69'(maximum(A, B, C), D, E, F).
'CHRmaximum_3__70'(['CHRleq_2'(leq(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRmaximum_3__70'([A|B], C, D, E) :-
	'CHRmaximum_3__70'(B, C, D, E).
:- set_flag('CHRmaximum_3__70' / 4, leash, notrace).
'CHRmaximum_3__71'(['CHRleq_2'(leq(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRmaximum_3__71'([A|B], C, D, E) :-
	'CHRmaximum_3__71'(B, C, D, E).
:- set_flag('CHRmaximum_3__71' / 4, leash, notrace).
'CHRmaximum_3__72'(['CHRlss_2'(lss(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRmaximum_3__72'([A|B], C, D, E) :-
	'CHRmaximum_3__72'(B, C, D, E).
:- set_flag('CHRmaximum_3__72' / 4, leash, notrace).
'CHRmaximum_3__73'(['CHRlss_2'(lss(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRmaximum_3__73'([A|B], C, D, E) :-
	'CHRmaximum_3__73'(B, C, D, E).
:- set_flag('CHRmaximum_3__73' / 4, leash, notrace).
'CHRmaximum_3__74'(['CHRmaximum_3'(maximum(A, B, C), D, E, F)|G], [B, A], [H], I) ?-
	'CHRvar'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHRmaximum_3__74'([A|B], C, D, E) :-
	'CHRmaximum_3__74'(B, C, D, E).
:- set_flag('CHRmaximum_3__74' / 4, leash, notrace).
'CHRmaximum_3__75'(['CHRmaximum_3'(maximum(A, B, C), D, E, F)|G], [A, B], [H], I) ?-
	'CHRvar'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHRmaximum_3__75'([A|B], C, D, E) :-
	'CHRmaximum_3__75'(B, C, D, E).
:- set_flag('CHRmaximum_3__75' / 4, leash, notrace).
:- set_flag('CHRmaximum_3' / 4, leash, notrace).
:- current_macro('CHRmaximum_3' / 4, _31147, _31148, _31149) -> true ; define_macro('CHRmaximum_3' / 4, tr_chr / 2, [write]).
'CHRmaximum_3__69'(maximum(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('CHRmaximum_3__69', E),
	coca(try_rule(F, maximum(A, B, C), propagation, maximum(G, H, I), augmentation, G \== H, (leq(G, I), leq(H, I)))),
	no_delayed_goals(A \== B),
	!,
	'CHRmaximum_3__69__77'(maximum(A, B, C), D, E, F),
	coca(fired_rule(propagation)),
	leq(A, C),
	leq(B, C).
'CHRmaximum_3__69'(A, B, C, D) ?-
	'CHRmaximum_3__69__77'(A, B, C, D).
:- set_flag('CHRmaximum_3__69' / 4, leash, notrace).
'CHRmaximum_3__69__77'(A, B, C, D) :-
	'CHRmaximum_3__76'(A, B, C, D).
:- set_flag('CHRmaximum_3__69__77' / 4, leash, notrace).
'CHRmaximum_3__76'(maximum(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRmaximum_3__76__78'(G, D, maximum(A, B, C), E, F).
'CHRmaximum_3__76'(maximum(A, B, C), D, E, F) :-
	'CHRmaximum_3__76__79'(maximum(A, B, C), D, E, F).
:- set_flag('CHRmaximum_3__76' / 4, leash, notrace).
'CHRmaximum_3__76__78'(['CHRmaximum_3'(maximum(A, B, C), D, E, F)|G], H, maximum(A, B, I), J, K) ?-
	'CHRvar'(D),
	coca(try_double(K, maximum(A, B, I), F, maximum(A, B, C), maximum(L, M, N), maximum(L, M, O), keep_first, true, O = N, functional)),
	!,
	'CHRkill'(D),
	coca(fired_rule(functional)),
	'CHRmaximum_3__76__78'(G, H, maximum(A, B, I), J, K),
	C = I.
'CHRmaximum_3__76__78'([A|B], C, D, E, F) :-
	'CHRmaximum_3__76__78'(B, C, D, E, F).
'CHRmaximum_3__76__78'([], A, B, C, D) :-
	'CHRmaximum_3__76__79'(B, A, C, D).
:- set_flag('CHRmaximum_3__76__78' / 5, leash, notrace).
'CHRmaximum_3__76__79'(maximum(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRmaximum_3__76__79__80'(G, D, maximum(A, B, C), E, F).
'CHRmaximum_3__76__79'(maximum(A, B, C), D, E, F) :-
	'CHRmaximum_3__76__79__81'(maximum(A, B, C), D, E, F).
:- set_flag('CHRmaximum_3__76__79' / 4, leash, notrace).
'CHRmaximum_3__76__79__80'(['CHRmaximum_3'(maximum(A, B, C), D, E, F)|G], H, maximum(B, A, I), J, K) ?-
	'CHRvar'(D),
	coca(try_double(K, maximum(B, A, I), F, maximum(A, B, C), maximum(L, M, N), maximum(M, L, O), keep_first, true, O = N, functional)),
	!,
	'CHRkill'(D),
	coca(fired_rule(functional)),
	'CHRmaximum_3__76__79__80'(G, H, maximum(B, A, I), J, K),
	C = I.
'CHRmaximum_3__76__79__80'([A|B], C, D, E, F) :-
	'CHRmaximum_3__76__79__80'(B, C, D, E, F).
'CHRmaximum_3__76__79__80'([], A, B, C, D) :-
	'CHRmaximum_3__76__79__81'(B, A, C, D).
:- set_flag('CHRmaximum_3__76__79__80' / 5, leash, notrace).
'CHRmaximum_3__76__79__81'(maximum(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, maximum(A, B, C)], 'CHRmaximum_3'(maximum(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRmaximum_3__76__79__81' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
