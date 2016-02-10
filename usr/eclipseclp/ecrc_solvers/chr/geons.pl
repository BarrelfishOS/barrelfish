
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- op(31, xfx, #).
constrained(_263) :- delayed_goals(_263, [_273|_274]).
:- local ground / 1.
delay ground(_301) if nonground(_301).
ground(_348).
geon(_359, _362, _365) :- geons(_359, [rect(_362, _365)]).
approx_walls([], []).
approx_walls([wall(#(_414, _418), #(_414, _424))|_428], [rect(#(_435, _418), #(_441, _424))|_448]) :- !, _435 is _414 - 1, _441 is _414 + 1, approx_walls(_428, _448).
approx_walls([wall(#(_660, _664), #(_667, _664))|_674], [rect(#(_660, _684), #(_667, _690))|_694]) :- !, _684 is _664 - 1, _690 is _664 + 1, approx_walls(_674, _694).
approx_walls([wall(#(_906, _910), #(_913, _917))|_921], [rect(#(_906, _910), #(_913, _917))|_939]) :- approx_walls(_921, _939).
intersect_geons(_1512, _1515, _1518) :- bagof(_1528, intersect_geon(_1512, _1515, _1528), _1518).
intersect_geon(_1555, _1558, rect(#(_1563, _1567), #(_1570, _1574))) :- member(rect(#(_1589, _1593), #(_1596, _1600)), _1555), member(rect(#(_1616, _1620), #(_1623, _1627)), _1558), _1563 is max(_1589, _1616), _1567 is max(_1593, _1620), _1570 is min(_1596, _1623), _1574 is min(_1600, _1627), _1563 =< _1570, _1567 =< _1574.



%%% Callables for geons / 2

'CHRlabel_with'(geons(A, B), C, D) ?-
	coca(try_clause(D, geons(A, B), geons(E, F), true)),
	coca(clause_fired(D)),
	'CHR='(C, clause_geons(A, B)).



%%% Prolog clauses for geons / 2

clause_geons(#(A, B), C) :-
	member(rect(#(D, E), #(F, G)), C),
	A is (D + F) / 2,
	B is (E + G) / 2.
:- current_macro(clause_geons / 2, _2196, _2197, _2198) -> true ; define_macro(clause_geons / 2, tr_chr / 2, [write]).
geons(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, geons(A, B))),
	'CHRgeons_2'(geons(A, B), D, E, C).



%%% Rules handling for geons / 2

'CHRgeons_2'(geons(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRgeons_2'(geons(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRgeons_2'(geons(A, B), C, D, E) :-
	'CHRgeons_2__1'(geons(A, B), C, D, E).
:- set_flag('CHRgeons_2' / 4, leash, notrace).
:- current_macro('CHRgeons_2' / 4, _2450, _2451, _2452) -> true ; define_macro('CHRgeons_2' / 4, tr_chr / 2, [write]).
'CHRgeons_2__1'(A, B, C, D) :-
	'CHRgeons_2__2'(A, B, C, D).
:- set_flag('CHRgeons_2__1' / 4, leash, notrace).
'CHRgeons_2__2'(geons(#(A, B), C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRgeons_2__2__3'(G, D, geons(#(A, B), C), E, F).
'CHRgeons_2__2'(geons(A, B), C, D, E) :-
	'CHRgeons_2__2__4'(geons(A, B), C, D, E).
:- set_flag('CHRgeons_2__2' / 4, leash, notrace).
'CHRgeons_2__2__3'(['CHRgeons_2'(geons(#(A, B), C), D, E, F)|G], H, geons(#(A, B), I), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(anonymous("0")), H, D, J, E),
	coca(try_double(K, geons(#(A, B), I), F, geons(#(A, B), C), geons(#(L, M), N), geons(#(L, M), O), augmentation, (true, 'CHRkeep_heads_checking'(geons(#(L, M), N), P, geons(#(L, M), O), Q, geons(#(L, M), R), S)), (intersect_geons(N, O, R), R \== [], ('CHRhead_not_kept'(S) -> geons(#(L, M), R) ; true)), anonymous("0"))),
	no_global_bindings((true, 'CHRkeep_heads_checking'(geons(#(A, B), I), H, geons(#(A, B), C), D, geons(#(A, B), T), U)), (geons(#(A, B), I), geons(#(A, B), C))),
	!,
	coca(fired_rule(anonymous("0"))),
	'CHRgeons_2__2__3'(G, H, geons(#(A, B), I), J, K),
	intersect_geons(I, C, T),
	T \== [],
	(
	    'CHRhead_not_kept'(U)
	->
	    geons(#(A, B), T)
	;
	    true
	).
'CHRgeons_2__2__3'([A|B], C, D, E, F) :-
	'CHRgeons_2__2__3'(B, C, D, E, F).
'CHRgeons_2__2__3'([], A, B, C, D) :-
	'CHRgeons_2__2__4'(B, A, C, D).
:- set_flag('CHRgeons_2__2__3' / 5, leash, notrace).
'CHRgeons_2__2__4'(geons(#(A, B), C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRgeons_2__2__4__5'(G, D, geons(#(A, B), C), E, F).
'CHRgeons_2__2__4'(geons(A, B), C, D, E) :-
	'CHRgeons_2__2__4__6'(geons(A, B), C, D, E).
:- set_flag('CHRgeons_2__2__4' / 4, leash, notrace).
'CHRgeons_2__2__4__5'(['CHRgeons_2'(geons(#(A, B), C), D, E, F)|G], H, geons(#(A, B), I), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('21'(anonymous("0")), H, D, J, E),
	coca(try_double(K, geons(#(A, B), I), F, geons(#(A, B), C), geons(#(L, M), N), geons(#(L, M), O), augmentation, (true, 'CHRkeep_heads_checking'(geons(#(L, M), O), P, geons(#(L, M), N), Q, geons(#(L, M), R), S)), (intersect_geons(O, N, R), R \== [], ('CHRhead_not_kept'(S) -> geons(#(L, M), R) ; true)), anonymous("0"))),
	no_global_bindings((true, 'CHRkeep_heads_checking'(geons(#(A, B), C), D, geons(#(A, B), I), H, geons(#(A, B), T), U)), (geons(#(A, B), I), geons(#(A, B), C))),
	!,
	coca(fired_rule(anonymous("0"))),
	'CHRgeons_2__2__4__5'(G, H, geons(#(A, B), I), J, K),
	intersect_geons(C, I, T),
	T \== [],
	(
	    'CHRhead_not_kept'(U)
	->
	    geons(#(A, B), T)
	;
	    true
	).
'CHRgeons_2__2__4__5'([A|B], C, D, E, F) :-
	'CHRgeons_2__2__4__5'(B, C, D, E, F).
'CHRgeons_2__2__4__5'([], A, B, C, D) :-
	'CHRgeons_2__2__4__6'(B, A, C, D).
:- set_flag('CHRgeons_2__2__4__5' / 5, leash, notrace).
'CHRgeons_2__2__4__6'(geons(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, geons(A, B)], 'CHRgeons_2'(geons(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRgeons_2__2__4__6' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
