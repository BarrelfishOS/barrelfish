
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- op(700, xfx, eqlist).
:- op(700, xfx, lenlist).
chr_member(_1664, _1667) :- eqlist([_1674, [_1664], _1680], _1667).
chr_append(_1693, _1696, _1699) :- eqlist([_1693, _1696], _1699).
chr_last(_1722, _1725) :- eqlist([_1732, [_1725]], _1722).
palindrome([]).
palindrome([_1761]).
palindrome(_1772) :- lenlist(_1778, 1), eqlist([_1778, _1789, _1778], _1772), palindrome(_1789).
reverse([], []).
reverse(_1907, _1910) :- lenlist(_1907, _1920), lenlist(_1910, _1920), lenlist(_1930, 1), eqlist([_1930, _1941], _1907), eqlist([_1950, _1930], _1910), reverse(_1941, _1950).
permute([], []).
permute(_2141, _2144) :- lenlist(_2141, _2154), lenlist(_2144, _2154), lenlist(_2164, 1), eqlist([_2164, _2175], _2141), eqlist([_2184, _2164, _2189], _2144), eqlist([_2184, _2189], _2203), permute(_2175, _2203).
tree([_2420], [_2420], _2420) :- var(_2420) -> suspend(atomic(_2420), 3, _2420 -> inst) ; atomic(_2420).
tree(_2571, _2574, t(_2579, _2582, _2585)) :- eqlist([[_2579], _2603, _2606], _2571), eqlist([_2615, _2618, [_2579]], _2574), tree(_2603, _2615, _2582), tree(_2606, _2618, _2585).
transpose([], _2837) :- eqlist([_2837, [[]]], [[]|_2837]).
transpose([_2865|_2866], _2869) :- first_column(_2869, _2865, _2882), transpose(_2866, _2882).
first_column([], [], []).
first_column([[_2987|_2988]|_2989], [_2987|_2994], [_2988|_2999]) :- first_column(_2989, _2994, _2999).



%%% Callables for eqlist / 2

'CHRlabel_with'(eqlist(A, B), C, D) ?-
	coca(try_clause(D, eqlist(A, B), eqlist(E, F), true)),
	coca(clause_fired(D)),
	'CHR='(C, clause_eqlist(A, B)).



%%% Prolog clauses for eqlist / 2

clause_eqlist([A|B], C) :-
	(
	    var(C)
	->
	    length(C, D)
	;
	    true
	),
	(
	    A = [],
	    eqlist(B, C)
	;
	    C = [E|F],
	    A = [E|G],
	    eqlist([G|B], F)
	).
:- current_macro(clause_eqlist / 2, _3259, _3260, _3261) -> true ; define_macro(clause_eqlist / 2, tr_chr / 2, [write]).
eqlist(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, eqlist(A, B))),
	'CHReqlist_2'(eqlist(A, B), D, E, C).



%%% Rules handling for eqlist / 2

'CHReqlist_2'(eqlist(A, B), C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHReqlist_2'(eqlist([], A), B, C, D) ?-
	coca(try_rule(D, eqlist([], A), anonymous("0"), eqlist([], E), replacement, true, E = [])),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("0"))),
	A = [].
'CHReqlist_2'(eqlist([A], B), C, D, E) ?-
	coca(try_rule(E, eqlist([A], B), anonymous("1"), eqlist([F], G), replacement, true, F = G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("1"))),
	A = B.
'CHReqlist_2'(eqlist([A|B], []), C, D, E) ?-
	coca(try_rule(E, eqlist([A|B], []), anonymous("2"), eqlist([F|G], []), replacement, true, (F = [], eqlist(G, [])))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("2"))),
	A = [],
	eqlist(B, []).
'CHReqlist_2'(eqlist([[A|B]|C], D), E, F, G) ?-
	coca(try_rule(G, eqlist([[A|B]|C], D), anonymous("3"), eqlist([[H|I]|J], K), replacement, true, (K = [H|L], eqlist([I|J], L)))),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("3"))),
	D = [A|M],
	eqlist([B|C], M).
'CHReqlist_2'(eqlist(A, B), C, D, E) ?-
	coca(try_rule(E, eqlist(A, B), anonymous("4"), eqlist(F, G), replacement, (delete(H, F, I), H == []), eqlist(I, G))),
	no_delayed_goals((delete(J, A, K), J == [])),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("4"))),
	eqlist(K, B).
'CHReqlist_2'(eqlist(A, B), C, D, E) ?-
	coca(try_rule(E, eqlist(A, B), anonymous("5"), eqlist(F, G), replacement, (delete(H, F, I), H == G), eqlist(I, []))),
	no_delayed_goals((delete(J, A, K), J == B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("5"))),
	eqlist(K, []).
'CHReqlist_2'(eqlist(A, B), C, D, E) :-
	'CHReqlist_2__9'(eqlist(A, B), C, D, E).
:- set_flag('CHReqlist_2' / 4, leash, notrace).
:- current_macro('CHReqlist_2' / 4, _4322, _4323, _4324) -> true ; define_macro('CHReqlist_2' / 4, tr_chr / 2, [write]).
'CHReqlist_2__9'(A, B, C, D) :-
	'CHReqlist_2__10'(A, B, C, D).
:- set_flag('CHReqlist_2__9' / 4, leash, notrace).
'CHReqlist_2__10'(eqlist(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, eqlist(A, B)], 'CHReqlist_2'(eqlist(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHReqlist_2__10' / 4, leash, notrace).
lenlist(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, lenlist(A, B))),
	'CHRlenlist_2'(lenlist(A, B), D, E, C).



%%% Rules handling for lenlist / 2

'CHRlenlist_2'(lenlist(A, B), C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHRlenlist_2'(lenlist([], A), B, C, D) ?-
	coca(try_rule(D, lenlist([], A), anonymous("6"), lenlist([], E), replacement, true, var(E) -> E = 0 ; E =:= 0)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("6"))),
	(
	    var(A)
	->
	    A = 0
	;
	    A =:= 0
	).
'CHRlenlist_2'(lenlist([A|B], C), D, E, F) ?-
	coca(try_rule(F, lenlist([A|B], C), anonymous("7"), lenlist([G|H], I), replacement, true, (I > 0, plus(J, 1, I), lenlist(H, J)))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("7"))),
	C > 0,
	plus(K, 1, C),
	lenlist(B, K).
'CHRlenlist_2'(lenlist(A, B), C, D, E) ?-
	coca(try_rule(E, lenlist(A, B), anonymous("8"), lenlist(F, G), replacement, ground(G), length(F, G))),
	no_delayed_goals(ground(B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("8"))),
	length(A, B).
'CHRlenlist_2'(lenlist(A, B), C, D, E) :-
	'CHRlenlist_2__11'(lenlist(A, B), C, D, E).
:- set_flag('CHRlenlist_2' / 4, leash, notrace).
:- current_macro('CHRlenlist_2' / 4, _5181, _5182, _5183) -> true ; define_macro('CHRlenlist_2' / 4, tr_chr / 2, [write]).
'CHRlenlist_2__11'(A, B, C, D) :-
	'CHRlenlist_2__12'(A, B, C, D).
:- set_flag('CHRlenlist_2__11' / 4, leash, notrace).
'CHRlenlist_2__12'(lenlist(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, lenlist(A, B)], 'CHRlenlist_2'(lenlist(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRlenlist_2__12' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
