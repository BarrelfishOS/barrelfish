
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
card(_11752, _11751, _11750) :- length(_11750, _11749), _11752 =< _11751, 0 =< _11751, _11752 =< _11749, card(_11752, _11751, _11750, _11749).
set_to_ones([]).
set_to_ones([1|_12539]) :- set_to_ones(_12539).
set_to_zeros([]).
set_to_zeros([0|_12565]) :- set_to_zeros(_12565).
:- op(100, fy, ~~).
:- op(100, xfy, #).
solve_bool(_12634, _12633) :- var(_12634), !, _12634 = _12633.
solve_bool(_12660, _12659) :- atomic(_12660), !, _12660 = _12659.
solve_bool(_12699 * _12695, _12689) ?- !, solve_bool(_12699, _12691), solve_bool(_12695, _12690), and(_12691, _12690, _12689).
solve_bool(_12739 + _12735, _12729) ?- !, solve_bool(_12739, _12731), solve_bool(_12735, _12730), or(_12731, _12730, _12729).
solve_bool(#(_12779, _12775), _12769) ?- !, solve_bool(_12779, _12771), solve_bool(_12775, _12770), exor(_12771, _12770, _12769).
solve_bool(~~(_12810), _12805) ?- !, solve_bool(_12810, _12806), neg(_12806, _12805).
solve_bool(_12854 -> _12850, _12841) ?- !, solve_bool(_12854, _12846), solve_bool(_12850, _12845), imp(_12846, _12845), _12841 = 1.
solve_bool(_12899 = _12895, _12886) ?- !, solve_bool(_12899, _12891), solve_bool(_12895, _12890), _12891 = _12890, _12886 = 1.
bool_portray(and(_12930, _12929, _12925), _12931) ?- !, _12931 = (_12930 * _12929 = _12925).
bool_portray(or(_12963, _12962, _12958), _12964) ?- !, _12964 = (_12963 + _12962 = _12958).
bool_portray(exor(_12996, _12995, _12991), _12997) ?- !, _12997 = (#(_12996, _12995) = _12991).
bool_portray(neg(_13027, _13026), _13028) ?- !, _13028 = (_13027 = ~~(_13026)).
bool_portray(imp(_13055, _13054), _13056) ?- !, _13056 = (_13055 -> _13054).
bool_portray(card(_13085, _13084, _13083, _13095), _13086) ?- !, _13086 = card(_13085, _13084, _13083).
:- define_macro(type(compound), bool_portray / 2, [write]).
label_bool([]).
label_bool([_13147|_13140]) :- (_13147 = 0 ; _13147 = 1), label_bool(_13140).



%%% Callables for boolean / 1

'CHRlabel_with'(boolean(A), B, C) ?-
	coca(try_clause(C, boolean(A), boolean(D), true)),
	coca(clause_fired(C)),
	'CHR='(B, clause_boolean(A)).



%%% Callables for and / 3

'CHRlabel_with'(and(A, B, C), D, E) ?-
	coca(try_clause(E, and(A, B, C), and(F, G, H), true)),
	coca(clause_fired(E)),
	'CHR='(D, clause_and(A, B, C)).



%%% Callables for or / 3

'CHRlabel_with'(or(A, B, C), D, E) ?-
	coca(try_clause(E, or(A, B, C), or(F, G, H), true)),
	coca(clause_fired(E)),
	'CHR='(D, clause_or(A, B, C)).
'CHRlabel_with'(or(A, B, C), D, E) ?-
	coca(try_clause(E, or(A, B, C), or(F, G, H), true)),
	coca(clause_fired(E)),
	'CHR='(D, clause_or(A, B, C)).



%%% Callables for neg / 2

'CHRlabel_with'(neg(A, B), C, D) ?-
	coca(try_clause(D, neg(A, B), neg(E, F), true)),
	coca(clause_fired(D)),
	'CHR='(C, clause_neg(A, B)).



%%% Callables for imp / 2

'CHRlabel_with'(imp(A, B), C, D) ?-
	coca(try_clause(D, imp(A, B), imp(E, F), true)),
	coca(clause_fired(D)),
	'CHR='(C, clause_imp(A, B)).



%%% Callables for card / 4

'CHRlabel_with'(card(A, B, C, D), E, F) ?-
	coca(try_clause(F, card(A, B, C, D), card(G, H, I, J), true)),
	coca(clause_fired(F)),
	'CHR='(E, clause_card(A, B, C, D)).



%%% Prolog clauses for boolean / 1

clause_boolean(0).
clause_boolean(1).
:- current_macro(clause_boolean / 1, _14192, _14193, _14194) -> true ; define_macro(clause_boolean / 1, tr_chr / 2, [write]).
boolean(A) :-
	'CHRgen_num'(B),
	coca(add_one_constraint(B, boolean(A))),
	'CHRboolean_1'(boolean(A), C, D, B).



%%% Rules handling for boolean / 1

'CHRboolean_1'(boolean(A), B, C, D) :-
	(
	    'CHRnonvar'(B)
	;
	    'CHRalready_in'('CHRboolean_1'(boolean(A), B, C, D)),
	    coca(already_in)
	),
	!.
'CHRboolean_1'(boolean(0), A, B, C) ?-
	coca(try_rule(C, boolean(0), anonymous("0"), boolean(0), replacement, true, true)),
	!,
	'CHRkill'(A),
	coca(fired_rule(anonymous("0"))).
'CHRboolean_1'(boolean(1), A, B, C) ?-
	coca(try_rule(C, boolean(1), anonymous("1"), boolean(1), replacement, true, true)),
	!,
	'CHRkill'(A),
	coca(fired_rule(anonymous("1"))).
'CHRboolean_1'(boolean(A), B, C, D) :-
	'CHRboolean_1__60'(boolean(A), B, C, D).
:- set_flag('CHRboolean_1' / 4, leash, notrace).
:- current_macro('CHRboolean_1' / 4, _14790, _14791, _14792) -> true ; define_macro('CHRboolean_1' / 4, tr_chr / 2, [write]).
'CHRboolean_1__60'(A, B, C, D) :-
	'CHRboolean_1__61'(A, B, C, D).
:- set_flag('CHRboolean_1__60' / 4, leash, notrace).
'CHRboolean_1__61'(boolean(A), B, C, D) :-
	(
	    'CHRvar'(B)
	->
	    'CHRdelay'([B, boolean(A)], 'CHRboolean_1'(boolean(A), B, C, D))
	;
	    true
	).
:- set_flag('CHRboolean_1__61' / 4, leash, notrace).



%%% Prolog clauses for and / 3

clause_and(0, A, 0).
clause_and(1, A, A).
:- current_macro(clause_and / 3, _15139, _15140, _15141) -> true ; define_macro(clause_and / 3, tr_chr / 2, [write]).
and(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, and(A, B, C))),
	'CHRand_3'(and(A, B, C), E, F, D).



%%% Rules handling for and / 3

'CHRand_3'(and(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHRand_3'(and(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHRand_3'(and(0, A, B), C, D, E) ?-
	coca(try_rule(E, and(0, A, B), anonymous("2"), and(0, F, G), replacement, true, G = 0)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("2"))),
	B = 0.
'CHRand_3'(and(A, 0, B), C, D, E) ?-
	coca(try_rule(E, and(A, 0, B), anonymous("3"), and(F, 0, G), replacement, true, G = 0)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("3"))),
	B = 0.
'CHRand_3'(and(1, A, B), C, D, E) ?-
	coca(try_rule(E, and(1, A, B), anonymous("4"), and(1, F, G), replacement, true, G = F)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("4"))),
	B = A.
'CHRand_3'(and(A, 1, B), C, D, E) ?-
	coca(try_rule(E, and(A, 1, B), anonymous("5"), and(F, 1, G), replacement, true, G = F)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("5"))),
	B = A.
'CHRand_3'(and(A, B, 1), C, D, E) ?-
	coca(try_rule(E, and(A, B, 1), anonymous("6"), and(F, G, 1), replacement, true, (F = 1, G = 1))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("6"))),
	[A, B] = [1, 1].
'CHRand_3'(and(A, A, B), C, D, E) ?-
	coca(try_rule(E, and(A, A, B), anonymous("7"), and(F, F, G), replacement, true, F = G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("7"))),
	A = B.
'CHRand_3'(and(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRand_3__63'(G, [C, A], [], H),
	coca(try_double(F, and(A, B, C), H, neg(A, C), and(I, J, K), neg(I, K), replacement, true, (I = 1, J = 0, K = 0), anonymous("36"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("36"))),
	[A, [B, C]] = [1, [0, 0]].
'CHRand_3'(and(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRand_3__64'(G, [C, A], [], H),
	coca(try_double(F, and(A, B, C), H, neg(C, A), and(I, J, K), neg(K, I), replacement, true, (I = 1, J = 0, K = 0), anonymous("37"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("37"))),
	[A, [B, C]] = [1, [0, 0]].
'CHRand_3'(and(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRand_3__65'(G, [C, B], [], H),
	coca(try_double(F, and(A, B, C), H, neg(B, C), and(I, J, K), neg(J, K), replacement, true, (I = 0, J = 1, K = 0), anonymous("38"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("38"))),
	[A, [B, C]] = [0, [1, 0]].
'CHRand_3'(and(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRand_3__66'(G, [C, B], [], H),
	coca(try_double(F, and(A, B, C), H, neg(C, B), and(I, J, K), neg(K, J), replacement, true, (I = 0, J = 1, K = 0), anonymous("39"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("39"))),
	[A, [B, C]] = [0, [1, 0]].
'CHRand_3'(and(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRand_3__67'(G, [B, A], [H], I),
	coca(try_double(F, and(A, B, C), I, and(A, B, H), and(J, K, L), and(J, K, M), keep_second, true, M = L, anonymous("8"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("8"))),
	H = C.
'CHRand_3'(and(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRand_3__68'(G, [B, A], [H], I),
	coca(try_double(F, and(A, B, C), I, and(B, A, H), and(J, K, L), and(K, J, M), keep_second, true, M = L, anonymous("9"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("9"))),
	H = C.
'CHRand_3'(and(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRand_3__69'(G, [B, A], [], H),
	coca(try_double(F, and(A, B, C), H, neg(A, B), and(I, J, K), neg(I, J), keep_second, true, K = 0, anonymous("34"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("34"))),
	C = 0.
'CHRand_3'(and(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRand_3__70'(G, [B, A], [], H),
	coca(try_double(F, and(A, B, C), H, neg(B, A), and(I, J, K), neg(J, I), keep_second, true, K = 0, anonymous("35"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("35"))),
	C = 0.
'CHRand_3'(and(A, B, C), D, E, F) :-
	'CHRand_3__62'(and(A, B, C), D, E, F).
'CHRand_3__63'(['CHRneg_2'(neg(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRand_3__63'([A|B], C, D, E) :-
	'CHRand_3__63'(B, C, D, E).
:- set_flag('CHRand_3__63' / 4, leash, notrace).
'CHRand_3__64'(['CHRneg_2'(neg(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRand_3__64'([A|B], C, D, E) :-
	'CHRand_3__64'(B, C, D, E).
:- set_flag('CHRand_3__64' / 4, leash, notrace).
'CHRand_3__65'(['CHRneg_2'(neg(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRand_3__65'([A|B], C, D, E) :-
	'CHRand_3__65'(B, C, D, E).
:- set_flag('CHRand_3__65' / 4, leash, notrace).
'CHRand_3__66'(['CHRneg_2'(neg(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRand_3__66'([A|B], C, D, E) :-
	'CHRand_3__66'(B, C, D, E).
:- set_flag('CHRand_3__66' / 4, leash, notrace).
'CHRand_3__67'(['CHRand_3'(and(A, B, C), D, E, F)|G], [B, A], [H], I) ?-
	'CHRvar'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHRand_3__67'([A|B], C, D, E) :-
	'CHRand_3__67'(B, C, D, E).
:- set_flag('CHRand_3__67' / 4, leash, notrace).
'CHRand_3__68'(['CHRand_3'(and(A, B, C), D, E, F)|G], [A, B], [H], I) ?-
	'CHRvar'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHRand_3__68'([A|B], C, D, E) :-
	'CHRand_3__68'(B, C, D, E).
:- set_flag('CHRand_3__68' / 4, leash, notrace).
'CHRand_3__69'(['CHRneg_2'(neg(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRand_3__69'([A|B], C, D, E) :-
	'CHRand_3__69'(B, C, D, E).
:- set_flag('CHRand_3__69' / 4, leash, notrace).
'CHRand_3__70'(['CHRneg_2'(neg(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRand_3__70'([A|B], C, D, E) :-
	'CHRand_3__70'(B, C, D, E).
:- set_flag('CHRand_3__70' / 4, leash, notrace).
:- set_flag('CHRand_3' / 4, leash, notrace).
:- current_macro('CHRand_3' / 4, _20401, _20402, _20403) -> true ; define_macro('CHRand_3' / 4, tr_chr / 2, [write]).
'CHRand_3__62'(A, B, C, D) :-
	'CHRand_3__71'(A, B, C, D).
:- set_flag('CHRand_3__62' / 4, leash, notrace).
'CHRand_3__71'(and(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRand_3__71__72'(G, D, and(A, B, C), E, F).
'CHRand_3__71'(and(A, B, C), D, E, F) :-
	'CHRand_3__71__73'(and(A, B, C), D, E, F).
:- set_flag('CHRand_3__71' / 4, leash, notrace).
'CHRand_3__71__72'(['CHRand_3'(and(A, B, C), D, E, F)|G], H, and(A, B, I), J, K) ?-
	'CHRvar'(D),
	coca(try_double(K, and(A, B, I), F, and(A, B, C), and(L, M, N), and(L, M, O), keep_first, true, N = O, anonymous("8"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("8"))),
	'CHRand_3__71__72'(G, H, and(A, B, I), J, K),
	I = C.
'CHRand_3__71__72'([A|B], C, D, E, F) :-
	'CHRand_3__71__72'(B, C, D, E, F).
'CHRand_3__71__72'([], A, B, C, D) :-
	'CHRand_3__71__73'(B, A, C, D).
:- set_flag('CHRand_3__71__72' / 5, leash, notrace).
'CHRand_3__71__73'(and(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRand_3__71__73__74'(G, D, and(A, B, C), E, F).
'CHRand_3__71__73'(and(A, B, C), D, E, F) :-
	'CHRand_3__71__73__75'(and(A, B, C), D, E, F).
:- set_flag('CHRand_3__71__73' / 4, leash, notrace).
'CHRand_3__71__73__74'(['CHRand_3'(and(A, B, C), D, E, F)|G], H, and(B, A, I), J, K) ?-
	'CHRvar'(D),
	coca(try_double(K, and(B, A, I), F, and(A, B, C), and(L, M, N), and(M, L, O), keep_first, true, N = O, anonymous("9"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("9"))),
	'CHRand_3__71__73__74'(G, H, and(B, A, I), J, K),
	I = C.
'CHRand_3__71__73__74'([A|B], C, D, E, F) :-
	'CHRand_3__71__73__74'(B, C, D, E, F).
'CHRand_3__71__73__74'([], A, B, C, D) :-
	'CHRand_3__71__73__75'(B, A, C, D).
:- set_flag('CHRand_3__71__73__74' / 5, leash, notrace).
'CHRand_3__71__73__75'(and(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, and(A, B, C)], 'CHRand_3'(and(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRand_3__71__73__75' / 4, leash, notrace).



%%% Prolog clauses for or / 3

clause_or(0, A, A).
clause_or(1, A, 1).
:- current_macro(clause_or / 3, _21932, _21933, _21934) -> true ; define_macro(clause_or / 3, tr_chr / 2, [write]).
or(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, or(A, B, C))),
	'CHRor_3'(or(A, B, C), E, F, D).



%%% Rules handling for or / 3

'CHRor_3'(or(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHRor_3'(or(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHRor_3'(or(0, A, B), C, D, E) ?-
	coca(try_rule(E, or(0, A, B), anonymous("10"), or(0, F, G), replacement, true, G = F)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("10"))),
	B = A.
'CHRor_3'(or(A, 0, B), C, D, E) ?-
	coca(try_rule(E, or(A, 0, B), anonymous("11"), or(F, 0, G), replacement, true, G = F)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("11"))),
	B = A.
'CHRor_3'(or(A, B, 0), C, D, E) ?-
	coca(try_rule(E, or(A, B, 0), anonymous("12"), or(F, G, 0), replacement, true, (F = 0, G = 0))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("12"))),
	[A, B] = [0, 0].
'CHRor_3'(or(1, A, B), C, D, E) ?-
	coca(try_rule(E, or(1, A, B), anonymous("13"), or(1, F, G), replacement, true, G = 1)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("13"))),
	B = 1.
'CHRor_3'(or(A, 1, B), C, D, E) ?-
	coca(try_rule(E, or(A, 1, B), anonymous("14"), or(F, 1, G), replacement, true, G = 1)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("14"))),
	B = 1.
'CHRor_3'(or(A, A, B), C, D, E) ?-
	coca(try_rule(E, or(A, A, B), anonymous("15"), or(F, F, G), replacement, true, F = G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("15"))),
	A = B.
'CHRor_3'(or(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRor_3__77'(G, [C, A], [], H),
	coca(try_double(F, or(A, B, C), H, neg(A, C), or(I, J, K), neg(I, K), replacement, true, (I = 0, J = 1, K = 1), anonymous("42"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("42"))),
	[A, [B, C]] = [0, [1, 1]].
'CHRor_3'(or(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRor_3__78'(G, [C, A], [], H),
	coca(try_double(F, or(A, B, C), H, neg(C, A), or(I, J, K), neg(K, I), replacement, true, (I = 0, J = 1, K = 1), anonymous("43"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("43"))),
	[A, [B, C]] = [0, [1, 1]].
'CHRor_3'(or(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRor_3__79'(G, [C, B], [], H),
	coca(try_double(F, or(A, B, C), H, neg(B, C), or(I, J, K), neg(J, K), replacement, true, (I = 1, J = 0, K = 1), anonymous("44"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("44"))),
	[A, [B, C]] = [1, [0, 1]].
'CHRor_3'(or(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRor_3__80'(G, [C, B], [], H),
	coca(try_double(F, or(A, B, C), H, neg(C, B), or(I, J, K), neg(K, J), replacement, true, (I = 1, J = 0, K = 1), anonymous("45"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("45"))),
	[A, [B, C]] = [1, [0, 1]].
'CHRor_3'(or(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRor_3__81'(G, [B, A], [H], I),
	coca(try_double(F, or(A, B, C), I, or(A, B, H), or(J, K, L), or(J, K, M), keep_second, true, M = L, anonymous("16"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("16"))),
	H = C.
'CHRor_3'(or(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRor_3__82'(G, [B, A], [H], I),
	coca(try_double(F, or(A, B, C), I, or(B, A, H), or(J, K, L), or(K, J, M), keep_second, true, M = L, anonymous("17"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("17"))),
	H = C.
'CHRor_3'(or(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRor_3__83'(G, [B, A], [], H),
	coca(try_double(F, or(A, B, C), H, neg(A, B), or(I, J, K), neg(I, J), keep_second, true, K = 1, anonymous("40"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("40"))),
	C = 1.
'CHRor_3'(or(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRor_3__84'(G, [B, A], [], H),
	coca(try_double(F, or(A, B, C), H, neg(B, A), or(I, J, K), neg(J, I), keep_second, true, K = 1, anonymous("41"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("41"))),
	C = 1.
'CHRor_3'(or(A, B, C), D, E, F) :-
	'CHRor_3__76'(or(A, B, C), D, E, F).
'CHRor_3__77'(['CHRneg_2'(neg(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRor_3__77'([A|B], C, D, E) :-
	'CHRor_3__77'(B, C, D, E).
:- set_flag('CHRor_3__77' / 4, leash, notrace).
'CHRor_3__78'(['CHRneg_2'(neg(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRor_3__78'([A|B], C, D, E) :-
	'CHRor_3__78'(B, C, D, E).
:- set_flag('CHRor_3__78' / 4, leash, notrace).
'CHRor_3__79'(['CHRneg_2'(neg(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRor_3__79'([A|B], C, D, E) :-
	'CHRor_3__79'(B, C, D, E).
:- set_flag('CHRor_3__79' / 4, leash, notrace).
'CHRor_3__80'(['CHRneg_2'(neg(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRor_3__80'([A|B], C, D, E) :-
	'CHRor_3__80'(B, C, D, E).
:- set_flag('CHRor_3__80' / 4, leash, notrace).
'CHRor_3__81'(['CHRor_3'(or(A, B, C), D, E, F)|G], [B, A], [H], I) ?-
	'CHRvar'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHRor_3__81'([A|B], C, D, E) :-
	'CHRor_3__81'(B, C, D, E).
:- set_flag('CHRor_3__81' / 4, leash, notrace).
'CHRor_3__82'(['CHRor_3'(or(A, B, C), D, E, F)|G], [A, B], [H], I) ?-
	'CHRvar'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHRor_3__82'([A|B], C, D, E) :-
	'CHRor_3__82'(B, C, D, E).
:- set_flag('CHRor_3__82' / 4, leash, notrace).
'CHRor_3__83'(['CHRneg_2'(neg(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRor_3__83'([A|B], C, D, E) :-
	'CHRor_3__83'(B, C, D, E).
:- set_flag('CHRor_3__83' / 4, leash, notrace).
'CHRor_3__84'(['CHRneg_2'(neg(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRor_3__84'([A|B], C, D, E) :-
	'CHRor_3__84'(B, C, D, E).
:- set_flag('CHRor_3__84' / 4, leash, notrace).
:- set_flag('CHRor_3' / 4, leash, notrace).
:- current_macro('CHRor_3' / 4, _27194, _27195, _27196) -> true ; define_macro('CHRor_3' / 4, tr_chr / 2, [write]).
'CHRor_3__76'(A, B, C, D) :-
	'CHRor_3__85'(A, B, C, D).
:- set_flag('CHRor_3__76' / 4, leash, notrace).
'CHRor_3__85'(or(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRor_3__85__86'(G, D, or(A, B, C), E, F).
'CHRor_3__85'(or(A, B, C), D, E, F) :-
	'CHRor_3__85__87'(or(A, B, C), D, E, F).
:- set_flag('CHRor_3__85' / 4, leash, notrace).
'CHRor_3__85__86'(['CHRor_3'(or(A, B, C), D, E, F)|G], H, or(A, B, I), J, K) ?-
	'CHRvar'(D),
	coca(try_double(K, or(A, B, I), F, or(A, B, C), or(L, M, N), or(L, M, O), keep_first, true, N = O, anonymous("16"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("16"))),
	'CHRor_3__85__86'(G, H, or(A, B, I), J, K),
	I = C.
'CHRor_3__85__86'([A|B], C, D, E, F) :-
	'CHRor_3__85__86'(B, C, D, E, F).
'CHRor_3__85__86'([], A, B, C, D) :-
	'CHRor_3__85__87'(B, A, C, D).
:- set_flag('CHRor_3__85__86' / 5, leash, notrace).
'CHRor_3__85__87'(or(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRor_3__85__87__88'(G, D, or(A, B, C), E, F).
'CHRor_3__85__87'(or(A, B, C), D, E, F) :-
	'CHRor_3__85__87__89'(or(A, B, C), D, E, F).
:- set_flag('CHRor_3__85__87' / 4, leash, notrace).
'CHRor_3__85__87__88'(['CHRor_3'(or(A, B, C), D, E, F)|G], H, or(B, A, I), J, K) ?-
	'CHRvar'(D),
	coca(try_double(K, or(B, A, I), F, or(A, B, C), or(L, M, N), or(M, L, O), keep_first, true, N = O, anonymous("17"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("17"))),
	'CHRor_3__85__87__88'(G, H, or(B, A, I), J, K),
	I = C.
'CHRor_3__85__87__88'([A|B], C, D, E, F) :-
	'CHRor_3__85__87__88'(B, C, D, E, F).
'CHRor_3__85__87__88'([], A, B, C, D) :-
	'CHRor_3__85__87__89'(B, A, C, D).
:- set_flag('CHRor_3__85__87__88' / 5, leash, notrace).
'CHRor_3__85__87__89'(or(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, or(A, B, C)], 'CHRor_3'(or(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRor_3__85__87__89' / 4, leash, notrace).



%%% Prolog clauses for exor / 3

clause_exor(0, A, A).
clause_exor(1, A, B) :-
	neg(A, B).
:- current_macro(clause_exor / 3, _28726, _28727, _28728) -> true ; define_macro(clause_exor / 3, tr_chr / 2, [write]).
exor(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, exor(A, B, C))),
	'CHRexor_3'(exor(A, B, C), E, F, D).



%%% Rules handling for exor / 3

'CHRexor_3'(exor(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHRexor_3'(exor(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHRexor_3'(exor(0, A, B), C, D, E) ?-
	coca(try_rule(E, exor(0, A, B), anonymous("18"), exor(0, F, G), replacement, true, F = G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("18"))),
	A = B.
'CHRexor_3'(exor(A, 0, B), C, D, E) ?-
	coca(try_rule(E, exor(A, 0, B), anonymous("19"), exor(F, 0, G), replacement, true, F = G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("19"))),
	A = B.
'CHRexor_3'(exor(A, B, 0), C, D, E) ?-
	coca(try_rule(E, exor(A, B, 0), anonymous("20"), exor(F, G, 0), replacement, true, F = G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("20"))),
	A = B.
'CHRexor_3'(exor(1, A, B), C, D, E) ?-
	coca(try_rule(E, exor(1, A, B), anonymous("21"), exor(1, F, G), replacement, true, neg(F, G))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("21"))),
	neg(A, B).
'CHRexor_3'(exor(A, 1, B), C, D, E) ?-
	coca(try_rule(E, exor(A, 1, B), anonymous("22"), exor(F, 1, G), replacement, true, neg(F, G))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("22"))),
	neg(A, B).
'CHRexor_3'(exor(A, B, 1), C, D, E) ?-
	coca(try_rule(E, exor(A, B, 1), anonymous("23"), exor(F, G, 1), replacement, true, neg(F, G))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("23"))),
	neg(A, B).
'CHRexor_3'(exor(A, A, B), C, D, E) ?-
	coca(try_rule(E, exor(A, A, B), anonymous("24"), exor(F, F, G), replacement, true, G = 0)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("24"))),
	B = 0.
'CHRexor_3'(exor(A, B, A), C, D, E) ?-
	coca(try_rule(E, exor(A, B, A), anonymous("25"), exor(F, G, F), replacement, true, G = 0)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("25"))),
	B = 0.
'CHRexor_3'(exor(A, B, B), C, D, E) ?-
	coca(try_rule(E, exor(A, B, B), anonymous("26"), exor(F, G, G), replacement, true, F = 0)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("26"))),
	A = 0.
'CHRexor_3'(exor(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRexor_3__91'(G, [B, A], [H], I),
	coca(try_double(F, exor(A, B, C), I, exor(A, B, H), exor(J, K, L), exor(J, K, M), keep_second, true, M = L, anonymous("27"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("27"))),
	H = C.
'CHRexor_3'(exor(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRexor_3__92'(G, [B, A], [H], I),
	coca(try_double(F, exor(A, B, C), I, exor(B, A, H), exor(J, K, L), exor(K, J, M), keep_second, true, M = L, anonymous("28"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("28"))),
	H = C.
'CHRexor_3'(exor(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRexor_3__93'(G, [B, A], [], H),
	coca(try_double(F, exor(A, B, C), H, neg(A, B), exor(I, J, K), neg(I, J), keep_second, true, K = 1, anonymous("46"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("46"))),
	C = 1.
'CHRexor_3'(exor(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRexor_3__94'(G, [B, A], [], H),
	coca(try_double(F, exor(A, B, C), H, neg(B, A), exor(I, J, K), neg(J, I), keep_second, true, K = 1, anonymous("47"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("47"))),
	C = 1.
'CHRexor_3'(exor(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRexor_3__95'(G, [C, A], [], H),
	coca(try_double(F, exor(A, B, C), H, neg(A, C), exor(I, J, K), neg(I, K), keep_second, true, J = 1, anonymous("48"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("48"))),
	B = 1.
'CHRexor_3'(exor(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRexor_3__96'(G, [C, A], [], H),
	coca(try_double(F, exor(A, B, C), H, neg(C, A), exor(I, J, K), neg(K, I), keep_second, true, J = 1, anonymous("49"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("49"))),
	B = 1.
'CHRexor_3'(exor(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRexor_3__97'(G, [C, B], [], H),
	coca(try_double(F, exor(A, B, C), H, neg(B, C), exor(I, J, K), neg(J, K), keep_second, true, I = 1, anonymous("50"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("50"))),
	A = 1.
'CHRexor_3'(exor(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRexor_3__98'(G, [C, B], [], H),
	coca(try_double(F, exor(A, B, C), H, neg(C, B), exor(I, J, K), neg(K, J), keep_second, true, I = 1, anonymous("51"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("51"))),
	A = 1.
'CHRexor_3'(exor(A, B, C), D, E, F) :-
	'CHRexor_3__90'(exor(A, B, C), D, E, F).
'CHRexor_3__91'(['CHRexor_3'(exor(A, B, C), D, E, F)|G], [B, A], [H], I) ?-
	'CHRvar'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHRexor_3__91'([A|B], C, D, E) :-
	'CHRexor_3__91'(B, C, D, E).
:- set_flag('CHRexor_3__91' / 4, leash, notrace).
'CHRexor_3__92'(['CHRexor_3'(exor(A, B, C), D, E, F)|G], [A, B], [H], I) ?-
	'CHRvar'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHRexor_3__92'([A|B], C, D, E) :-
	'CHRexor_3__92'(B, C, D, E).
:- set_flag('CHRexor_3__92' / 4, leash, notrace).
'CHRexor_3__93'(['CHRneg_2'(neg(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRexor_3__93'([A|B], C, D, E) :-
	'CHRexor_3__93'(B, C, D, E).
:- set_flag('CHRexor_3__93' / 4, leash, notrace).
'CHRexor_3__94'(['CHRneg_2'(neg(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRexor_3__94'([A|B], C, D, E) :-
	'CHRexor_3__94'(B, C, D, E).
:- set_flag('CHRexor_3__94' / 4, leash, notrace).
'CHRexor_3__95'(['CHRneg_2'(neg(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRexor_3__95'([A|B], C, D, E) :-
	'CHRexor_3__95'(B, C, D, E).
:- set_flag('CHRexor_3__95' / 4, leash, notrace).
'CHRexor_3__96'(['CHRneg_2'(neg(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRexor_3__96'([A|B], C, D, E) :-
	'CHRexor_3__96'(B, C, D, E).
:- set_flag('CHRexor_3__96' / 4, leash, notrace).
'CHRexor_3__97'(['CHRneg_2'(neg(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRexor_3__97'([A|B], C, D, E) :-
	'CHRexor_3__97'(B, C, D, E).
:- set_flag('CHRexor_3__97' / 4, leash, notrace).
'CHRexor_3__98'(['CHRneg_2'(neg(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRexor_3__98'([A|B], C, D, E) :-
	'CHRexor_3__98'(B, C, D, E).
:- set_flag('CHRexor_3__98' / 4, leash, notrace).
:- set_flag('CHRexor_3' / 4, leash, notrace).
:- current_macro('CHRexor_3' / 4, _34041, _34042, _34043) -> true ; define_macro('CHRexor_3' / 4, tr_chr / 2, [write]).
'CHRexor_3__90'(A, B, C, D) :-
	'CHRexor_3__99'(A, B, C, D).
:- set_flag('CHRexor_3__90' / 4, leash, notrace).
'CHRexor_3__99'(exor(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRexor_3__99__100'(G, D, exor(A, B, C), E, F).
'CHRexor_3__99'(exor(A, B, C), D, E, F) :-
	'CHRexor_3__99__101'(exor(A, B, C), D, E, F).
:- set_flag('CHRexor_3__99' / 4, leash, notrace).
'CHRexor_3__99__100'(['CHRexor_3'(exor(A, B, C), D, E, F)|G], H, exor(A, B, I), J, K) ?-
	'CHRvar'(D),
	coca(try_double(K, exor(A, B, I), F, exor(A, B, C), exor(L, M, N), exor(L, M, O), keep_first, true, N = O, anonymous("27"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("27"))),
	'CHRexor_3__99__100'(G, H, exor(A, B, I), J, K),
	I = C.
'CHRexor_3__99__100'([A|B], C, D, E, F) :-
	'CHRexor_3__99__100'(B, C, D, E, F).
'CHRexor_3__99__100'([], A, B, C, D) :-
	'CHRexor_3__99__101'(B, A, C, D).
:- set_flag('CHRexor_3__99__100' / 5, leash, notrace).
'CHRexor_3__99__101'(exor(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRexor_3__99__101__102'(G, D, exor(A, B, C), E, F).
'CHRexor_3__99__101'(exor(A, B, C), D, E, F) :-
	'CHRexor_3__99__101__103'(exor(A, B, C), D, E, F).
:- set_flag('CHRexor_3__99__101' / 4, leash, notrace).
'CHRexor_3__99__101__102'(['CHRexor_3'(exor(A, B, C), D, E, F)|G], H, exor(B, A, I), J, K) ?-
	'CHRvar'(D),
	coca(try_double(K, exor(B, A, I), F, exor(A, B, C), exor(L, M, N), exor(M, L, O), keep_first, true, N = O, anonymous("28"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("28"))),
	'CHRexor_3__99__101__102'(G, H, exor(B, A, I), J, K),
	I = C.
'CHRexor_3__99__101__102'([A|B], C, D, E, F) :-
	'CHRexor_3__99__101__102'(B, C, D, E, F).
'CHRexor_3__99__101__102'([], A, B, C, D) :-
	'CHRexor_3__99__101__103'(B, A, C, D).
:- set_flag('CHRexor_3__99__101__102' / 5, leash, notrace).
'CHRexor_3__99__101__103'(exor(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, exor(A, B, C)], 'CHRexor_3'(exor(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRexor_3__99__101__103' / 4, leash, notrace).



%%% Prolog clauses for neg / 2

clause_neg(0, 1).
clause_neg(1, 0).
:- current_macro(clause_neg / 2, _35573, _35574, _35575) -> true ; define_macro(clause_neg / 2, tr_chr / 2, [write]).
neg(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, neg(A, B))),
	'CHRneg_2'(neg(A, B), D, E, C).



%%% Rules handling for neg / 2

'CHRneg_2'(neg(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRneg_2'(neg(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRneg_2'(neg(0, A), B, C, D) ?-
	coca(try_rule(D, neg(0, A), anonymous("29"), neg(0, E), replacement, true, E = 1)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("29"))),
	A = 1.
'CHRneg_2'(neg(A, 0), B, C, D) ?-
	coca(try_rule(D, neg(A, 0), anonymous("30"), neg(E, 0), replacement, true, E = 1)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("30"))),
	A = 1.
'CHRneg_2'(neg(1, A), B, C, D) ?-
	coca(try_rule(D, neg(1, A), anonymous("31"), neg(1, E), replacement, true, E = 0)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("31"))),
	A = 0.
'CHRneg_2'(neg(A, 1), B, C, D) ?-
	coca(try_rule(D, neg(A, 1), anonymous("32"), neg(E, 1), replacement, true, E = 0)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("32"))),
	A = 0.
'CHRneg_2'(neg(A, A), B, C, D) ?-
	coca(try_rule(D, neg(A, A), anonymous("33"), neg(E, E), replacement, true, fail)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("33"))),
	fail.
'CHRneg_2'(neg(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__105'(F, [B, A], [G], H),
	coca(try_double(E, neg(A, B), H, and(A, G, B), neg(I, J), and(I, K, J), replacement, true, (I = 1, K = 0, J = 0), anonymous("36"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("36"))),
	[A, [G, B]] = [1, [0, 0]].
'CHRneg_2'(neg(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__106'(F, [B, A], [G], H),
	coca(try_double(E, neg(A, B), H, and(B, G, A), neg(I, J), and(J, K, I), replacement, true, (J = 1, K = 0, I = 0), anonymous("37"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("37"))),
	[B, [G, A]] = [1, [0, 0]].
'CHRneg_2'(neg(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__107'(F, [B, A], [G], H),
	coca(try_double(E, neg(A, B), H, and(G, A, B), neg(I, J), and(K, I, J), replacement, true, (K = 0, I = 1, J = 0), anonymous("38"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("38"))),
	[G, [A, B]] = [0, [1, 0]].
'CHRneg_2'(neg(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__108'(F, [B, A], [G], H),
	coca(try_double(E, neg(A, B), H, and(G, B, A), neg(I, J), and(K, J, I), replacement, true, (K = 0, J = 1, I = 0), anonymous("39"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("39"))),
	[G, [B, A]] = [0, [1, 0]].
'CHRneg_2'(neg(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__109'(F, [B, A], [G], H),
	coca(try_double(E, neg(A, B), H, or(A, G, B), neg(I, J), or(I, K, J), replacement, true, (I = 0, K = 1, J = 1), anonymous("42"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("42"))),
	[A, [G, B]] = [0, [1, 1]].
'CHRneg_2'(neg(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__110'(F, [B, A], [G], H),
	coca(try_double(E, neg(A, B), H, or(B, G, A), neg(I, J), or(J, K, I), replacement, true, (J = 0, K = 1, I = 1), anonymous("43"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("43"))),
	[B, [G, A]] = [0, [1, 1]].
'CHRneg_2'(neg(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__111'(F, [B, A], [G], H),
	coca(try_double(E, neg(A, B), H, or(G, A, B), neg(I, J), or(K, I, J), replacement, true, (K = 1, I = 0, J = 1), anonymous("44"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("44"))),
	[G, [A, B]] = [1, [0, 1]].
'CHRneg_2'(neg(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__112'(F, [B, A], [G], H),
	coca(try_double(E, neg(A, B), H, or(G, B, A), neg(I, J), or(K, J, I), replacement, true, (K = 1, J = 0, I = 1), anonymous("45"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("45"))),
	[G, [B, A]] = [1, [0, 1]].
'CHRneg_2'(neg(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__113'(F, [B, A], [], G),
	coca(try_double(E, neg(A, B), G, imp(A, B), neg(H, I), imp(H, I), replacement, true, (H = 0, I = 1), anonymous("52"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("52"))),
	[A, B] = [0, 1].
'CHRneg_2'(neg(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__114'(F, [B, A], [], G),
	coca(try_double(E, neg(A, B), G, imp(B, A), neg(H, I), imp(I, H), replacement, true, (I = 0, H = 1), anonymous("53"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("53"))),
	[B, A] = [0, 1].
'CHRneg_2'(neg(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHRneg_2__115'(F, [A], [G], H),
	coca(try_double(E, neg(A, B), H, neg(G, A), neg(I, J), neg(K, I), keep_second, true, K = J, neg_neg)),
	!,
	'CHRkill'(C),
	coca(fired_rule(neg_neg)),
	G = B.
'CHRneg_2'(neg(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__116'(F, [B], [G], H),
	coca(try_double(E, neg(A, B), H, neg(G, B), neg(I, J), neg(K, J), keep_second, true, K = I, neg_neg)),
	!,
	'CHRkill'(C),
	coca(fired_rule(neg_neg)),
	G = A.
'CHRneg_2'(neg(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHRneg_2__117'(F, [A], [G], H),
	coca(try_double(E, neg(A, B), H, neg(A, G), neg(I, J), neg(I, K), keep_second, true, K = J, neg_neg)),
	!,
	'CHRkill'(C),
	coca(fired_rule(neg_neg)),
	G = B.
'CHRneg_2'(neg(A, B), C, D, E) :-
	'CHRneg_2__104'(neg(A, B), C, D, E).
'CHRneg_2__105'(['CHRand_3'(and(A, B, C), D, E, F)|G], [C, A], [H], I) ?-
	'CHRvar'(D),
	'CHRkill'(D),
	'CHR='([B], [H]),
	'CHR='(F, I).
'CHRneg_2__105'([A|B], C, D, E) :-
	'CHRneg_2__105'(B, C, D, E).
:- set_flag('CHRneg_2__105' / 4, leash, notrace).
'CHRneg_2__106'(['CHRand_3'(and(A, B, C), D, E, F)|G], [A, C], [H], I) ?-
	'CHRvar'(D),
	'CHRkill'(D),
	'CHR='([B], [H]),
	'CHR='(F, I).
'CHRneg_2__106'([A|B], C, D, E) :-
	'CHRneg_2__106'(B, C, D, E).
:- set_flag('CHRneg_2__106' / 4, leash, notrace).
'CHRneg_2__107'(['CHRand_3'(and(A, B, C), D, E, F)|G], [C, B], [H], I) ?-
	'CHRvar'(D),
	'CHRkill'(D),
	'CHR='([A], [H]),
	'CHR='(F, I).
'CHRneg_2__107'([A|B], C, D, E) :-
	'CHRneg_2__107'(B, C, D, E).
:- set_flag('CHRneg_2__107' / 4, leash, notrace).
'CHRneg_2__108'(['CHRand_3'(and(A, B, C), D, E, F)|G], [B, C], [H], I) ?-
	'CHRvar'(D),
	'CHRkill'(D),
	'CHR='([A], [H]),
	'CHR='(F, I).
'CHRneg_2__108'([A|B], C, D, E) :-
	'CHRneg_2__108'(B, C, D, E).
:- set_flag('CHRneg_2__108' / 4, leash, notrace).
'CHRneg_2__109'(['CHRor_3'(or(A, B, C), D, E, F)|G], [C, A], [H], I) ?-
	'CHRvar'(D),
	'CHRkill'(D),
	'CHR='([B], [H]),
	'CHR='(F, I).
'CHRneg_2__109'([A|B], C, D, E) :-
	'CHRneg_2__109'(B, C, D, E).
:- set_flag('CHRneg_2__109' / 4, leash, notrace).
'CHRneg_2__110'(['CHRor_3'(or(A, B, C), D, E, F)|G], [A, C], [H], I) ?-
	'CHRvar'(D),
	'CHRkill'(D),
	'CHR='([B], [H]),
	'CHR='(F, I).
'CHRneg_2__110'([A|B], C, D, E) :-
	'CHRneg_2__110'(B, C, D, E).
:- set_flag('CHRneg_2__110' / 4, leash, notrace).
'CHRneg_2__111'(['CHRor_3'(or(A, B, C), D, E, F)|G], [C, B], [H], I) ?-
	'CHRvar'(D),
	'CHRkill'(D),
	'CHR='([A], [H]),
	'CHR='(F, I).
'CHRneg_2__111'([A|B], C, D, E) :-
	'CHRneg_2__111'(B, C, D, E).
:- set_flag('CHRneg_2__111' / 4, leash, notrace).
'CHRneg_2__112'(['CHRor_3'(or(A, B, C), D, E, F)|G], [B, C], [H], I) ?-
	'CHRvar'(D),
	'CHRkill'(D),
	'CHR='([A], [H]),
	'CHR='(F, I).
'CHRneg_2__112'([A|B], C, D, E) :-
	'CHRneg_2__112'(B, C, D, E).
:- set_flag('CHRneg_2__112' / 4, leash, notrace).
'CHRneg_2__113'(['CHRimp_2'(imp(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRneg_2__113'([A|B], C, D, E) :-
	'CHRneg_2__113'(B, C, D, E).
:- set_flag('CHRneg_2__113' / 4, leash, notrace).
'CHRneg_2__114'(['CHRimp_2'(imp(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRneg_2__114'([A|B], C, D, E) :-
	'CHRneg_2__114'(B, C, D, E).
:- set_flag('CHRneg_2__114' / 4, leash, notrace).
'CHRneg_2__115'(['CHRneg_2'(neg(A, B), C, D, E)|F], [B], [G], H) ?-
	'CHRvar'(C),
	'CHR='([A], [G]),
	'CHR='(E, H).
'CHRneg_2__115'([A|B], C, D, E) :-
	'CHRneg_2__115'(B, C, D, E).
:- set_flag('CHRneg_2__115' / 4, leash, notrace).
'CHRneg_2__116'(['CHRneg_2'(neg(A, B), C, D, E)|F], [B], [G], H) ?-
	'CHRvar'(C),
	'CHR='([A], [G]),
	'CHR='(E, H).
'CHRneg_2__116'([A|B], C, D, E) :-
	'CHRneg_2__116'(B, C, D, E).
:- set_flag('CHRneg_2__116' / 4, leash, notrace).
'CHRneg_2__117'(['CHRneg_2'(neg(A, B), C, D, E)|F], [A], [G], H) ?-
	'CHRvar'(C),
	'CHR='([B], [G]),
	'CHR='(E, H).
'CHRneg_2__117'([A|B], C, D, E) :-
	'CHRneg_2__117'(B, C, D, E).
:- set_flag('CHRneg_2__117' / 4, leash, notrace).
:- set_flag('CHRneg_2' / 4, leash, notrace).
:- current_macro('CHRneg_2' / 4, _43373, _43374, _43375) -> true ; define_macro('CHRneg_2' / 4, tr_chr / 2, [write]).
'CHRneg_2__104'(A, B, C, D) :-
	'CHRneg_2__118'(A, B, C, D).
:- set_flag('CHRneg_2__104' / 4, leash, notrace).
'CHRneg_2__118'(neg(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__118__119'(F, C, neg(A, B), D, E).
'CHRneg_2__118'(neg(A, B), C, D, E) :-
	'CHRneg_2__118__120'(neg(A, B), C, D, E).
:- set_flag('CHRneg_2__118' / 4, leash, notrace).
'CHRneg_2__118__119'(['CHRneg_2'(neg(A, B), C, D, E)|F], G, neg(H, A), I, J) ?-
	'CHRvar'(C),
	coca(try_double(J, neg(H, A), E, neg(A, B), neg(K, L), neg(L, M), keep_first, true, K = M, neg_neg)),
	!,
	'CHRkill'(C),
	coca(fired_rule(neg_neg)),
	'CHRneg_2__118__119'(F, G, neg(H, A), I, J),
	H = B.
'CHRneg_2__118__119'([A|B], C, D, E, F) :-
	'CHRneg_2__118__119'(B, C, D, E, F).
'CHRneg_2__118__119'([], A, B, C, D) :-
	'CHRneg_2__118__120'(B, A, C, D).
:- set_flag('CHRneg_2__118__119' / 5, leash, notrace).
'CHRneg_2__118__120'(neg(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__118__120__121'(F, C, neg(A, B), D, E).
'CHRneg_2__118__120'(neg(A, B), C, D, E) :-
	'CHRneg_2__118__120__122'(neg(A, B), C, D, E).
:- set_flag('CHRneg_2__118__120' / 4, leash, notrace).
'CHRneg_2__118__120__121'(['CHRneg_2'(neg(A, B), C, D, E)|F], G, neg(H, B), I, J) ?-
	'CHRvar'(C),
	coca(try_double(J, neg(H, B), E, neg(A, B), neg(K, L), neg(M, L), keep_first, true, K = M, neg_neg)),
	!,
	'CHRkill'(C),
	coca(fired_rule(neg_neg)),
	'CHRneg_2__118__120__121'(F, G, neg(H, B), I, J),
	H = A.
'CHRneg_2__118__120__121'([A|B], C, D, E, F) :-
	'CHRneg_2__118__120__121'(B, C, D, E, F).
'CHRneg_2__118__120__121'([], A, B, C, D) :-
	'CHRneg_2__118__120__122'(B, A, C, D).
:- set_flag('CHRneg_2__118__120__121' / 5, leash, notrace).
'CHRneg_2__118__120__122'(neg(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHRneg_2__118__120__122__123'(F, C, neg(A, B), D, E).
'CHRneg_2__118__120__122'(neg(A, B), C, D, E) :-
	'CHRneg_2__118__120__122__124'(neg(A, B), C, D, E).
:- set_flag('CHRneg_2__118__120__122' / 4, leash, notrace).
'CHRneg_2__118__120__122__123'(['CHRneg_2'(neg(A, B), C, D, E)|F], G, neg(A, H), I, J) ?-
	'CHRvar'(C),
	coca(try_double(J, neg(A, H), E, neg(A, B), neg(K, L), neg(K, M), keep_first, true, L = M, neg_neg)),
	!,
	'CHRkill'(C),
	coca(fired_rule(neg_neg)),
	'CHRneg_2__118__120__122__123'(F, G, neg(A, H), I, J),
	H = B.
'CHRneg_2__118__120__122__123'([A|B], C, D, E, F) :-
	'CHRneg_2__118__120__122__123'(B, C, D, E, F).
'CHRneg_2__118__120__122__123'([], A, B, C, D) :-
	'CHRneg_2__118__120__122__124'(B, A, C, D).
:- set_flag('CHRneg_2__118__120__122__123' / 5, leash, notrace).
'CHRneg_2__118__120__122__124'(neg(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__118__120__122__124__125'(F, C, neg(A, B), D, E).
'CHRneg_2__118__120__122__124'(neg(A, B), C, D, E) :-
	'CHRneg_2__118__120__122__124__126'(neg(A, B), C, D, E).
:- set_flag('CHRneg_2__118__120__122__124' / 4, leash, notrace).
'CHRneg_2__118__120__122__124__125'(['CHRand_3'(and(A, B, C), D, E, F)|G], H, neg(A, B), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, neg(A, B), F, and(A, B, C), neg(K, L), and(K, L, M), keep_first, true, M = 0, anonymous("34"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("34"))),
	'CHRneg_2__118__120__122__124__125'(G, H, neg(A, B), I, J),
	C = 0.
'CHRneg_2__118__120__122__124__125'([A|B], C, D, E, F) :-
	'CHRneg_2__118__120__122__124__125'(B, C, D, E, F).
'CHRneg_2__118__120__122__124__125'([], A, B, C, D) :-
	'CHRneg_2__118__120__122__124__126'(B, A, C, D).
:- set_flag('CHRneg_2__118__120__122__124__125' / 5, leash, notrace).
'CHRneg_2__118__120__122__124__126'(neg(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__118__120__122__124__126__127'(F, C, neg(A, B), D, E).
'CHRneg_2__118__120__122__124__126'(neg(A, B), C, D, E) :-
	'CHRneg_2__118__120__122__124__126__128'(neg(A, B), C, D, E).
:- set_flag('CHRneg_2__118__120__122__124__126' / 4, leash, notrace).
'CHRneg_2__118__120__122__124__126__127'(['CHRand_3'(and(A, B, C), D, E, F)|G], H, neg(B, A), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, neg(B, A), F, and(A, B, C), neg(K, L), and(L, K, M), keep_first, true, M = 0, anonymous("35"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("35"))),
	'CHRneg_2__118__120__122__124__126__127'(G, H, neg(B, A), I, J),
	C = 0.
'CHRneg_2__118__120__122__124__126__127'([A|B], C, D, E, F) :-
	'CHRneg_2__118__120__122__124__126__127'(B, C, D, E, F).
'CHRneg_2__118__120__122__124__126__127'([], A, B, C, D) :-
	'CHRneg_2__118__120__122__124__126__128'(B, A, C, D).
:- set_flag('CHRneg_2__118__120__122__124__126__127' / 5, leash, notrace).
'CHRneg_2__118__120__122__124__126__128'(neg(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__118__120__122__124__126__128__129'(F, C, neg(A, B), D, E).
'CHRneg_2__118__120__122__124__126__128'(neg(A, B), C, D, E) :-
	'CHRneg_2__118__120__122__124__126__128__130'(neg(A, B), C, D, E).
:- set_flag('CHRneg_2__118__120__122__124__126__128' / 4, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__129'(['CHRor_3'(or(A, B, C), D, E, F)|G], H, neg(A, B), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, neg(A, B), F, or(A, B, C), neg(K, L), or(K, L, M), keep_first, true, M = 1, anonymous("40"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("40"))),
	'CHRneg_2__118__120__122__124__126__128__129'(G, H, neg(A, B), I, J),
	C = 1.
'CHRneg_2__118__120__122__124__126__128__129'([A|B], C, D, E, F) :-
	'CHRneg_2__118__120__122__124__126__128__129'(B, C, D, E, F).
'CHRneg_2__118__120__122__124__126__128__129'([], A, B, C, D) :-
	'CHRneg_2__118__120__122__124__126__128__130'(B, A, C, D).
:- set_flag('CHRneg_2__118__120__122__124__126__128__129' / 5, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130'(neg(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__118__120__122__124__126__128__130__131'(F, C, neg(A, B), D, E).
'CHRneg_2__118__120__122__124__126__128__130'(neg(A, B), C, D, E) :-
	'CHRneg_2__118__120__122__124__126__128__130__132'(neg(A, B), C, D, E).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130' / 4, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130__131'(['CHRor_3'(or(A, B, C), D, E, F)|G], H, neg(B, A), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, neg(B, A), F, or(A, B, C), neg(K, L), or(L, K, M), keep_first, true, M = 1, anonymous("41"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("41"))),
	'CHRneg_2__118__120__122__124__126__128__130__131'(G, H, neg(B, A), I, J),
	C = 1.
'CHRneg_2__118__120__122__124__126__128__130__131'([A|B], C, D, E, F) :-
	'CHRneg_2__118__120__122__124__126__128__130__131'(B, C, D, E, F).
'CHRneg_2__118__120__122__124__126__128__130__131'([], A, B, C, D) :-
	'CHRneg_2__118__120__122__124__126__128__130__132'(B, A, C, D).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130__131' / 5, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130__132'(neg(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__118__120__122__124__126__128__130__132__133'(F, C, neg(A, B), D, E).
'CHRneg_2__118__120__122__124__126__128__130__132'(neg(A, B), C, D, E) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134'(neg(A, B), C, D, E).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130__132' / 4, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130__132__133'(['CHRexor_3'(exor(A, B, C), D, E, F)|G], H, neg(A, B), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, neg(A, B), F, exor(A, B, C), neg(K, L), exor(K, L, M), keep_first, true, M = 1, anonymous("46"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("46"))),
	'CHRneg_2__118__120__122__124__126__128__130__132__133'(G, H, neg(A, B), I, J),
	C = 1.
'CHRneg_2__118__120__122__124__126__128__130__132__133'([A|B], C, D, E, F) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__133'(B, C, D, E, F).
'CHRneg_2__118__120__122__124__126__128__130__132__133'([], A, B, C, D) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134'(B, A, C, D).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130__132__133' / 5, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130__132__134'(neg(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__118__120__122__124__126__128__130__132__134__135'(F, C, neg(A, B), D, E).
'CHRneg_2__118__120__122__124__126__128__130__132__134'(neg(A, B), C, D, E) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136'(neg(A, B), C, D, E).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130__132__134' / 4, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130__132__134__135'(['CHRexor_3'(exor(A, B, C), D, E, F)|G], H, neg(B, A), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, neg(B, A), F, exor(A, B, C), neg(K, L), exor(L, K, M), keep_first, true, M = 1, anonymous("47"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("47"))),
	'CHRneg_2__118__120__122__124__126__128__130__132__134__135'(G, H, neg(B, A), I, J),
	C = 1.
'CHRneg_2__118__120__122__124__126__128__130__132__134__135'([A|B], C, D, E, F) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__135'(B, C, D, E, F).
'CHRneg_2__118__120__122__124__126__128__130__132__134__135'([], A, B, C, D) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136'(B, A, C, D).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130__132__134__135' / 5, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136'(neg(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__137'(F, C, neg(A, B), D, E).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136'(neg(A, B), C, D, E) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138'(neg(A, B), C, D, E).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130__132__134__136' / 4, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__137'(['CHRexor_3'(exor(A, B, C), D, E, F)|G], H, neg(A, C), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, neg(A, C), F, exor(A, B, C), neg(K, L), exor(K, M, L), keep_first, true, M = 1, anonymous("48"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("48"))),
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__137'(G, H, neg(A, C), I, J),
	B = 1.
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__137'([A|B], C, D, E, F) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__137'(B, C, D, E, F).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__137'([], A, B, C, D) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138'(B, A, C, D).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130__132__134__136__137' / 5, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138'(neg(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__139'(F, C, neg(A, B), D, E).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138'(neg(A, B), C, D, E) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140'(neg(A, B), C, D, E).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130__132__134__136__138' / 4, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__139'(['CHRexor_3'(exor(A, B, C), D, E, F)|G], H, neg(C, A), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, neg(C, A), F, exor(A, B, C), neg(K, L), exor(L, M, K), keep_first, true, M = 1, anonymous("49"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("49"))),
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__139'(G, H, neg(C, A), I, J),
	B = 1.
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__139'([A|B], C, D, E, F) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__139'(B, C, D, E, F).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__139'([], A, B, C, D) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140'(B, A, C, D).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__139' / 5, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140'(neg(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__141'(F, C, neg(A, B), D, E).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140'(neg(A, B), C, D, E) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142'(neg(A, B), C, D, E).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140' / 4, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__141'(['CHRexor_3'(exor(A, B, C), D, E, F)|G], H, neg(B, C), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, neg(B, C), F, exor(A, B, C), neg(K, L), exor(M, K, L), keep_first, true, M = 1, anonymous("50"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("50"))),
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__141'(G, H, neg(B, C), I, J),
	A = 1.
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__141'([A|B], C, D, E, F) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__141'(B, C, D, E, F).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__141'([], A, B, C, D) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142'(B, A, C, D).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__141' / 5, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142'(neg(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142__143'(F, C, neg(A, B), D, E).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142'(neg(A, B), C, D, E) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142__144'(neg(A, B), C, D, E).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142' / 4, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142__143'(['CHRexor_3'(exor(A, B, C), D, E, F)|G], H, neg(C, B), I, J) ?-
	'CHRvar'(D),
	coca(try_double(J, neg(C, B), F, exor(A, B, C), neg(K, L), exor(M, L, K), keep_first, true, M = 1, anonymous("51"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("51"))),
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142__143'(G, H, neg(C, B), I, J),
	A = 1.
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142__143'([A|B], C, D, E, F) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142__143'(B, C, D, E, F).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142__143'([], A, B, C, D) :-
	'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142__144'(B, A, C, D).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142__143' / 5, leash, notrace).
'CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142__144'(neg(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, neg(A, B)], 'CHRneg_2'(neg(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRneg_2__118__120__122__124__126__128__130__132__134__136__138__140__142__144' / 4, leash, notrace).



%%% Prolog clauses for imp / 2

clause_imp(0, A).
clause_imp(1, 1).
:- current_macro(clause_imp / 2, _51526, _51527, _51528) -> true ; define_macro(clause_imp / 2, tr_chr / 2, [write]).
imp(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, imp(A, B))),
	'CHRimp_2'(imp(A, B), D, E, C).



%%% Rules handling for imp / 2

'CHRimp_2'(imp(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRimp_2'(imp(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRimp_2'(imp(0, A), B, C, D) ?-
	coca(try_rule(D, imp(0, A), anonymous("54"), imp(0, E), replacement, true, true)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("54"))).
'CHRimp_2'(imp(A, 0), B, C, D) ?-
	coca(try_rule(D, imp(A, 0), anonymous("55"), imp(E, 0), replacement, true, E = 0)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("55"))),
	A = 0.
'CHRimp_2'(imp(1, A), B, C, D) ?-
	coca(try_rule(D, imp(1, A), anonymous("56"), imp(1, E), replacement, true, E = 1)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("56"))),
	A = 1.
'CHRimp_2'(imp(A, 1), B, C, D) ?-
	coca(try_rule(D, imp(A, 1), anonymous("57"), imp(E, 1), replacement, true, true)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("57"))).
'CHRimp_2'(imp(A, A), B, C, D) ?-
	coca(try_rule(D, imp(A, A), anonymous("58"), imp(E, E), replacement, true, true)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("58"))).
'CHRimp_2'(imp(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRimp_2__146'(F, [B, A], [], G),
	coca(try_double(E, imp(A, B), G, neg(A, B), imp(H, I), neg(H, I), replacement, true, (H = 0, I = 1), anonymous("52"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("52"))),
	[A, B] = [0, 1].
'CHRimp_2'(imp(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRimp_2__147'(F, [B, A], [], G),
	coca(try_double(E, imp(A, B), G, neg(B, A), imp(H, I), neg(I, H), replacement, true, (H = 0, I = 1), anonymous("53"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("53"))),
	[A, B] = [0, 1].
'CHRimp_2'(imp(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRimp_2__148'(F, [B, A], [], G),
	coca(try_double(E, imp(A, B), G, imp(B, A), imp(H, I), imp(I, H), replacement, true, H = I, anonymous("59"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("59"))),
	A = B.
'CHRimp_2'(imp(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRimp_2__149'(F, [B, A], [], G),
	coca(try_double(E, imp(A, B), G, imp(B, A), imp(H, I), imp(I, H), replacement, true, I = H, anonymous("59"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("59"))),
	B = A.
'CHRimp_2'(imp(A, B), C, D, E) :-
	'CHRimp_2__145'(imp(A, B), C, D, E).
'CHRimp_2__146'(['CHRneg_2'(neg(A, B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRimp_2__146'([A|B], C, D, E) :-
	'CHRimp_2__146'(B, C, D, E).
:- set_flag('CHRimp_2__146' / 4, leash, notrace).
'CHRimp_2__147'(['CHRneg_2'(neg(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRimp_2__147'([A|B], C, D, E) :-
	'CHRimp_2__147'(B, C, D, E).
:- set_flag('CHRimp_2__147' / 4, leash, notrace).
'CHRimp_2__148'(['CHRimp_2'(imp(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRimp_2__148'([A|B], C, D, E) :-
	'CHRimp_2__148'(B, C, D, E).
:- set_flag('CHRimp_2__148' / 4, leash, notrace).
'CHRimp_2__149'(['CHRimp_2'(imp(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRimp_2__149'([A|B], C, D, E) :-
	'CHRimp_2__149'(B, C, D, E).
:- set_flag('CHRimp_2__149' / 4, leash, notrace).
:- set_flag('CHRimp_2' / 4, leash, notrace).
:- current_macro('CHRimp_2' / 4, _54488, _54489, _54490) -> true ; define_macro('CHRimp_2' / 4, tr_chr / 2, [write]).
'CHRimp_2__145'(A, B, C, D) :-
	'CHRimp_2__150'(A, B, C, D).
:- set_flag('CHRimp_2__145' / 4, leash, notrace).
'CHRimp_2__150'(imp(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, imp(A, B)], 'CHRimp_2'(imp(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRimp_2__150' / 4, leash, notrace).



%%% Prolog clauses for card / 4

clause_card(A, B, [], 0) :-
	A =< 0,
	0 =< B.
clause_card(A, B, [0|C], D) :-
	E is D - 1,
	card(A, B, C).
clause_card(A, B, [1|C], D) :-
	E is A - 1,
	F is B - 1,
	G is D - 1,
	card(E, F, C).
:- current_macro(clause_card / 4, _54950, _54951, _54952) -> true ; define_macro(clause_card / 4, tr_chr / 2, [write]).
card(A, B, C, D) :-
	'CHRgen_num'(E),
	coca(add_one_constraint(E, card(A, B, C, D))),
	'CHRcard_4'(card(A, B, C, D), F, G, E).



%%% Rules handling for card / 4

'CHRcard_4'(card(A, B, C, D), E, F, G) :-
	(
	    'CHRnonvar'(E)
	;
	    'CHRalready_in'('CHRcard_4'(card(A, B, C, D), E, F, G)),
	    coca(already_in)
	),
	!.
'CHRcard_4'(card(A, B, C, D), E, F, G) ?-
	coca(try_rule(G, card(A, B, C, D), triv_sat, card(H, I, J, K), replacement, (H =< 0, K =< I), true)),
	no_delayed_goals((A =< 0, D =< B)),
	!,
	'CHRkill'(E),
	coca(fired_rule(triv_sat)).
'CHRcard_4'(card(A, B, C, A), D, E, F) ?-
	coca(try_rule(F, card(A, B, C, A), pos_sat, card(G, H, I, G), replacement, true, set_to_ones(I))),
	!,
	'CHRkill'(D),
	coca(fired_rule(pos_sat)),
	set_to_ones(C).
'CHRcard_4'(card(A, 0, B, C), D, E, F) ?-
	coca(try_rule(F, card(A, 0, B, C), neg_sat, card(G, 0, H, I), replacement, true, set_to_zeros(H))),
	!,
	'CHRkill'(D),
	coca(fired_rule(neg_sat)),
	set_to_zeros(B).
'CHRcard_4'(card(A, B, C, D), E, F, G) ?-
	coca(try_rule(G, card(A, B, C, D), pos_red, card(H, I, J, K), replacement, (delete(L, J, M), L == 1), (N is H - 1, O is I - 1, P is K - 1, card(N, O, M, P)))),
	no_delayed_goals((delete(Q, C, R), Q == 1)),
	!,
	'CHRkill'(E),
	coca(fired_rule(pos_red)),
	S is A - 1,
	T is B - 1,
	U is D - 1,
	card(S, T, R, U).
'CHRcard_4'(card(A, B, C, D), E, F, G) ?-
	coca(try_rule(G, card(A, B, C, D), neg_red, card(H, I, J, K), replacement, (delete(L, J, M), L == 0), (N is K - 1, card(H, I, M, N)))),
	no_delayed_goals((delete(O, C, P), O == 0)),
	!,
	'CHRkill'(E),
	coca(fired_rule(neg_red)),
	Q is D - 1,
	card(A, B, P, Q).
'CHRcard_4'(card(0, 1, [A, B], 2), C, D, E) ?-
	coca(try_rule(E, card(0, 1, [A, B], 2), card2nand, card(0, 1, [F, G], 2), replacement, true, and(F, G, 0))),
	!,
	'CHRkill'(C),
	coca(fired_rule(card2nand)),
	and(A, B, 0).
'CHRcard_4'(card(1, 1, [A, B], 2), C, D, E) ?-
	coca(try_rule(E, card(1, 1, [A, B], 2), card2neg, card(1, 1, [F, G], 2), replacement, true, neg(F, G))),
	!,
	'CHRkill'(C),
	coca(fired_rule(card2neg)),
	neg(A, B).
'CHRcard_4'(card(1, 2, [A, B], 2), C, D, E) ?-
	coca(try_rule(E, card(1, 2, [A, B], 2), card2or, card(1, 2, [F, G], 2), replacement, true, or(F, G, 1))),
	!,
	'CHRkill'(C),
	coca(fired_rule(card2or)),
	or(A, B, 1).
'CHRcard_4'(card(A, B, C, D), E, F, G) :-
	'CHRcard_4__151'(card(A, B, C, D), E, F, G).
:- set_flag('CHRcard_4' / 4, leash, notrace).
:- current_macro('CHRcard_4' / 4, _56673, _56674, _56675) -> true ; define_macro('CHRcard_4' / 4, tr_chr / 2, [write]).
'CHRcard_4__151'(A, B, C, D) :-
	'CHRcard_4__152'(A, B, C, D).
:- set_flag('CHRcard_4__151' / 4, leash, notrace).
'CHRcard_4__152'(card(A, B, C, D), E, F, G) :-
	(
	    'CHRvar'(E)
	->
	    'CHRdelay'([E, card(A, B, C, D)], 'CHRcard_4'(card(A, B, C, D), E, F, G))
	;
	    true
	).
:- set_flag('CHRcard_4__152' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
