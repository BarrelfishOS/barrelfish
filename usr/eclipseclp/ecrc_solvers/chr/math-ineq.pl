
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- local (<) / 2, (>) / 2, (=\=) / 2, (>=) / 2, (=<) / 2, (=:=) / 2.
:- ['math-utilities'].
A < B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A < B)),
	'CHR<_2'(A < B, D, E, C).



%%% Rules handling for < / 2

'CHR<_2'(A < B, C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHR<_2'(A < B, C, D, E) ?-
	coca(try_rule(E, A < B, anonymous("2"), F < G, replacement, (ground(F), ground(G)), call_kernel(F < G))),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("2"))),
	call_kernel(A < B).
'CHR<_2'(A < B, C, D, E) ?-
	coca(try_rule(E, A < B, anonymous("7"), F < G, replacement, true, (F + slack(H) =:= G, call_kernel(H > 0)))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("7"))),
	A + slack(I) =:= B,
	call_kernel(I > 0).
'CHR<_2'(A < B, C, D, E) :-
	'CHR<_2__16'(A < B, C, D, E).
:- set_flag('CHR<_2' / 4, leash, notrace).
:- current_macro('CHR<_2' / 4, _3427, _3428, _3429) -> true ; define_macro('CHR<_2' / 4, tr_chr / 2, [write]).
'CHR<_2__16'(A, B, C, D) :-
	'CHR<_2__17'(A, B, C, D).
:- set_flag('CHR<_2__16' / 4, leash, notrace).
'CHR<_2__17'(A < B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A < B], 'CHR<_2'(A < B, C, D, E))
	;
	    true
	).
:- set_flag('CHR<_2__17' / 4, leash, notrace).
A > B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A > B)),
	'CHR>_2'(A > B, D, E, C).



%%% Rules handling for > / 2

'CHR>_2'(A > B, C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHR>_2'(A > B, C, D, E) ?-
	coca(try_rule(E, A > B, anonymous("3"), F > G, replacement, (ground(F), ground(G)), call_kernel(F > G))),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("3"))),
	call_kernel(A > B).
'CHR>_2'(A > B, C, D, E) ?-
	coca(try_rule(E, A > B, anonymous("8"), F > G, replacement, true, (G + slack(H) =:= F, call_kernel(H > 0)))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("8"))),
	B + slack(I) =:= A,
	call_kernel(I > 0).
'CHR>_2'(A > B, C, D, E) :-
	'CHR>_2__18'(A > B, C, D, E).
:- set_flag('CHR>_2' / 4, leash, notrace).
:- current_macro('CHR>_2' / 4, _4366, _4367, _4368) -> true ; define_macro('CHR>_2' / 4, tr_chr / 2, [write]).
'CHR>_2__18'(A, B, C, D) :-
	'CHR>_2__19'(A, B, C, D).
:- set_flag('CHR>_2__18' / 4, leash, notrace).
'CHR>_2__19'(A > B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A > B], 'CHR>_2'(A > B, C, D, E))
	;
	    true
	).
:- set_flag('CHR>_2__19' / 4, leash, notrace).
A =\= B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A =\= B)),
	'CHR=\\=_2'(A =\= B, D, E, C).



%%% Rules handling for =\= / 2

'CHR=\\=_2'(A =\= B, C, D, E) :-
	'CHRnonvar'(C),
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
	'CHR=\\=_2__20'(A =\= B, C, D, E).
:- set_flag('CHR=\\=_2' / 4, leash, notrace).
:- current_macro('CHR=\\=_2' / 4, _5301, _5302, _5303) -> true ; define_macro('CHR=\\=_2' / 4, tr_chr / 2, [write]).
'CHR=\\=_2__20'(A, B, C, D) :-
	'CHR=\\=_2__21'(A, B, C, D).
:- set_flag('CHR=\\=_2__20' / 4, leash, notrace).
'CHR=\\=_2__21'(A =\= B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A =\= B], 'CHR=\\=_2'(A =\= B, C, D, E))
	;
	    true
	).
:- set_flag('CHR=\\=_2__21' / 4, leash, notrace).
A >= B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A >= B)),
	'CHR>=_2'(A >= B, D, E, C).



%%% Rules handling for >= / 2

'CHR>=_2'(A >= B, C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHR>=_2'(A >= B, C, D, E) ?-
	coca(try_rule(E, A >= B, anonymous("1"), F >= G, replacement, (ground(F), ground(G)), call_kernel(F >= G))),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("1"))),
	call_kernel(A >= B).
'CHR>=_2'(A >= B, C, D, E) ?-
	coca(try_rule(E, A >= B, anonymous("6"), F >= G, replacement, true, (G + slack(H) =:= F, call_kernel(H >= 0)))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("6"))),
	B + slack(I) =:= A,
	call_kernel(I >= 0).
'CHR>=_2'(A >= B, C, D, E) :-
	'CHR>=_2__22'(A >= B, C, D, E).
:- set_flag('CHR>=_2' / 4, leash, notrace).
:- current_macro('CHR>=_2' / 4, _6243, _6244, _6245) -> true ; define_macro('CHR>=_2' / 4, tr_chr / 2, [write]).
'CHR>=_2__22'(A, B, C, D) :-
	'CHR>=_2__23'(A, B, C, D).
:- set_flag('CHR>=_2__22' / 4, leash, notrace).
'CHR>=_2__23'(A >= B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A >= B], 'CHR>=_2'(A >= B, C, D, E))
	;
	    true
	).
:- set_flag('CHR>=_2__23' / 4, leash, notrace).
A =< B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A =< B)),
	'CHR=<_2'(A =< B, D, E, C).



%%% Rules handling for =< / 2

'CHR=<_2'(A =< B, C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHR=<_2'(A =< B, C, D, E) ?-
	coca(try_rule(E, A =< B, anonymous("0"), F =< G, replacement, (ground(F), ground(G)), call_kernel(F =< G))),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("0"))),
	call_kernel(A =< B).
'CHR=<_2'(A =< B, C, D, E) ?-
	coca(try_rule(E, A =< B, anonymous("5"), F =< G, replacement, true, (F + slack(H) =:= G, call_kernel(H >= 0)))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("5"))),
	A + slack(I) =:= B,
	call_kernel(I >= 0).
'CHR=<_2'(A =< B, C, D, E) :-
	'CHR=<_2__24'(A =< B, C, D, E).
:- set_flag('CHR=<_2' / 4, leash, notrace).
:- current_macro('CHR=<_2' / 4, _7182, _7183, _7184) -> true ; define_macro('CHR=<_2' / 4, tr_chr / 2, [write]).
'CHR=<_2__24'(A, B, C, D) :-
	'CHR=<_2__25'(A, B, C, D).
:- set_flag('CHR=<_2__24' / 4, leash, notrace).
'CHR=<_2__25'(A =< B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A =< B], 'CHR=<_2'(A =< B, C, D, E))
	;
	    true
	).
:- set_flag('CHR=<_2__25' / 4, leash, notrace).
A =:= B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A =:= B)),
	'CHR=:=_2'(A =:= B, D, E, C).



%%% Rules handling for =:= / 2

'CHR=:=_2'(A =:= B, C, D, E) :-
	'CHRnonvar'(C),
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
	coca(try_rule(E, A =:= B, anonymous("15"), F =:= G, replacement, true, (normalize(F, G, H, I), equals(H, I)))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("15"))),
	normalize(A, B, J, K),
	equals(J, K).
'CHR=:=_2'(A =:= B, C, D, E) :-
	'CHR=:=_2__26'(A =:= B, C, D, E).
:- set_flag('CHR=:=_2' / 4, leash, notrace).
:- current_macro('CHR=:=_2' / 4, _8773, _8774, _8775) -> true ; define_macro('CHR=:=_2' / 4, tr_chr / 2, [write]).
'CHR=:=_2__26'(A, B, C, D) :-
	'CHR=:=_2__27'(A, B, C, D).
:- set_flag('CHR=:=_2__26' / 4, leash, notrace).
'CHR=:=_2__27'(A =:= B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A =:= B], 'CHR=:=_2'(A =:= B, C, D, E))
	;
	    true
	).
:- set_flag('CHR=:=_2__27' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
