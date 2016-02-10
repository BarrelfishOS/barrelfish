
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
primes1(_762) :- primes1(2, _762).
primes(A) :-
	'CHRgen_num'(B),
	coca(add_one_constraint(B, primes(A))),
	'CHRprimes_1'(primes(A), C, D, B).



%%% Rules handling for primes / 1

'CHRprimes_1'(primes(A), B, C, D) :-
	(
	    'CHRnonvar'(B)
	;
	    'CHRalready_in'('CHRprimes_1'(primes(A), B, C, D)),
	    coca(already_in)
	),
	!.
'CHRprimes_1'(primes(1), A, B, C) ?-
	coca(try_rule(C, primes(1), anonymous("0"), primes(1), replacement, true, true)),
	!,
	'CHRkill'(A),
	coca(fired_rule(anonymous("0"))).
'CHRprimes_1'(primes(A), B, C, D) ?-
	coca(try_rule(D, primes(A), anonymous("1"), primes(E), replacement, E > 1, (F is E - 1, prime(E), primes(F)))),
	no_global_bindings(A > 1, primes(A)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("1"))),
	G is A - 1,
	prime(A),
	primes(G).
'CHRprimes_1'(primes(A), B, C, D) :-
	'CHRprimes_1__14'(primes(A), B, C, D).
:- set_flag('CHRprimes_1' / 4, leash, notrace).
:- current_macro('CHRprimes_1' / 4, _2781, _2782, _2783) -> true ; define_macro('CHRprimes_1' / 4, tr_chr / 2, [write]).
'CHRprimes_1__14'(A, B, C, D) :-
	'CHRprimes_1__15'(A, B, C, D).
:- set_flag('CHRprimes_1__14' / 4, leash, notrace).
'CHRprimes_1__15'(primes(A), B, C, D) :-
	(
	    'CHRvar'(B)
	->
	    'CHRdelay'([B, primes(A)], 'CHRprimes_1'(primes(A), B, C, D))
	;
	    true
	).
:- set_flag('CHRprimes_1__15' / 4, leash, notrace).
prime(A) :-
	'CHRgen_num'(B),
	coca(add_one_constraint(B, prime(A))),
	'CHRprime_1'(prime(A), C, D, B).



%%% Rules handling for prime / 1

'CHRprime_1'(prime(A), B, C, D) :-
	(
	    'CHRnonvar'(B)
	;
	    'CHRalready_in'('CHRprime_1'(prime(A), B, C, D)),
	    coca(already_in)
	),
	!.
'CHRprime_1'(prime(A), B, C, D) ?-
	'CHRget_delayed_goals'(true, E),
	'CHRprime_1__17'(E, [], [F], G),
	coca(try_double(D, prime(A), G, prime(F), prime(H), prime(I), keep_second, mod(H, I, 0), true, anonymous("2"))),
	no_global_bindings(mod(A, F, 0), (prime(A), prime(F))),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("2"))).
'CHRprime_1'(prime(A), B, C, D) :-
	'CHRprime_1__16'(prime(A), B, C, D).
'CHRprime_1__17'(['CHRprime_1'(prime(A), B, C, D)|E], [], [F], G) ?-
	'CHRvar'(B),
	'CHR='([A], [F]),
	'CHR='(D, G).
'CHRprime_1__17'([A|B], C, D, E) :-
	'CHRprime_1__17'(B, C, D, E).
:- set_flag('CHRprime_1__17' / 4, leash, notrace).
:- set_flag('CHRprime_1' / 4, leash, notrace).
:- current_macro('CHRprime_1' / 4, _3817, _3818, _3819) -> true ; define_macro('CHRprime_1' / 4, tr_chr / 2, [write]).
'CHRprime_1__16'(A, B, C, D) :-
	'CHRprime_1__18'(A, B, C, D).
:- set_flag('CHRprime_1__16' / 4, leash, notrace).
'CHRprime_1__18'(prime(A), B, C, D) ?-
	'CHRvar'(B),
	!,
	'CHRget_delayed_goals'(true, E),
	'CHRprime_1__18__19'(E, B, prime(A), C, D).
'CHRprime_1__18'(prime(A), B, C, D) :-
	'CHRprime_1__18__20'(prime(A), B, C, D).
:- set_flag('CHRprime_1__18' / 4, leash, notrace).
'CHRprime_1__18__19'(['CHRprime_1'(prime(A), B, C, D)|E], F, prime(G), H, I) ?-
	'CHRvar'(B),
	coca(try_double(I, prime(G), D, prime(A), prime(J), prime(K), keep_first, mod(K, J, 0), true, anonymous("2"))),
	no_global_bindings(mod(A, G, 0), (prime(G), prime(A))),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("2"))),
	'CHRprime_1__18__19'(E, F, prime(G), H, I).
'CHRprime_1__18__19'([A|B], C, D, E, F) :-
	'CHRprime_1__18__19'(B, C, D, E, F).
'CHRprime_1__18__19'([], A, B, C, D) :-
	'CHRprime_1__18__20'(B, A, C, D).
:- set_flag('CHRprime_1__18__19' / 5, leash, notrace).
'CHRprime_1__18__20'(prime(A), B, C, D) :-
	(
	    'CHRvar'(B)
	->
	    'CHRdelay'([B, prime(A)], 'CHRprime_1'(prime(A), B, C, D))
	;
	    true
	).
:- set_flag('CHRprime_1__18__20' / 4, leash, notrace).
primes1(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, primes1(A, B))),
	'CHRprimes1_2'(primes1(A, B), D, E, C).



%%% Rules handling for primes1 / 2

'CHRprimes1_2'(primes1(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRprimes1_2'(primes1(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRprimes1_2'(primes1(A, B), C, D, E) ?-
	coca(try_rule(E, primes1(A, B), anonymous("3"), primes1(F, G), replacement, F > G, true)),
	no_global_bindings(A > B, primes1(A, B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("3"))).
'CHRprimes1_2'(primes1(A, B), C, D, E) ?-
	coca(try_rule(E, primes1(A, B), anonymous("4"), primes1(F, G), replacement, F =< G, (H is F + 1, prime1(F), primes1(H, G)))),
	no_global_bindings(A =< B, primes1(A, B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("4"))),
	I is A + 1,
	prime1(A),
	primes1(I, B).
'CHRprimes1_2'(primes1(A, B), C, D, E) :-
	'CHRprimes1_2__21'(primes1(A, B), C, D, E).
:- set_flag('CHRprimes1_2' / 4, leash, notrace).
:- current_macro('CHRprimes1_2' / 4, _5344, _5345, _5346) -> true ; define_macro('CHRprimes1_2' / 4, tr_chr / 2, [write]).
'CHRprimes1_2__21'(A, B, C, D) :-
	'CHRprimes1_2__22'(A, B, C, D).
:- set_flag('CHRprimes1_2__21' / 4, leash, notrace).
'CHRprimes1_2__22'(primes1(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, primes1(A, B)], 'CHRprimes1_2'(primes1(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRprimes1_2__22' / 4, leash, notrace).
prime1(A) :-
	'CHRgen_num'(B),
	coca(add_one_constraint(B, prime1(A))),
	'CHRprime1_1'(prime1(A), C, D, B).



%%% Rules handling for prime1 / 1

'CHRprime1_1'(prime1(A), B, C, D) :-
	(
	    'CHRnonvar'(B)
	;
	    'CHRalready_in'('CHRprime1_1'(prime1(A), B, C, D)),
	    coca(already_in)
	),
	!.
'CHRprime1_1'(prime1(A), B, C, D) ?-
	'CHRget_delayed_goals'(true, E),
	'CHRprime1_1__24'(E, [], [F], G),
	coca(try_double(D, prime1(A), G, prime1(F), prime1(H), prime1(I), keep_second, mod(H, I, 0), true, anonymous("5"))),
	no_global_bindings(mod(A, F, 0), (prime1(A), prime1(F))),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("5"))).
'CHRprime1_1'(prime1(A), B, C, D) :-
	'CHRprime1_1__23'(prime1(A), B, C, D).
'CHRprime1_1__24'(['CHRprime1_1'(prime1(A), B, C, D)|E], [], [F], G) ?-
	'CHRvar'(B),
	'CHR='([A], [F]),
	'CHR='(D, G).
'CHRprime1_1__24'([A|B], C, D, E) :-
	'CHRprime1_1__24'(B, C, D, E).
:- set_flag('CHRprime1_1__24' / 4, leash, notrace).
:- set_flag('CHRprime1_1' / 4, leash, notrace).
:- current_macro('CHRprime1_1' / 4, _6384, _6385, _6386) -> true ; define_macro('CHRprime1_1' / 4, tr_chr / 2, [write]).
'CHRprime1_1__23'(A, B, C, D) :-
	'CHRprime1_1__25'(A, B, C, D).
:- set_flag('CHRprime1_1__23' / 4, leash, notrace).
'CHRprime1_1__25'(prime1(A), B, C, D) ?-
	'CHRvar'(B),
	!,
	'CHRget_delayed_goals'(true, E),
	'CHRprime1_1__25__26'(E, B, prime1(A), C, D).
'CHRprime1_1__25'(prime1(A), B, C, D) :-
	'CHRprime1_1__25__27'(prime1(A), B, C, D).
:- set_flag('CHRprime1_1__25' / 4, leash, notrace).
'CHRprime1_1__25__26'(['CHRprime1_1'(prime1(A), B, C, D)|E], F, prime1(G), H, I) ?-
	'CHRvar'(B),
	coca(try_double(I, prime1(G), D, prime1(A), prime1(J), prime1(K), keep_first, mod(K, J, 0), true, anonymous("5"))),
	no_global_bindings(mod(A, G, 0), (prime1(G), prime1(A))),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("5"))),
	'CHRprime1_1__25__26'(E, F, prime1(G), H, I).
'CHRprime1_1__25__26'([A|B], C, D, E, F) :-
	'CHRprime1_1__25__26'(B, C, D, E, F).
'CHRprime1_1__25__26'([], A, B, C, D) :-
	'CHRprime1_1__25__27'(B, A, C, D).
:- set_flag('CHRprime1_1__25__26' / 5, leash, notrace).
'CHRprime1_1__25__27'(prime1(A), B, C, D) :-
	(
	    'CHRvar'(B)
	->
	    'CHRdelay'([B, prime1(A)], 'CHRprime1_1'(prime1(A), B, C, D))
	;
	    true
	).
:- set_flag('CHRprime1_1__25__27' / 4, leash, notrace).
primes(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, primes(A, B))),
	'CHRprimes_2'(primes(A, B), D, E, C).



%%% Rules handling for primes / 2

'CHRprimes_2'(primes(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRprimes_2'(primes(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRprimes_2'(primes(A, B), C, D, E) ?-
	coca(try_rule(E, primes(A, B), anonymous("6"), primes(F, G), replacement, true, (integers(2, F, H), sift(H, G)))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("6"))),
	integers(2, A, I),
	sift(I, B).
'CHRprimes_2'(primes(A, B), C, D, E) :-
	'CHRprimes_2__28'(primes(A, B), C, D, E).
:- set_flag('CHRprimes_2' / 4, leash, notrace).
:- current_macro('CHRprimes_2' / 4, _7738, _7739, _7740) -> true ; define_macro('CHRprimes_2' / 4, tr_chr / 2, [write]).
'CHRprimes_2__28'(A, B, C, D) :-
	'CHRprimes_2__29'(A, B, C, D).
:- set_flag('CHRprimes_2__28' / 4, leash, notrace).
'CHRprimes_2__29'(primes(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, primes(A, B)], 'CHRprimes_2'(primes(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRprimes_2__29' / 4, leash, notrace).
integers(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, integers(A, B, C))),
	'CHRintegers_3'(integers(A, B, C), E, F, D).



%%% Rules handling for integers / 3

'CHRintegers_3'(integers(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHRintegers_3'(integers(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHRintegers_3'(integers(A, B, C), D, E, F) ?-
	coca(try_rule(F, integers(A, B, C), anonymous("7"), integers(G, H, I), replacement, G > H, I = [])),
	no_global_bindings(A > B, integers(A, B, C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("7"))),
	C = [].
'CHRintegers_3'(integers(A, B, C), D, E, F) ?-
	coca(try_rule(F, integers(A, B, C), anonymous("8"), integers(G, H, I), replacement, G =< H, (I = [G|J], K is G + 1, integers(K, H, J)))),
	no_global_bindings(A =< B, integers(A, B, C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("8"))),
	C = [A|L],
	M is A + 1,
	integers(M, B, L).
'CHRintegers_3'(integers(A, B, C), D, E, F) :-
	'CHRintegers_3__30'(integers(A, B, C), D, E, F).
:- set_flag('CHRintegers_3' / 4, leash, notrace).
:- current_macro('CHRintegers_3' / 4, _8717, _8718, _8719) -> true ; define_macro('CHRintegers_3' / 4, tr_chr / 2, [write]).
'CHRintegers_3__30'(A, B, C, D) :-
	'CHRintegers_3__31'(A, B, C, D).
:- set_flag('CHRintegers_3__30' / 4, leash, notrace).
'CHRintegers_3__31'(integers(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, integers(A, B, C)], 'CHRintegers_3'(integers(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRintegers_3__31' / 4, leash, notrace).
sift(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, sift(A, B))),
	'CHRsift_2'(sift(A, B), D, E, C).



%%% Rules handling for sift / 2

'CHRsift_2'(sift(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRsift_2'(sift(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRsift_2'(sift([A|B], C), D, E, F) ?-
	coca(try_rule(F, sift([A|B], C), anonymous("9"), sift([G|H], I), replacement, true, (I = [G|J], filter(H, G, K), sift(K, J)))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("9"))),
	C = [A|L],
	filter(B, A, M),
	sift(M, L).
'CHRsift_2'(sift([], A), B, C, D) ?-
	coca(try_rule(D, sift([], A), anonymous("10"), sift([], E), replacement, true, E = [])),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("10"))),
	A = [].
'CHRsift_2'(sift(A, B), C, D, E) :-
	'CHRsift_2__32'(sift(A, B), C, D, E).
:- set_flag('CHRsift_2' / 4, leash, notrace).
:- current_macro('CHRsift_2' / 4, _9677, _9678, _9679) -> true ; define_macro('CHRsift_2' / 4, tr_chr / 2, [write]).
'CHRsift_2__32'(A, B, C, D) :-
	'CHRsift_2__33'(A, B, C, D).
:- set_flag('CHRsift_2__32' / 4, leash, notrace).
'CHRsift_2__33'(sift(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, sift(A, B)], 'CHRsift_2'(sift(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRsift_2__33' / 4, leash, notrace).
filter(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, filter(A, B, C))),
	'CHRfilter_3'(filter(A, B, C), E, F, D).



%%% Rules handling for filter / 3

'CHRfilter_3'(filter(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHRfilter_3'(filter(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHRfilter_3'(filter([A|B], C, D), E, F, G) ?-
	coca(try_rule(G, filter([A|B], C, D), anonymous("11"), filter([H|I], J, K), replacement, 0 =\= H mod J, (K = [H|L], filter(I, J, L)))),
	no_global_bindings(0 =\= A mod C, filter([A|B], C, D)),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("11"))),
	D = [A|M],
	filter(B, C, M).
'CHRfilter_3'(filter([A|B], C, D), E, F, G) ?-
	coca(try_rule(G, filter([A|B], C, D), anonymous("12"), filter([H|I], J, K), replacement, 0 =:= H mod J, filter(I, J, K))),
	no_global_bindings(0 =:= A mod C, filter([A|B], C, D)),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("12"))),
	filter(B, C, D).
'CHRfilter_3'(filter([], A, B), C, D, E) ?-
	coca(try_rule(E, filter([], A, B), anonymous("13"), filter([], F, G), replacement, true, G = [])),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("13"))),
	B = [].
'CHRfilter_3'(filter(A, B, C), D, E, F) :-
	'CHRfilter_3__34'(filter(A, B, C), D, E, F).
:- set_flag('CHRfilter_3' / 4, leash, notrace).
:- current_macro('CHRfilter_3' / 4, _10806, _10807, _10808) -> true ; define_macro('CHRfilter_3' / 4, tr_chr / 2, [write]).
'CHRfilter_3__34'(A, B, C, D) :-
	'CHRfilter_3__35'(A, B, C, D).
:- set_flag('CHRfilter_3__34' / 4, leash, notrace).
'CHRfilter_3__35'(filter(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, filter(A, B, C)], 'CHRfilter_3'(filter(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRfilter_3__35' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
