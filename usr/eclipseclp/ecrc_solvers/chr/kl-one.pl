
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- op(1200, xfx, isa).
:- op(950, xfx, :).
:- op(940, xfy, or).
:- op(930, xfy, and).
:- op(700, xfx, is).
:- op(690, fy, nota).
:- op(650, fx, some).
:- op(650, fx, every).
:- op(100, fx, feature).
:- op(100, fx, distinct).
:- op(100, fx, at_most_one).
:- op(100, yfx, of).
:- dynamic feature / 1, distinct / 1, isa / 2.
role_assertion((_831, _828) : _825) :- individual(_831), individual(_828), role(_825).
membership_assertion(_858 : _855) :- individual(_858), concept_term(_855).
concept_definition(isa(_882, _879)) :- concept(_882), concept_term(_879).
concept_term(_900) :- concept(_900).
concept_term(or(_921, _918)) :- concept_term(_921), concept_term(_918).
concept_term(and(_945, _942)) :- concept_term(_945), concept_term(_942).
concept_term(nota(_963)) :- concept_term(_963).
concept_term(some(_986) is _983) :- role(_986), concept_term(_983).
concept_term(every(_1012) is _1009) :- role(_1012), concept_term(_1009).
concept_term(at_most_one(_1032)) :- role(_1032).
individual(_1052) :- var(_1052) ; atomic(_1052).
role(_1069) :- atom(_1069).
role(_1090 of _1087) :- role(_1090), role(_1087).
concept(_1116) :- atom(_1116).
isa(female, nota(male)).
isa(woman, and(human, female)).
isa(man, and(human, male)).
isa(parent, and(human, some(child) is human)).
isa(father, and(parent, man)).
isa(mother, and(parent, woman)).
isa(grandfather, and(father, some(child) is parent)).
isa(grandmother, and(mother, some(child) is parent)).
isa(fatherofsons, and(father, every(child) is male)).
feature(age).
isa(person, and(or(man, woman), every(age) is number)).
distinct(number).
feature(partner).
isa(married_person, and(person, every(partner) is married_person)).
distinct(interface).
distinct(configuration).
isa(simple_device, and(device, some(connector) is interface)).
feature(component_1).
feature(component_2).
isa(simple_config, and(configuration, and(some(component_1) is simple_device, some(component_2) is simple_device))).
isa(very_simple_device, and(simple_device, at_most_one(connector))).
feature(price).
feature(voltage).
feature(frequency).
isa(electrical_device, and(very_simple_device, and(some(voltage) is greater(0), some(price) is greater(1)))).
isa(low_cost_device, and(electrical_device, every(price) is smaller(200))).
isa(high_voltage_device, and(electrical_device, every(voltage) is greater(15))).
isa(electrical_config, and(simple_configuration, and(every(component_1) is electrical_device, and(every(component_2) is electrical_device, every(and(voltage of component_1, voltage of component_2)) is greater)))).
isa(bus_device, and(simple_device, and(bus, some(frequency) is greater(0)))).
isa(cpu_device, and(simple_device, and(cpu, some(frequency) is greater(0)))).
isa(bus_config, and(configuration, and(some(main_device) is bus_device, and(every(component) is cpu_device, every(and(frequency of main_device, frequency of sub_device)) is greater)))).
catalog(dev1) :- dev1 : electrical_device, (dev1, 10) : voltage, (dev1, 100) : price.
catalog(dev2) :- dev2 : electrical_device, (dev2, 20) : voltage, (dev2, 1000) : price.
possible_config(_4867) :- catalog(_4876), (_4867, _4876) : component_1, catalog(_4866), (_4867, _4866) : component_2.
feature(functor).
feature(arity).
feature(arg(_4910)).
isa(term, and(top, and(some(arity) is number, and(some(arity) is greater(-1), some(functor) is top)))).



%%% Callables for : / 2

'CHRlabel_with'(A : or(B, C), D, E) ?-
	coca(try_clause(E, A : or(B, C), F : or(G, H), true)),
	coca(clause_fired(E)),
	'CHR='(D, 'clause_:'(A, or(B, C))).



%%% Prolog clauses for : / 2

'clause_:'(A, or(B, C)) :-
	(
	    A : B
	;
	    A : C
	).
:- current_macro('clause_:' / 2, _5525, _5526, _5527) -> true ; define_macro('clause_:' / 2, tr_chr / 2, [write]).
A : B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A : B)),
	'CHR:_2'(A : B, D, E, C).



%%% Rules handling for : / 2

'CHR:_2'(A : B, C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHR:_2'(A : top, B, C, D) ?-
	coca(try_rule(D, A : top, anonymous("2"), E : top, replacement, true, true)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("2"))).
'CHR:_2'(A : nota(top), B, C, D) ?-
	coca(try_rule(D, A : nota(top), anonymous("3"), E : nota(top), replacement, true, fail)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("3"))),
	fail.
'CHR:_2'(A : nota(or(B, C)), D, E, F) ?-
	coca(try_rule(F, A : nota(or(B, C)), anonymous("4"), G : nota(or(H, I)), replacement, true, G : and(nota(H), nota(I)))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("4"))),
	A : and(nota(B), nota(C)).
'CHR:_2'(A : nota(and(B, C)), D, E, F) ?-
	coca(try_rule(F, A : nota(and(B, C)), anonymous("5"), G : nota(and(H, I)), replacement, true, G : or(nota(H), nota(I)))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("5"))),
	A : or(nota(B), nota(C)).
'CHR:_2'(A : nota(nota(B)), C, D, E) ?-
	coca(try_rule(E, A : nota(nota(B)), anonymous("6"), F : nota(nota(G)), replacement, true, F : G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("6"))),
	A : B.
'CHR:_2'(A : (nota(every(B)) is C), D, E, F) ?-
	coca(try_rule(F, A : (nota(every(B)) is C), anonymous("7"), G : (nota(every(H)) is I), replacement, true, G : (some(H) is nota(I)))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("7"))),
	A : (some(B) is nota(C)).
'CHR:_2'(A : (nota(some(B)) is C), D, E, F) ?-
	coca(try_rule(F, A : (nota(some(B)) is C), anonymous("8"), G : (nota(some(H)) is I), replacement, true, G : (every(H) is nota(I)))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("8"))),
	A : (every(B) is nota(C)).
'CHR:_2'(A : and(B, C), D, E, F) ?-
	coca(try_rule(F, A : and(B, C), anonymous("9"), G : and(H, I), replacement, true, (G : H, G : I))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("9"))),
	A : B,
	A : C.
'CHR:_2'(A : (some(B) is C), D, E, F) ?-
	coca(try_rule(F, A : (some(B) is C), anonymous("10"), G : (some(H) is I), replacement, role(H), ((G, J) : H, J : I))),
	no_delayed_goals(role(B)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("10"))),
	(A, K) : B,
	K : C.
'CHR:_2'((A, B) : (C of D), E, F, G) ?-
	coca(try_rule(G, (A, B) : (C of D), anonymous("15"), (H, I) : (J of K), replacement, true, ((H, L) : K, (L, I) : J))),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("15"))),
	(A, M) : D,
	(M, B) : C.
'CHR:_2'(A : nota(at_most_one(B)), C, D, E) ?-
	coca(try_rule(E, A : nota(at_most_one(B)), anonymous("20"), F : nota(at_most_one(G)), replacement, true, ((F, H) : G, (F, I) : G, (H, I) : different))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("20"))),
	(A, J) : B,
	(A, K) : B,
	(J, K) : different.
'CHR:_2'((A, A) : different, B, C, D) ?-
	coca(try_rule(D, (A, A) : different, anonymous("21"), (E, E) : different, replacement, true, fail)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("21"))),
	fail.
'CHR:_2'((A, B) : identical, C, D, E) ?-
	coca(try_rule(E, (A, B) : identical, anonymous("22"), (F, G) : identical, replacement, true, F = G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("22"))),
	A = B.
'CHR:_2'(A : greater(B), C, D, E) ?-
	coca(try_rule(E, A : greater(B), anonymous("23"), F : greater(G), replacement, true, F > G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("23"))),
	A > B.
'CHR:_2'((A, B) : greater, C, D, E) ?-
	coca(try_rule(E, (A, B) : greater, anonymous("24"), (F, G) : greater, replacement, true, F > G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("24"))),
	A > B.
'CHR:_2'(A : smaller(B), C, D, E) ?-
	coca(try_rule(E, A : smaller(B), anonymous("25"), F : smaller(G), replacement, true, F < G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("25"))),
	A < B.
'CHR:_2'((A, B) : smaller, C, D, E) ?-
	coca(try_rule(E, (A, B) : smaller, anonymous("26"), (F, G) : smaller, replacement, true, F < G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("26"))),
	A < B.
'CHR:_2'(A : (some(and(B, C)) is D), E, F, G) ?-
	coca(try_rule(G, A : (some(and(B, C)) is D), anonymous("27"), H : (some(and(I, J)) is K), replacement, true, ((H, L) : I, (H, M) : J, (L, M) : K))),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("27"))),
	(A, N) : B,
	(A, O) : C,
	(N, O) : D.
'CHR:_2'(A : (every(and(B, C)) is D), E, F, G) ?-
	coca(try_rule(G, A : (every(and(B, C)) is D), anonymous("28"), H : (every(and(I, J)) is K), replacement, true, every((H, H), (I, J), K))),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("28"))),
	every((A, A), (B, C), D).
'CHR:_2'(A : B, C, D, E) ?-
	coca(try_rule(E, A : B, anonymous("34"), F : G, replacement, isa(G, H), F : H)),
	no_delayed_goals(isa(B, I)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("34"))),
	A : I.
'CHR:_2'(A : nota(B), C, D, E) ?-
	coca(try_rule(E, A : nota(B), anonymous("35"), F : nota(G), replacement, isa(G, H), F : nota(H))),
	no_delayed_goals(isa(B, I)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("35"))),
	A : nota(I).
'CHR:_2'(A : number, B, C, D) ?-
	coca(try_rule(D, A : number, anonymous("36"), E : number, replacement, nonvar(E), number(E))),
	no_delayed_goals(nonvar(A)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("36"))),
	number(A).
'CHR:_2'(A : nota(B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHR:_2__42'(F, [B, A], [], G),
	coca(try_double(E, A : nota(B), G, A : B, H : nota(I), H : I, replacement, true, fail, anonymous("0"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("0"))),
	fail.
'CHR:_2'(A : B, C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHR:_2__43'(F, [B, A], [], G),
	coca(try_double(E, A : B, G, A : nota(B), H : I, H : nota(I), replacement, true, fail, anonymous("0"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("0"))),
	fail.
'CHR:_2'(A : (every(B) is C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHR:_2__44'(G, [B, A], [H], I),
	coca(try_double(F, A : (every(B) is C), I, A : (every(B) is H), J : (every(K) is L), J : (every(K) is M), replacement, true, (J : and(every(K) is L, M), N : and(L, M)), anonymous("12"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("12"))),
	A : and(every(B) is C, H),
	O : and(C, H).
'CHR:_2'(A : (every(B) is C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHR:_2__45'(G, [B, A], [H], I),
	coca(try_double(F, A : (every(B) is C), I, A : (every(B) is H), J : (every(K) is L), J : (every(K) is M), replacement, true, (J : and(every(K) is M, L), N : and(M, L)), anonymous("12"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("12"))),
	A : and(every(B) is H, C),
	O : and(H, C).
'CHR:_2'(A : B, C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHR:_2__46'(F, [B, A], [], G),
	coca(try_double(E, A : B, G, A : B, H : I, H : I, keep_second, true, true, anonymous("1"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("1"))).
'CHR:_2'(A : B, C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHR:_2__47'(F, [A], [G], H),
	coca(try_double(E, A : B, H, A : G, I : J, I : K, keep_second, (concept(K), concept(J), distinct(K)), K = J, anonymous("13"))),
	no_delayed_goals((concept(G), concept(B), distinct(G))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("13"))),
	G = B.
'CHR:_2'((A, B) : C, D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHR:_2__48'(G, [C, A], [H], I),
	coca(try_double(F, (A, B) : C, I, (A, H) : C, (J, K) : L, (J, M) : L, keep_second, feature(L), M = K, anonymous("14"))),
	no_delayed_goals(feature(C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("14"))),
	H = B.
'CHR:_2'(A : B, C, D, E) :-
	'CHR:_2__41'(A : B, C, D, E).
'CHR:_2__42'(['CHR:_2'(A : B, C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHR:_2__42'([A|B], C, D, E) :-
	'CHR:_2__42'(B, C, D, E).
:- set_flag('CHR:_2__42' / 4, leash, notrace).
'CHR:_2__43'(['CHR:_2'(A : nota(B), C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHR:_2__43'([A|B], C, D, E) :-
	'CHR:_2__43'(B, C, D, E).
:- set_flag('CHR:_2__43' / 4, leash, notrace).
'CHR:_2__44'(['CHR:_2'(A : (every(B) is C), D, E, F)|G], [B, A], [H], I) ?-
	'CHRvar'(D),
	'CHRkill'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHR:_2__44'([A|B], C, D, E) :-
	'CHR:_2__44'(B, C, D, E).
:- set_flag('CHR:_2__44' / 4, leash, notrace).
'CHR:_2__45'(['CHR:_2'(A : (every(B) is C), D, E, F)|G], [B, A], [H], I) ?-
	'CHRvar'(D),
	'CHRkill'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHR:_2__45'([A|B], C, D, E) :-
	'CHR:_2__45'(B, C, D, E).
:- set_flag('CHR:_2__45' / 4, leash, notrace).
'CHR:_2__46'(['CHR:_2'(A : B, C, D, E)|F], [B, A], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHR:_2__46'([A|B], C, D, E) :-
	'CHR:_2__46'(B, C, D, E).
:- set_flag('CHR:_2__46' / 4, leash, notrace).
'CHR:_2__47'(['CHR:_2'(A : B, C, D, E)|F], [A], [G], H) ?-
	'CHRvar'(C),
	'CHR='([B], [G]),
	'CHR='(E, H).
'CHR:_2__47'([A|B], C, D, E) :-
	'CHR:_2__47'(B, C, D, E).
:- set_flag('CHR:_2__47' / 4, leash, notrace).
'CHR:_2__48'(['CHR:_2'((A, B) : C, D, E, F)|G], [C, A], [H], I) ?-
	'CHRvar'(D),
	'CHR='([B], [H]),
	'CHR='(F, I).
'CHR:_2__48'([A|B], C, D, E) :-
	'CHR:_2__48'(B, C, D, E).
:- set_flag('CHR:_2__48' / 4, leash, notrace).
:- set_flag('CHR:_2' / 4, leash, notrace).
:- current_macro('CHR:_2' / 4, _12942, _12943, _12944) -> true ; define_macro('CHR:_2' / 4, tr_chr / 2, [write]).
'CHR:_2__41'((A, B) : arg(C), D, E, F) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('CHR:_2__41', E),
	coca(try_rule(F, (A, B) : arg(C), anonymous("37"), (G, H) : arg(I), augmentation, true, I >= 1)),
	!,
	'CHR:_2__41__50'((A, B) : arg(C), D, E, F),
	coca(fired_rule(anonymous("37"))),
	C >= 1.
'CHR:_2__41'(A, B, C, D) ?-
	'CHR:_2__41__50'(A, B, C, D).
:- set_flag('CHR:_2__41' / 4, leash, notrace).
'CHR:_2__41__50'((A, 0) : arity, B, C, D) ?-
	'CHRvar'(B),
	'CHRcheck_and_mark_applied'('CHR:_2__41__50', C),
	coca(try_rule(D, (A, 0) : arity, anonymous("39"), (E, 0) : arity, augmentation, true, (E, E) : functor)),
	!,
	'CHR:_2__41__50__51'((A, 0) : arity, B, C, D),
	coca(fired_rule(anonymous("39"))),
	(A, A) : functor.
'CHR:_2__41__50'(A, B, C, D) ?-
	'CHR:_2__41__50__51'(A, B, C, D).
:- set_flag('CHR:_2__41__50' / 4, leash, notrace).
'CHR:_2__41__50__51'((A, A) : functor, B, C, D) ?-
	'CHRvar'(B),
	'CHRcheck_and_mark_applied'('CHR:_2__41__50__51', C),
	coca(try_rule(D, (A, A) : functor, anonymous("40"), (E, E) : functor, augmentation, true, (E, 0) : arity)),
	!,
	'CHR:_2__41__50__51__52'((A, A) : functor, B, C, D),
	coca(fired_rule(anonymous("40"))),
	(A, 0) : arity.
'CHR:_2__41__50__51'(A, B, C, D) ?-
	'CHR:_2__41__50__51__52'(A, B, C, D).
:- set_flag('CHR:_2__41__50__51' / 4, leash, notrace).
'CHR:_2__41__50__51__52'(A, B, C, D) :-
	'CHR:_2__49'(A, B, C, D).
:- set_flag('CHR:_2__41__50__51__52' / 4, leash, notrace).
'CHR:_2__49'(A : B, C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHR:_2__49__53'(F, C, A : B, D, E).
'CHR:_2__49'(A : B, C, D, E) :-
	'CHR:_2__49__54'(A : B, C, D, E).
:- set_flag('CHR:_2__49' / 4, leash, notrace).
'CHR:_2__49__53'(['CHR:_2'(A : B, C, D, E)|F], G, A : B, H, I) ?-
	'CHRvar'(C),
	coca(try_double(I, A : B, E, A : B, J : K, J : K, keep_first, true, true, anonymous("1"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("1"))),
	'CHR:_2__49__53'(F, G, A : B, H, I).
'CHR:_2__49__53'([A|B], C, D, E, F) :-
	'CHR:_2__49__53'(B, C, D, E, F).
'CHR:_2__49__53'([], A, B, C, D) :-
	'CHR:_2__49__54'(B, A, C, D).
:- set_flag('CHR:_2__49__53' / 5, leash, notrace).
'CHR:_2__49__54'(A : B, C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHR:_2__49__54__55'(F, C, A : B, D, E).
'CHR:_2__49__54'(A : B, C, D, E) :-
	'CHR:_2__49__54__56'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54' / 4, leash, notrace).
'CHR:_2__49__54__55'(['CHR:_2'(A : B, C, D, E)|F], G, A : H, I, J) ?-
	'CHRvar'(C),
	coca(try_double(J, A : H, E, A : B, K : L, K : M, keep_first, (concept(L), concept(M), distinct(L)), L = M, anonymous("13"))),
	no_delayed_goals((concept(H), concept(B), distinct(H))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("13"))),
	'CHR:_2__49__54__55'(F, G, A : H, I, J),
	H = B.
'CHR:_2__49__54__55'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__55'(B, C, D, E, F).
'CHR:_2__49__54__55'([], A, B, C, D) :-
	'CHR:_2__49__54__56'(B, A, C, D).
:- set_flag('CHR:_2__49__54__55' / 5, leash, notrace).
'CHR:_2__49__54__56'((A, B) : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(C, G),
	'CHR:_2__49__54__56__57'(G, D, (A, B) : C, E, F).
'CHR:_2__49__54__56'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56' / 4, leash, notrace).
'CHR:_2__49__54__56__57'(['CHR:_2'((A, B) : C, D, E, F)|G], H, (A, I) : C, J, K) ?-
	'CHRvar'(D),
	coca(try_double(K, (A, I) : C, F, (A, B) : C, (L, M) : N, (L, O) : N, keep_first, feature(N), M = O, anonymous("14"))),
	no_delayed_goals(feature(C)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("14"))),
	'CHR:_2__49__54__56__57'(G, H, (A, I) : C, J, K),
	I = B.
'CHR:_2__49__54__56__57'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__57'(B, C, D, E, F).
'CHR:_2__49__54__56__57'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__57' / 5, leash, notrace).
'CHR:_2__49__54__56__58'(A : (every(B) is C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR:_2__49__54__56__58__59'(G, D, A : (every(B) is C), E, F).
'CHR:_2__49__54__56__58'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58__60'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56__58' / 4, leash, notrace).
'CHR:_2__49__54__56__58__59'(['CHR:_2'((A, B) : C, D, E, F)|G], H, A : (every(C) is I), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(anonymous("11")), H, D, J, E),
	coca(try_double(K, A : (every(C) is I), F, (A, B) : C, L : (every(M) is N), (L, O) : M, augmentation, true, O : N, anonymous("11"))),
	!,
	coca(fired_rule(anonymous("11"))),
	'CHR:_2__49__54__56__58__59'(G, H, A : (every(C) is I), J, K),
	B : I.
'CHR:_2__49__54__56__58__59'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__58__59'(B, C, D, E, F).
'CHR:_2__49__54__56__58__59'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58__60'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__58__59' / 5, leash, notrace).
'CHR:_2__49__54__56__58__60'((A, B) : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(C, G),
	'CHR:_2__49__54__56__58__60__61'(G, D, (A, B) : C, E, F).
'CHR:_2__49__54__56__58__60'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58__60__62'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56__58__60' / 4, leash, notrace).
'CHR:_2__49__54__56__58__60__61'(['CHR:_2'(A : (every(B) is C), D, E, F)|G], H, (A, I) : B, J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('21'(anonymous("11")), H, D, J, E),
	coca(try_double(K, (A, I) : B, F, A : (every(B) is C), (L, M) : N, L : (every(N) is O), augmentation, true, M : O, anonymous("11"))),
	!,
	coca(fired_rule(anonymous("11"))),
	'CHR:_2__49__54__56__58__60__61'(G, H, (A, I) : B, J, K),
	I : C.
'CHR:_2__49__54__56__58__60__61'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__58__60__61'(B, C, D, E, F).
'CHR:_2__49__54__56__58__60__61'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58__60__62'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__58__60__61' / 5, leash, notrace).
'CHR:_2__49__54__56__58__60__62'(A : (every(B of C) is D), E, F, G) ?-
	'CHRvar'(E),
	!,
	'CHRget_delayed_goals'(C, H),
	'CHR:_2__49__54__56__58__60__62__63'(H, E, A : (every(B of C) is D), F, G).
'CHR:_2__49__54__56__58__60__62'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58__60__62__64'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56__58__60__62' / 4, leash, notrace).
'CHR:_2__49__54__56__58__60__62__63'(['CHR:_2'((A, B) : C, D, E, F)|G], H, A : (every(I of C) is J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(anonymous("16")), H, D, K, E),
	coca(try_double(L, A : (every(I of C) is J), F, (A, B) : C, M : (every(N of O) is P), (M, Q) : O, augmentation, true, Q : (every(N) is P), anonymous("16"))),
	!,
	coca(fired_rule(anonymous("16"))),
	'CHR:_2__49__54__56__58__60__62__63'(G, H, A : (every(I of C) is J), K, L),
	B : (every(I) is J).
'CHR:_2__49__54__56__58__60__62__63'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__58__60__62__63'(B, C, D, E, F).
'CHR:_2__49__54__56__58__60__62__63'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58__60__62__64'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__58__60__62__63' / 5, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64'((A, B) : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(C, G),
	'CHR:_2__49__54__56__58__60__62__64__65'(G, D, (A, B) : C, E, F).
'CHR:_2__49__54__56__58__60__62__64'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58__60__62__64__66'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56__58__60__62__64' / 4, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__65'(['CHR:_2'(A : (every(B of C) is D), E, F, G)|H], I, (A, J) : C, K, L) ?-
	'CHRvar'(E),
	'CHRcheck_and_mark_applied'('21'(anonymous("16")), I, E, K, F),
	coca(try_double(L, (A, J) : C, G, A : (every(B of C) is D), (M, N) : O, M : (every(P of O) is Q), augmentation, true, N : (every(P) is Q), anonymous("16"))),
	!,
	coca(fired_rule(anonymous("16"))),
	'CHR:_2__49__54__56__58__60__62__64__65'(H, I, (A, J) : C, K, L),
	J : (every(B) is D).
'CHR:_2__49__54__56__58__60__62__64__65'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__58__60__62__64__65'(B, C, D, E, F).
'CHR:_2__49__54__56__58__60__62__64__65'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58__60__62__64__66'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__65' / 5, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66'(A : (at_most_one(B) of C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(C, G),
	'CHR:_2__49__54__56__58__60__62__64__66__67'(G, D, A : (at_most_one(B) of C), E, F).
'CHR:_2__49__54__56__58__60__62__64__66'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66' / 4, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__67'(['CHR:_2'((A, B) : C, D, E, F)|G], H, A : (at_most_one(I) of C), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(anonymous("17")), H, D, J, E),
	coca(try_double(K, A : (at_most_one(I) of C), F, (A, B) : C, L : (at_most_one(M) of N), (L, O) : N, augmentation, true, O : at_most_one(M), anonymous("17"))),
	!,
	coca(fired_rule(anonymous("17"))),
	'CHR:_2__49__54__56__58__60__62__64__66__67'(G, H, A : (at_most_one(I) of C), J, K),
	B : at_most_one(I).
'CHR:_2__49__54__56__58__60__62__64__66__67'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__58__60__62__64__66__67'(B, C, D, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__67'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__67' / 5, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68'((A, B) : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(C, G),
	'CHR:_2__49__54__56__58__60__62__64__66__68__69'(G, D, (A, B) : C, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68' / 4, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__69'(['CHR:_2'(A : (at_most_one(B) of C), D, E, F)|G], H, (A, I) : C, J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('21'(anonymous("17")), H, D, J, E),
	coca(try_double(K, (A, I) : C, F, A : (at_most_one(B) of C), (L, M) : N, L : (at_most_one(O) of N), augmentation, true, M : at_most_one(O), anonymous("17"))),
	!,
	coca(fired_rule(anonymous("17"))),
	'CHR:_2__49__54__56__58__60__62__64__66__68__69'(G, H, (A, I) : C, J, K),
	I : at_most_one(B).
'CHR:_2__49__54__56__58__60__62__64__66__68__69'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__69'(B, C, D, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__69'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__69' / 5, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70'(A : at_most_one(B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__71'(F, C, A : at_most_one(B), D, E).
'CHR:_2__49__54__56__58__60__62__64__66__68__70'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70' / 4, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__71'(['CHR:_2'((A, B) : C, D, E, F)|G], H, A : at_most_one(C), I, J) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(anonymous("18")), H, D, I, E),
	coca(try_double(J, A : at_most_one(C), F, (A, B) : C, K : at_most_one(L), (K, M) : L, augmentation, true, at_most_one(K, M, L), anonymous("18"))),
	!,
	coca(fired_rule(anonymous("18"))),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__71'(G, H, A : at_most_one(C), I, J),
	at_most_one(A, B, C).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__71'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__71'(B, C, D, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__71'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__71' / 5, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72'((A, B) : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(C, G),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__73'(G, D, (A, B) : C, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72' / 4, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__73'(['CHR:_2'(A : at_most_one(B), C, D, E)|F], G, (A, H) : B, I, J) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'('21'(anonymous("18")), G, C, I, D),
	coca(try_double(J, (A, H) : B, E, A : at_most_one(B), (K, L) : M, K : at_most_one(M), augmentation, true, at_most_one(K, L, M), anonymous("18"))),
	!,
	coca(fired_rule(anonymous("18"))),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__73'(F, G, (A, H) : B, I, J),
	at_most_one(A, H, B).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__73'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__73'(B, C, D, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__73'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72__73' / 5, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74'((A, B) : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(C, G),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__75'(G, D, (A, B) : C, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74' / 4, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__75'(['CHRevery_3'(every((A, B), (identical, C), D), E, F, G)|H], I, (B, J) : C, K, L) ?-
	'CHRvar'(E),
	'CHRcheck_and_mark_applied'(anonymous("30"), I, E, K, F),
	coca(try_double(L, (B, J) : C, G, every((A, B), (identical, C), D), (M, N) : O, every((P, M), (identical, O), Q), augmentation, true, (P, N) : Q, anonymous("30"))),
	!,
	coca(fired_rule(anonymous("30"))),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__75'(H, I, (B, J) : C, K, L),
	(A, J) : D.
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__75'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__75'(B, C, D, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__75'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__75' / 5, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76'((A, B) : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(C, G),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__77'(G, D, (A, B) : C, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76' / 4, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__77'(['CHRevery_3'(every((A, B), (identical, C of D), E), F, G, H)|I], J, (B, K) : D, L, M) ?-
	'CHRvar'(F),
	'CHRcheck_and_mark_applied'(anonymous("31"), J, F, L, G),
	coca(try_double(M, (B, K) : D, H, every((A, B), (identical, C of D), E), (N, O) : P, every((Q, N), (identical, R of P), S), augmentation, true, every((Q, O), (identical, R), S), anonymous("31"))),
	!,
	coca(fired_rule(anonymous("31"))),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__77'(I, J, (B, K) : D, L, M),
	every((A, K), (identical, C), E).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__77'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__77'(B, C, D, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__77'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__77' / 5, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78'((A, B) : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(C, G),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__79'(G, D, (A, B) : C, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78' / 4, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__79'(['CHRevery_3'(every((A, B), (C, D), E), F, G, H)|I], J, (A, K) : C, L, M) ?-
	'CHRvar'(F),
	'CHRcheck_and_mark_applied'(anonymous("32"), J, F, L, G),
	coca(try_double(M, (A, K) : C, H, every((A, B), (C, D), E), (N, O) : P, every((N, Q), (P, R), S), augmentation, true, every((O, Q), (identical, R), S), anonymous("32"))),
	!,
	coca(fired_rule(anonymous("32"))),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__79'(I, J, (A, K) : C, L, M),
	every((K, B), (identical, D), E).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__79'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__79'(B, C, D, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__79'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__79' / 5, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80'((A, B) : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(C, G),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__81'(G, D, (A, B) : C, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80' / 4, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__81'(['CHRevery_3'(every((A, B), (C of D, E), F), G, H, I)|J], K, (A, L) : D, M, N) ?-
	'CHRvar'(G),
	'CHRcheck_and_mark_applied'(anonymous("33"), K, G, M, H),
	coca(try_double(N, (A, L) : D, I, every((A, B), (C of D, E), F), (O, P) : Q, every((O, R), (S of Q, T), U), augmentation, true, every((P, R), (S, T), U), anonymous("33"))),
	!,
	coca(fired_rule(anonymous("33"))),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__81'(J, K, (A, L) : D, M, N),
	every((L, B), (C, E), F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__81'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__81'(B, C, D, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__81'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__81' / 5, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82'((A, B) : arity, C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__83'(F, C, (A, B) : arity, D, E).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82' / 4, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__83'(['CHR:_2'((A, B) : arg(C), D, E, F)|G], H, (A, I) : arity, J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("38"), H, D, J, E),
	coca(try_double(K, (A, I) : arity, F, (A, B) : arg(C), (L, M) : arity, (L, N) : arg(O), augmentation, true, (M >= O, M >= 1), anonymous("38"))),
	!,
	coca(fired_rule(anonymous("38"))),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__83'(G, H, (A, I) : arity, J, K),
	I >= C,
	I >= 1.
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__83'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__83'(B, C, D, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__83'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__83' / 5, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84'((A, B) : arg(C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84__85'(G, D, (A, B) : arg(C), E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84'(A : B, C, D, E) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84__86'(A : B, C, D, E).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84' / 4, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84__85'(['CHR:_2'((A, B) : arity, C, D, E)|F], G, (A, H) : arg(I), J, K) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(anonymous("38"), G, C, J, D),
	coca(try_double(K, (A, H) : arg(I), E, (A, B) : arity, (L, M) : arg(N), (L, O) : arity, augmentation, true, (O >= N, O >= 1), anonymous("38"))),
	!,
	coca(fired_rule(anonymous("38"))),
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84__85'(F, G, (A, H) : arg(I), J, K),
	B >= I,
	B >= 1.
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84__85'([A|B], C, D, E, F) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84__85'(B, C, D, E, F).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84__85'([], A, B, C, D) :-
	'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84__86'(B, A, C, D).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84__85' / 5, leash, notrace).
'CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84__86'(A : B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A : B], 'CHR:_2'(A : B, C, D, E))
	;
	    true
	).
:- set_flag('CHR:_2__49__54__56__58__60__62__64__66__68__70__72__74__76__78__80__82__84__86' / 4, leash, notrace).
at_most_one(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, at_most_one(A, B, C))),
	'CHRat_most_one_3'(at_most_one(A, B, C), E, F, D).



%%% Rules handling for at_most_one / 3

'CHRat_most_one_3'(at_most_one(A, B, C), D, E, F) :-
	'CHRnonvar'(D),
	!.
'CHRat_most_one_3'(at_most_one(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRat_most_one_3__88'(G, [C, A], [H], I),
	coca(try_double(F, at_most_one(A, B, C), I, at_most_one(A, H, C), at_most_one(J, K, L), at_most_one(J, M, L), keep_second, true, K = M, anonymous("19"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("19"))),
	B = H.
'CHRat_most_one_3'(at_most_one(A, B, C), D, E, F) :-
	'CHRat_most_one_3__87'(at_most_one(A, B, C), D, E, F).
'CHRat_most_one_3__88'(['CHRat_most_one_3'(at_most_one(A, B, C), D, E, F)|G], [C, A], [H], I) ?-
	'CHRvar'(D),
	'CHR='([B], [H]),
	'CHR='(F, I).
'CHRat_most_one_3__88'([A|B], C, D, E) :-
	'CHRat_most_one_3__88'(B, C, D, E).
:- set_flag('CHRat_most_one_3__88' / 4, leash, notrace).
:- set_flag('CHRat_most_one_3' / 4, leash, notrace).
:- current_macro('CHRat_most_one_3' / 4, _25667, _25668, _25669) -> true ; define_macro('CHRat_most_one_3' / 4, tr_chr / 2, [write]).
'CHRat_most_one_3__87'(A, B, C, D) :-
	'CHRat_most_one_3__89'(A, B, C, D).
:- set_flag('CHRat_most_one_3__87' / 4, leash, notrace).
'CHRat_most_one_3__89'(at_most_one(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(C, G),
	'CHRat_most_one_3__89__90'(G, D, at_most_one(A, B, C), E, F).
'CHRat_most_one_3__89'(at_most_one(A, B, C), D, E, F) :-
	'CHRat_most_one_3__89__91'(at_most_one(A, B, C), D, E, F).
:- set_flag('CHRat_most_one_3__89' / 4, leash, notrace).
'CHRat_most_one_3__89__90'(['CHRat_most_one_3'(at_most_one(A, B, C), D, E, F)|G], H, at_most_one(A, I, C), J, K) ?-
	'CHRvar'(D),
	coca(try_double(K, at_most_one(A, I, C), F, at_most_one(A, B, C), at_most_one(L, M, N), at_most_one(L, O, N), keep_first, true, O = M, anonymous("19"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("19"))),
	'CHRat_most_one_3__89__90'(G, H, at_most_one(A, I, C), J, K),
	B = I.
'CHRat_most_one_3__89__90'([A|B], C, D, E, F) :-
	'CHRat_most_one_3__89__90'(B, C, D, E, F).
'CHRat_most_one_3__89__90'([], A, B, C, D) :-
	'CHRat_most_one_3__89__91'(B, A, C, D).
:- set_flag('CHRat_most_one_3__89__90' / 5, leash, notrace).
'CHRat_most_one_3__89__91'(at_most_one(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, at_most_one(A, B, C)], 'CHRat_most_one_3'(at_most_one(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRat_most_one_3__89__91' / 4, leash, notrace).
every(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, every(A, B, C))),
	'CHRevery_3'(every(A, B, C), E, F, D).



%%% Rules handling for every / 3

'CHRevery_3'(every(A, B, C), D, E, F) :-
	'CHRnonvar'(D),
	!.
'CHRevery_3'(every((A, B), (identical, identical), C), D, E, F) ?-
	coca(try_rule(F, every((A, B), (identical, identical), C), anonymous("29"), every((G, H), (identical, identical), I), replacement, true, (G, H) : I)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("29"))),
	(A, B) : C.
'CHRevery_3'(every(A, B, C), D, E, F) :-
	'CHRevery_3__92'(every(A, B, C), D, E, F).
:- set_flag('CHRevery_3' / 4, leash, notrace).
:- current_macro('CHRevery_3' / 4, _27037, _27038, _27039) -> true ; define_macro('CHRevery_3' / 4, tr_chr / 2, [write]).
'CHRevery_3__92'(A, B, C, D) :-
	'CHRevery_3__93'(A, B, C, D).
:- set_flag('CHRevery_3__92' / 4, leash, notrace).
'CHRevery_3__93'(every((A, B), (identical, C), D), E, F, G) ?-
	'CHRvar'(E),
	!,
	'CHRget_delayed_goals'(C, H),
	'CHRevery_3__93__94'(H, E, every((A, B), (identical, C), D), F, G).
'CHRevery_3__93'(every(A, B, C), D, E, F) :-
	'CHRevery_3__93__95'(every(A, B, C), D, E, F).
:- set_flag('CHRevery_3__93' / 4, leash, notrace).
'CHRevery_3__93__94'(['CHR:_2'((A, B) : C, D, E, F)|G], H, every((I, A), (identical, C), J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("30"), H, D, K, E),
	coca(try_double(L, every((I, A), (identical, C), J), F, (A, B) : C, every((M, N), (identical, O), P), (N, Q) : O, augmentation, true, (M, Q) : P, anonymous("30"))),
	!,
	coca(fired_rule(anonymous("30"))),
	'CHRevery_3__93__94'(G, H, every((I, A), (identical, C), J), K, L),
	(I, B) : J.
'CHRevery_3__93__94'([A|B], C, D, E, F) :-
	'CHRevery_3__93__94'(B, C, D, E, F).
'CHRevery_3__93__94'([], A, B, C, D) :-
	'CHRevery_3__93__95'(B, A, C, D).
:- set_flag('CHRevery_3__93__94' / 5, leash, notrace).
'CHRevery_3__93__95'(every((A, B), (identical, C of D), E), F, G, H) ?-
	'CHRvar'(F),
	!,
	'CHRget_delayed_goals'(D, I),
	'CHRevery_3__93__95__96'(I, F, every((A, B), (identical, C of D), E), G, H).
'CHRevery_3__93__95'(every(A, B, C), D, E, F) :-
	'CHRevery_3__93__95__97'(every(A, B, C), D, E, F).
:- set_flag('CHRevery_3__93__95' / 4, leash, notrace).
'CHRevery_3__93__95__96'(['CHR:_2'((A, B) : C, D, E, F)|G], H, every((I, A), (identical, J of C), K), L, M) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("31"), H, D, L, E),
	coca(try_double(M, every((I, A), (identical, J of C), K), F, (A, B) : C, every((N, O), (identical, P of Q), R), (O, S) : Q, augmentation, true, every((N, S), (identical, P), R), anonymous("31"))),
	!,
	coca(fired_rule(anonymous("31"))),
	'CHRevery_3__93__95__96'(G, H, every((I, A), (identical, J of C), K), L, M),
	every((I, B), (identical, J), K).
'CHRevery_3__93__95__96'([A|B], C, D, E, F) :-
	'CHRevery_3__93__95__96'(B, C, D, E, F).
'CHRevery_3__93__95__96'([], A, B, C, D) :-
	'CHRevery_3__93__95__97'(B, A, C, D).
:- set_flag('CHRevery_3__93__95__96' / 5, leash, notrace).
'CHRevery_3__93__95__97'(every((A, B), (C, D), E), F, G, H) ?-
	'CHRvar'(F),
	!,
	'CHRget_delayed_goals'(C, I),
	'CHRevery_3__93__95__97__98'(I, F, every((A, B), (C, D), E), G, H).
'CHRevery_3__93__95__97'(every(A, B, C), D, E, F) :-
	'CHRevery_3__93__95__97__99'(every(A, B, C), D, E, F).
:- set_flag('CHRevery_3__93__95__97' / 4, leash, notrace).
'CHRevery_3__93__95__97__98'(['CHR:_2'((A, B) : C, D, E, F)|G], H, every((A, I), (C, J), K), L, M) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("32"), H, D, L, E),
	coca(try_double(M, every((A, I), (C, J), K), F, (A, B) : C, every((N, O), (P, Q), R), (N, S) : P, augmentation, true, every((S, O), (identical, Q), R), anonymous("32"))),
	!,
	coca(fired_rule(anonymous("32"))),
	'CHRevery_3__93__95__97__98'(G, H, every((A, I), (C, J), K), L, M),
	every((B, I), (identical, J), K).
'CHRevery_3__93__95__97__98'([A|B], C, D, E, F) :-
	'CHRevery_3__93__95__97__98'(B, C, D, E, F).
'CHRevery_3__93__95__97__98'([], A, B, C, D) :-
	'CHRevery_3__93__95__97__99'(B, A, C, D).
:- set_flag('CHRevery_3__93__95__97__98' / 5, leash, notrace).
'CHRevery_3__93__95__97__99'(every((A, B), (C of D, E), F), G, H, I) ?-
	'CHRvar'(G),
	!,
	'CHRget_delayed_goals'(D, J),
	'CHRevery_3__93__95__97__99__100'(J, G, every((A, B), (C of D, E), F), H, I).
'CHRevery_3__93__95__97__99'(every(A, B, C), D, E, F) :-
	'CHRevery_3__93__95__97__99__101'(every(A, B, C), D, E, F).
:- set_flag('CHRevery_3__93__95__97__99' / 4, leash, notrace).
'CHRevery_3__93__95__97__99__100'(['CHR:_2'((A, B) : C, D, E, F)|G], H, every((A, I), (J of C, K), L), M, N) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("33"), H, D, M, E),
	coca(try_double(N, every((A, I), (J of C, K), L), F, (A, B) : C, every((O, P), (Q of R, S), T), (O, U) : R, augmentation, true, every((U, P), (Q, S), T), anonymous("33"))),
	!,
	coca(fired_rule(anonymous("33"))),
	'CHRevery_3__93__95__97__99__100'(G, H, every((A, I), (J of C, K), L), M, N),
	every((B, I), (J, K), L).
'CHRevery_3__93__95__97__99__100'([A|B], C, D, E, F) :-
	'CHRevery_3__93__95__97__99__100'(B, C, D, E, F).
'CHRevery_3__93__95__97__99__100'([], A, B, C, D) :-
	'CHRevery_3__93__95__97__99__101'(B, A, C, D).
:- set_flag('CHRevery_3__93__95__97__99__100' / 5, leash, notrace).
'CHRevery_3__93__95__97__99__101'(every(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, every(A, B, C)], 'CHRevery_3'(every(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRevery_3__93__95__97__99__101' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
