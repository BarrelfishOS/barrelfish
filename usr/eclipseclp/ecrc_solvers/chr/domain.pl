
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- setval(domain, term).
:- op(700, xfx, ::).
:- op(600, xfx, ..).
:- op(600, xfx, :).
:- op(700, xfx, lt).
:- op(700, xfx, le).
:- op(700, xfx, gt).
:- op(700, xfx, ge).
:- op(700, xfx, ne).
gt(_579, _583) :- lt(_583, _579).
ge(_596, _600) :- le(_600, _596).
domain(_3360) :- getval(domain, _3360).
makedom([], _3433) :- true.
makedom([_3450|_3451], _3454) :- _3450 :: _3454, makedom(_3451, _3454).
interval(_3833, _3836, [_3833|_3841]) :- _3833 < _3836, !, _3858 is _3833 + 1, interval(_3858, _3836, _3841).
interval(_3988, _3988, [_3988]).
eval_list([], []).
eval_list([_4262|_4263], [_4268|_4269]) :- _4268 is _4262, eval_list(_4263, _4269).
last(_7226, _7229) :- append(_7245, [_7229], _7226), !.
minimum(_8217, _8220, _8223) :- (domain(number) -> _8217 < _8220 ; _8217 @< _8220) -> _8217 = _8223 ; _8220 = _8223.
maximum(_8371, _8374, _8377) :- (domain(number) -> _8371 < _8374 ; _8371 @< _8374) -> _8374 = _8377 ; _8371 = _8377.
element(_11688, _11691, _11694) :- length(_11691, _11706), interval(1, _11706, _11719), gen_pair(_11719, _11691, _11733), _11688 - _11694 :: _11733.
gen_pair([], [], []).
gen_pair([_11880|_11881], [_11886|_11887], [_11880 - _11886|_11895]) :- gen_pair(_11881, _11887, _11895).
intersect(_13758 :: _13762, _13765 :: _13769, _13772 :: _13776) :- findall(_13772, (member(_13758, _13762), member(_13765, _13769)), _13776).
atmost(_15997, _16000, _16003) :- length(_16000, _16015), atmost(_15997, _16000, _16003, _16015).
delete_ground(_16609, _16612, _16615) :- delete(_16609, _16612, _16615), ground(_16609), !.
outof(_17114, []).
outof(_17128, [_17133|_17134]) :- ne(_17128, _17133), outof(_17128, _17134).
circuit1(_17788) :- length(_17788, _17798), _17798 > 1, circuit1(_17798, _17788).
circuit1(2, [2, 1]).
circuit1(_17907, _17910) :- _17907 > 2, interval(1, _17907, _17929), _17937 =.. [f|_17910], domains1(1, _17929, _17910), alldistinct1([], _17910), no_subtours(_17907, 1, _17937, []).
domains1(_18136, _18139, []).
domains1(_18154, _18157, [_18162|_18163]) :- remove(_18154, _18157, _18177), _18162 :: _18177, _18191 is _18154 + 1, domains1(_18191, _18157, _18163).
no_subtours(0, _18336, _18339, _18342) :- !.
no_subtours(_18359, _18362, _18365, _18368) :- outof(_18362, _18368), (var(_18362) -> suspend(no_subtours1(_18359, _18362, _18365, _18368), 3, _18362 -> inst) ; no_subtours1(_18359, _18362, _18365, _18368)).
no_subtours1(_18572, _18575, _18578, _18581) :- _18572 > 0, _18596 is _18572 - 1, arg(_18575, _18578, _18612), no_subtours(_18596, _18612, _18578, [_18575|_18581]).
circuit(_18766) :- length(_18766, _18776), _18776 > 1, circuit(_18776, _18766).
circuit(2, [2, 1]).
circuit(_18885, _18888) :- _18885 > 2, interval(1, _18885, _18907), _18915 =.. [f|_18888], _18924 is _18885 - 1, domains(1, _18907, _18888, _18915, _18924), alldistinct(_18888).
domains(_19116, _19119, [], _19124, _19127).
domains(_19142, _19145, [_19150|_19151], _19154, _19157) :- remove(_19142, _19145, _19173), _19150 :: _19173, _19187 is _19142 + 1, no_subtours1(_19157, _19150, _19154, [_19142]), domains(_19187, _19145, _19151, _19154, _19157).
remove(_19404, _19407, _19410) :- delete(_19404, _19407, _19410) -> true ; _19407 = _19410.
remove_list(_19451, [], _19449) :- !, _19449 = [].
remove_list([], _19518, _19521) :- _19518 = _19521.
remove_list([_19542|_19543], [_19548|_19549], _19552) :- remove(_19542, [_19548|_19549], _19568), remove_list(_19543, _19568, _19552).
remove_lower(_19686, [], _19684) :- !, _19684 = [].
remove_lower(_19751, [_19756|_19757], _19760) :- _19756 @< _19751, !, remove_lower(_19751, _19757, _19760).
remove_lower(_19870, [_19875|_19876], [_19875|_19881]) :- remove_lower(_19870, _19876, _19881).
remove_higher(_19913, [], _19911) :- !, _19911 = [].
remove_higher(_19978, [_19983|_19984], _19987) :- _19983 @> _19978, !, remove_higher(_19978, _19984, _19987).
remove_higher(_20097, [_20102|_20103], [_20102|_20108]) :- remove_higher(_20097, _20103, _20108).



%%% Callables for :: / 2

'CHRlabel_with'(A :: [B|C], D, E) ?-
	coca(try_clause(E, A :: [B|C], F :: [G|H], true)),
	coca(clause_fired(E)),
	'CHR='(D, 'clause_::'(A, [B|C])).
'CHRlabel_with'(A :: B : C, D, E) ?-
	coca(try_clause(E, A :: B : C, F :: G : H, domain(number))),
	no_delayed_goals(domain(number)),
	coca(clause_fired(E)),
	'CHR='(D, 'clause_::'(A, B : C)).
lt(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, lt(A, B))),
	'CHRlt_2'(lt(A, B), D, E, C).



%%% Rules handling for lt / 2

'CHRlt_2'(lt(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRlt_2'(lt(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRlt_2'(lt(A, A), B, C, D) ?-
	coca(try_rule(D, lt(A, A), anonymous("0"), lt(E, E), replacement, true, fail)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("0"))),
	fail.
'CHRlt_2'(lt(A, B), C, D, E) ?-
	coca(try_rule(E, lt(A, B), anonymous("6"), lt(F, G), replacement, (domain(number), ground(F), not number(F)), (H is F, lt(H, G)))),
	no_delayed_goals((domain(number), ground(A), not number(A))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("6"))),
	I is A,
	lt(I, B).
'CHRlt_2'(lt(A, B), C, D, E) ?-
	coca(try_rule(E, lt(A, B), anonymous("7"), lt(F, G), replacement, (domain(number), ground(G), not number(G)), (H is G, lt(F, H)))),
	no_delayed_goals((domain(number), ground(B), not number(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("7"))),
	I is B,
	lt(A, I).
'CHRlt_2'(lt(A, B), C, D, E) ?-
	coca(try_rule(E, lt(A, B), anonymous("12"), lt(F, G), replacement, (ground(F), ground(G)), domain(number) -> F < G ; F @< G)),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("12"))),
	(
	    domain(number)
	->
	    A < B
	;
	    A @< B
	).
'CHRlt_2'(lt(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRlt_2__79'(F, [B, A], [], G),
	coca(try_double(E, lt(A, B), G, lt(B, A), lt(H, I), lt(I, H), replacement, true, fail, anonymous("3"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("3"))),
	fail.
'CHRlt_2'(lt(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRlt_2__80'(F, [B, A], [], G),
	coca(try_double(E, lt(A, B), G, lt(B, A), lt(H, I), lt(I, H), replacement, true, fail, anonymous("3"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("3"))),
	fail.
'CHRlt_2'(lt(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRlt_2__81'(F, [B], [G, H, I], J),
	coca(try_double(E, lt(A, B), J, B :: [I|H], lt(K, L), L :: [M|N], keep_second, ((ground(K), remove_lower(K, [M|N], O), remove(K, O, P)), 'CHRkeep_heads_checking'(L :: [M|N], Q, L :: P, R)), 'CHRhead_not_kept'(R) -> L :: P ; true, anonymous("27"))),
	no_delayed_goals(((ground(A), remove_lower(A, [I|H], S), remove(A, S, T)), 'CHRkeep_heads_checking'(B :: [I|H], G, B :: T, U))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("27"))),
	(
	    'CHRhead_not_kept'(U)
	->
	    B :: T
	;
	    true
	).
'CHRlt_2'(lt(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHRlt_2__82'(F, [A], [G, H, I], J),
	coca(try_double(E, lt(A, B), J, A :: [I|H], lt(K, L), K :: [M|N], keep_second, ((ground(L), remove_higher(L, [M|N], O), remove(L, O, P)), 'CHRkeep_heads_checking'(K :: [M|N], Q, K :: P, R)), 'CHRhead_not_kept'(R) -> K :: P ; true, anonymous("28"))),
	no_delayed_goals(((ground(B), remove_higher(B, [I|H], S), remove(B, S, T)), 'CHRkeep_heads_checking'(A :: [I|H], G, A :: T, U))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("28"))),
	(
	    'CHRhead_not_kept'(U)
	->
	    A :: T
	;
	    true
	).
'CHRlt_2'(lt(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRlt_2__83'(F, [B], [G, H, I], J),
	coca(try_double(E, lt(A, B), J, B :: I : H, lt(K, L), L :: M : N, keep_second, ((ground(K), maximum(M, K, O)), 'CHRkeep_heads_checking'(L :: M : N, P, L :: O : N, Q)), (('CHRhead_not_kept'(Q) -> L :: O : N ; true), ne(L, O)), anonymous("43"))),
	no_delayed_goals(((ground(A), maximum(I, A, R)), 'CHRkeep_heads_checking'(B :: I : H, G, B :: R : H, S))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("43"))),
	(
	    'CHRhead_not_kept'(S)
	->
	    B :: R : H
	;
	    true
	),
	ne(B, R).
'CHRlt_2'(lt(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHRlt_2__84'(F, [A], [G, H, I], J),
	coca(try_double(E, lt(A, B), J, A :: I : H, lt(K, L), K :: M : N, keep_second, ((ground(L), minimum(N, L, O)), 'CHRkeep_heads_checking'(K :: M : N, P, K :: M : O, Q)), (('CHRhead_not_kept'(Q) -> K :: M : O ; true), ne(K, O)), anonymous("44"))),
	no_delayed_goals(((ground(B), minimum(H, B, R)), 'CHRkeep_heads_checking'(A :: I : H, G, A :: I : R, S))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("44"))),
	(
	    'CHRhead_not_kept'(S)
	->
	    A :: I : R
	;
	    true
	),
	ne(A, R).
'CHRlt_2'(lt(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRlt_2__85'(F, [B, A], [G, H], I),
	coca(try_double(E, lt(A, B), I, B - A :: H, lt(J, K), K - J :: L, keep_second, ((M = N - O, findall(M, (member(M, L), N @< O), P)), 'CHRkeep_heads_checking'(K - J :: L, Q, K - J :: P, R)), 'CHRhead_not_kept'(R) -> K - J :: P ; true, anonymous("60"))),
	no_delayed_goals(((S = T - U, findall(S, (member(S, H), T @< U), V)), 'CHRkeep_heads_checking'(B - A :: H, G, B - A :: V, W))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("60"))),
	(
	    'CHRhead_not_kept'(W)
	->
	    B - A :: V
	;
	    true
	).
'CHRlt_2'(lt(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRlt_2__86'(F, [B, A], [G, H], I),
	coca(try_double(E, lt(A, B), I, A - B :: H, lt(J, K), J - K :: L, keep_second, ((M = N - O, findall(M, (member(M, L), O @< N), P)), 'CHRkeep_heads_checking'(J - K :: L, Q, J - K :: P, R)), 'CHRhead_not_kept'(R) -> J - K :: P ; true, anonymous("61"))),
	no_delayed_goals(((S = T - U, findall(S, (member(S, H), U @< T), V)), 'CHRkeep_heads_checking'(A - B :: H, G, A - B :: V, W))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("61"))),
	(
	    'CHRhead_not_kept'(W)
	->
	    A - B :: V
	;
	    true
	).
'CHRlt_2'(lt(A, B), C, D, E) :-
	'CHRlt_2__78'(lt(A, B), C, D, E).
'CHRlt_2__79'(['CHRlt_2'(lt(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRlt_2__79'([A|B], C, D, E) :-
	'CHRlt_2__79'(B, C, D, E).
:- set_flag('CHRlt_2__79' / 4, leash, notrace).
'CHRlt_2__80'(['CHRlt_2'(lt(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRlt_2__80'([A|B], C, D, E) :-
	'CHRlt_2__80'(B, C, D, E).
:- set_flag('CHRlt_2__80' / 4, leash, notrace).
'CHRlt_2__81'(['CHR::_2'(A :: [B|C], D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRlt_2__81'([A|B], C, D, E) :-
	'CHRlt_2__81'(B, C, D, E).
:- set_flag('CHRlt_2__81' / 4, leash, notrace).
'CHRlt_2__82'(['CHR::_2'(A :: [B|C], D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRlt_2__82'([A|B], C, D, E) :-
	'CHRlt_2__82'(B, C, D, E).
:- set_flag('CHRlt_2__82' / 4, leash, notrace).
'CHRlt_2__83'(['CHR::_2'(A :: B : C, D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRlt_2__83'([A|B], C, D, E) :-
	'CHRlt_2__83'(B, C, D, E).
:- set_flag('CHRlt_2__83' / 4, leash, notrace).
'CHRlt_2__84'(['CHR::_2'(A :: B : C, D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRlt_2__84'([A|B], C, D, E) :-
	'CHRlt_2__84'(B, C, D, E).
:- set_flag('CHRlt_2__84' / 4, leash, notrace).
'CHRlt_2__85'(['CHR::_2'(A - B :: C, D, E, F)|G], [A, B], [H, I], J) ?-
	'CHRvar'(D),
	'CHR='([D, C], [H, I]),
	'CHR='(F, J).
'CHRlt_2__85'([A|B], C, D, E) :-
	'CHRlt_2__85'(B, C, D, E).
:- set_flag('CHRlt_2__85' / 4, leash, notrace).
'CHRlt_2__86'(['CHR::_2'(A - B :: C, D, E, F)|G], [B, A], [H, I], J) ?-
	'CHRvar'(D),
	'CHR='([D, C], [H, I]),
	'CHR='(F, J).
'CHRlt_2__86'([A|B], C, D, E) :-
	'CHRlt_2__86'(B, C, D, E).
:- set_flag('CHRlt_2__86' / 4, leash, notrace).
:- set_flag('CHRlt_2' / 4, leash, notrace).
:- current_macro('CHRlt_2' / 4, _26074, _26075, _26076) -> true ; define_macro('CHRlt_2' / 4, tr_chr / 2, [write]).
'CHRlt_2__78'(A, B, C, D) :-
	'CHRlt_2__87'(A, B, C, D).
:- set_flag('CHRlt_2__78' / 4, leash, notrace).
'CHRlt_2__87'(lt(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRlt_2__87__88'(F, C, lt(A, B), D, E).
'CHRlt_2__87'(lt(A, B), C, D, E) :-
	'CHRlt_2__87__89'(lt(A, B), C, D, E).
:- set_flag('CHRlt_2__87' / 4, leash, notrace).
'CHRlt_2__87__88'(['CHR::_2'(A :: [B|C], D, E, F)|G], H, lt(I, A), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("32"), H, D, J, E),
	coca(try_double(K, lt(I, A), F, A :: [B|C], lt(L, M), M :: [N|O], augmentation, var(L), (last([N|O], P), lt(L, P)), anonymous("32"))),
	no_delayed_goals(var(I)),
	!,
	coca(fired_rule(anonymous("32"))),
	'CHRlt_2__87__88'(G, H, lt(I, A), J, K),
	last([B|C], Q),
	lt(I, Q).
'CHRlt_2__87__88'([A|B], C, D, E, F) :-
	'CHRlt_2__87__88'(B, C, D, E, F).
'CHRlt_2__87__88'([], A, B, C, D) :-
	'CHRlt_2__87__89'(B, A, C, D).
:- set_flag('CHRlt_2__87__88' / 5, leash, notrace).
'CHRlt_2__87__89'(lt(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHRlt_2__87__89__90'(F, C, lt(A, B), D, E).
'CHRlt_2__87__89'(lt(A, B), C, D, E) :-
	'CHRlt_2__87__89__91'(lt(A, B), C, D, E).
:- set_flag('CHRlt_2__87__89' / 4, leash, notrace).
'CHRlt_2__87__89__90'(['CHR::_2'(A :: [B|C], D, E, F)|G], H, lt(A, I), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("33"), H, D, J, E),
	coca(try_double(K, lt(A, I), F, A :: [B|C], lt(L, M), L :: [N|O], augmentation, var(M), lt(N, M), anonymous("33"))),
	no_delayed_goals(var(I)),
	!,
	coca(fired_rule(anonymous("33"))),
	'CHRlt_2__87__89__90'(G, H, lt(A, I), J, K),
	lt(B, I).
'CHRlt_2__87__89__90'([A|B], C, D, E, F) :-
	'CHRlt_2__87__89__90'(B, C, D, E, F).
'CHRlt_2__87__89__90'([], A, B, C, D) :-
	'CHRlt_2__87__89__91'(B, A, C, D).
:- set_flag('CHRlt_2__87__89__90' / 5, leash, notrace).
'CHRlt_2__87__89__91'(lt(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRlt_2__87__89__91__92'(F, C, lt(A, B), D, E).
'CHRlt_2__87__89__91'(lt(A, B), C, D, E) :-
	'CHRlt_2__87__89__91__93'(lt(A, B), C, D, E).
:- set_flag('CHRlt_2__87__89__91' / 4, leash, notrace).
'CHRlt_2__87__89__91__92'(['CHR::_2'(A :: B : C, D, E, F)|G], H, lt(I, A), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("47"), H, D, J, E),
	coca(try_double(K, lt(I, A), F, A :: B : C, lt(L, M), M :: N : O, augmentation, var(L), lt(L, O), anonymous("47"))),
	no_delayed_goals(var(I)),
	!,
	coca(fired_rule(anonymous("47"))),
	'CHRlt_2__87__89__91__92'(G, H, lt(I, A), J, K),
	lt(I, C).
'CHRlt_2__87__89__91__92'([A|B], C, D, E, F) :-
	'CHRlt_2__87__89__91__92'(B, C, D, E, F).
'CHRlt_2__87__89__91__92'([], A, B, C, D) :-
	'CHRlt_2__87__89__91__93'(B, A, C, D).
:- set_flag('CHRlt_2__87__89__91__92' / 5, leash, notrace).
'CHRlt_2__87__89__91__93'(lt(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHRlt_2__87__89__91__93__94'(F, C, lt(A, B), D, E).
'CHRlt_2__87__89__91__93'(lt(A, B), C, D, E) :-
	'CHRlt_2__87__89__91__93__95'(lt(A, B), C, D, E).
:- set_flag('CHRlt_2__87__89__91__93' / 4, leash, notrace).
'CHRlt_2__87__89__91__93__94'(['CHR::_2'(A :: B : C, D, E, F)|G], H, lt(A, I), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("48"), H, D, J, E),
	coca(try_double(K, lt(A, I), F, A :: B : C, lt(L, M), L :: N : O, augmentation, var(M), lt(N, M), anonymous("48"))),
	no_delayed_goals(var(I)),
	!,
	coca(fired_rule(anonymous("48"))),
	'CHRlt_2__87__89__91__93__94'(G, H, lt(A, I), J, K),
	lt(B, I).
'CHRlt_2__87__89__91__93__94'([A|B], C, D, E, F) :-
	'CHRlt_2__87__89__91__93__94'(B, C, D, E, F).
'CHRlt_2__87__89__91__93__94'([], A, B, C, D) :-
	'CHRlt_2__87__89__91__93__95'(B, A, C, D).
:- set_flag('CHRlt_2__87__89__91__93__94' / 5, leash, notrace).
'CHRlt_2__87__89__91__93__95'(lt(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, lt(A, B)], 'CHRlt_2'(lt(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRlt_2__87__89__91__93__95' / 4, leash, notrace).
le(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, le(A, B))),
	'CHRle_2'(le(A, B), D, E, C).



%%% Rules handling for le / 2

'CHRle_2'(le(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRle_2'(le(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRle_2'(le(A, A), B, C, D) ?-
	coca(try_rule(D, le(A, A), anonymous("1"), le(E, E), replacement, true, true)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("1"))).
'CHRle_2'(le(A, B), C, D, E) ?-
	coca(try_rule(E, le(A, B), anonymous("8"), le(F, G), replacement, (domain(number), ground(F), not number(F)), (H is F, le(H, G)))),
	no_delayed_goals((domain(number), ground(A), not number(A))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("8"))),
	I is A,
	le(I, B).
'CHRle_2'(le(A, B), C, D, E) ?-
	coca(try_rule(E, le(A, B), anonymous("9"), le(F, G), replacement, (domain(number), ground(G), not number(G)), (H is G, le(F, H)))),
	no_delayed_goals((domain(number), ground(B), not number(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("9"))),
	I is B,
	le(A, I).
'CHRle_2'(le(A, B), C, D, E) ?-
	coca(try_rule(E, le(A, B), anonymous("13"), le(F, G), replacement, (ground(F), ground(G)), domain(number) -> F =< G ; F @=< G)),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("13"))),
	(
	    domain(number)
	->
	    A =< B
	;
	    A @=< B
	).
'CHRle_2'(le(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRle_2__97'(F, [B, A], [], G),
	coca(try_double(E, le(A, B), G, le(B, A), le(H, I), le(I, H), replacement, true, H = I, anonymous("4"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("4"))),
	A = B.
'CHRle_2'(le(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRle_2__98'(F, [B, A], [], G),
	coca(try_double(E, le(A, B), G, le(B, A), le(H, I), le(I, H), replacement, true, I = H, anonymous("4"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("4"))),
	B = A.
'CHRle_2'(le(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRle_2__99'(F, [B], [G, H, I], J),
	coca(try_double(E, le(A, B), J, B :: [I|H], le(K, L), L :: [M|N], keep_second, ((ground(K), remove_lower(K, [M|N], O)), 'CHRkeep_heads_checking'(L :: [M|N], P, L :: O, Q)), 'CHRhead_not_kept'(Q) -> L :: O ; true, anonymous("25"))),
	no_delayed_goals(((ground(A), remove_lower(A, [I|H], R)), 'CHRkeep_heads_checking'(B :: [I|H], G, B :: R, S))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("25"))),
	(
	    'CHRhead_not_kept'(S)
	->
	    B :: R
	;
	    true
	).
'CHRle_2'(le(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHRle_2__100'(F, [A], [G, H, I], J),
	coca(try_double(E, le(A, B), J, A :: [I|H], le(K, L), K :: [M|N], keep_second, ((ground(L), remove_higher(L, [M|N], O)), 'CHRkeep_heads_checking'(K :: [M|N], P, K :: O, Q)), 'CHRhead_not_kept'(Q) -> K :: O ; true, anonymous("26"))),
	no_delayed_goals(((ground(B), remove_higher(B, [I|H], R)), 'CHRkeep_heads_checking'(A :: [I|H], G, A :: R, S))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("26"))),
	(
	    'CHRhead_not_kept'(S)
	->
	    A :: R
	;
	    true
	).
'CHRle_2'(le(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRle_2__101'(F, [B], [G, H, I], J),
	coca(try_double(E, le(A, B), J, B :: I : H, le(K, L), L :: M : N, keep_second, ((ground(K), maximum(M, K, O)), 'CHRkeep_heads_checking'(L :: M : N, P, L :: O : N, Q)), 'CHRhead_not_kept'(Q) -> L :: O : N ; true, anonymous("41"))),
	no_delayed_goals(((ground(A), maximum(I, A, R)), 'CHRkeep_heads_checking'(B :: I : H, G, B :: R : H, S))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("41"))),
	(
	    'CHRhead_not_kept'(S)
	->
	    B :: R : H
	;
	    true
	).
'CHRle_2'(le(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHRle_2__102'(F, [A], [G, H, I], J),
	coca(try_double(E, le(A, B), J, A :: I : H, le(K, L), K :: M : N, keep_second, ((ground(L), minimum(N, L, O)), 'CHRkeep_heads_checking'(K :: M : N, P, K :: M : O, Q)), 'CHRhead_not_kept'(Q) -> K :: M : O ; true, anonymous("42"))),
	no_delayed_goals(((ground(B), minimum(H, B, R)), 'CHRkeep_heads_checking'(A :: I : H, G, A :: I : R, S))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("42"))),
	(
	    'CHRhead_not_kept'(S)
	->
	    A :: I : R
	;
	    true
	).
'CHRle_2'(le(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRle_2__103'(F, [B, A], [G, H], I),
	coca(try_double(E, le(A, B), I, B - A :: H, le(J, K), K - J :: L, keep_second, ((M = N - O, findall(M, (member(M, L), N @=< O), P)), 'CHRkeep_heads_checking'(K - J :: L, Q, K - J :: P, R)), 'CHRhead_not_kept'(R) -> K - J :: P ; true, anonymous("62"))),
	no_delayed_goals(((S = T - U, findall(S, (member(S, H), T @=< U), V)), 'CHRkeep_heads_checking'(B - A :: H, G, B - A :: V, W))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("62"))),
	(
	    'CHRhead_not_kept'(W)
	->
	    B - A :: V
	;
	    true
	).
'CHRle_2'(le(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRle_2__104'(F, [B, A], [G, H], I),
	coca(try_double(E, le(A, B), I, A - B :: H, le(J, K), J - K :: L, keep_second, ((M = N - O, findall(M, (member(M, L), O @=< N), P)), 'CHRkeep_heads_checking'(J - K :: L, Q, J - K :: P, R)), 'CHRhead_not_kept'(R) -> J - K :: P ; true, anonymous("63"))),
	no_delayed_goals(((S = T - U, findall(S, (member(S, H), U @=< T), V)), 'CHRkeep_heads_checking'(A - B :: H, G, A - B :: V, W))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("63"))),
	(
	    'CHRhead_not_kept'(W)
	->
	    A - B :: V
	;
	    true
	).
'CHRle_2'(le(A, B), C, D, E) :-
	'CHRle_2__96'(le(A, B), C, D, E).
'CHRle_2__97'(['CHRle_2'(le(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRle_2__97'([A|B], C, D, E) :-
	'CHRle_2__97'(B, C, D, E).
:- set_flag('CHRle_2__97' / 4, leash, notrace).
'CHRle_2__98'(['CHRle_2'(le(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHRkill'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRle_2__98'([A|B], C, D, E) :-
	'CHRle_2__98'(B, C, D, E).
:- set_flag('CHRle_2__98' / 4, leash, notrace).
'CHRle_2__99'(['CHR::_2'(A :: [B|C], D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRle_2__99'([A|B], C, D, E) :-
	'CHRle_2__99'(B, C, D, E).
:- set_flag('CHRle_2__99' / 4, leash, notrace).
'CHRle_2__100'(['CHR::_2'(A :: [B|C], D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRle_2__100'([A|B], C, D, E) :-
	'CHRle_2__100'(B, C, D, E).
:- set_flag('CHRle_2__100' / 4, leash, notrace).
'CHRle_2__101'(['CHR::_2'(A :: B : C, D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRle_2__101'([A|B], C, D, E) :-
	'CHRle_2__101'(B, C, D, E).
:- set_flag('CHRle_2__101' / 4, leash, notrace).
'CHRle_2__102'(['CHR::_2'(A :: B : C, D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRle_2__102'([A|B], C, D, E) :-
	'CHRle_2__102'(B, C, D, E).
:- set_flag('CHRle_2__102' / 4, leash, notrace).
'CHRle_2__103'(['CHR::_2'(A - B :: C, D, E, F)|G], [A, B], [H, I], J) ?-
	'CHRvar'(D),
	'CHR='([D, C], [H, I]),
	'CHR='(F, J).
'CHRle_2__103'([A|B], C, D, E) :-
	'CHRle_2__103'(B, C, D, E).
:- set_flag('CHRle_2__103' / 4, leash, notrace).
'CHRle_2__104'(['CHR::_2'(A - B :: C, D, E, F)|G], [B, A], [H, I], J) ?-
	'CHRvar'(D),
	'CHR='([D, C], [H, I]),
	'CHR='(F, J).
'CHRle_2__104'([A|B], C, D, E) :-
	'CHRle_2__104'(B, C, D, E).
:- set_flag('CHRle_2__104' / 4, leash, notrace).
:- set_flag('CHRle_2' / 4, leash, notrace).
:- current_macro('CHRle_2' / 4, _32744, _32745, _32746) -> true ; define_macro('CHRle_2' / 4, tr_chr / 2, [write]).
'CHRle_2__96'(A, B, C, D) :-
	'CHRle_2__105'(A, B, C, D).
:- set_flag('CHRle_2__96' / 4, leash, notrace).
'CHRle_2__105'(le(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRle_2__105__106'(F, C, le(A, B), D, E).
'CHRle_2__105'(le(A, B), C, D, E) :-
	'CHRle_2__105__107'(le(A, B), C, D, E).
:- set_flag('CHRle_2__105' / 4, leash, notrace).
'CHRle_2__105__106'(['CHR::_2'(A :: [B|C], D, E, F)|G], H, le(I, A), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("30"), H, D, J, E),
	coca(try_double(K, le(I, A), F, A :: [B|C], le(L, M), M :: [N|O], augmentation, var(L), (last([N|O], P), le(L, P)), anonymous("30"))),
	no_delayed_goals(var(I)),
	!,
	coca(fired_rule(anonymous("30"))),
	'CHRle_2__105__106'(G, H, le(I, A), J, K),
	last([B|C], Q),
	le(I, Q).
'CHRle_2__105__106'([A|B], C, D, E, F) :-
	'CHRle_2__105__106'(B, C, D, E, F).
'CHRle_2__105__106'([], A, B, C, D) :-
	'CHRle_2__105__107'(B, A, C, D).
:- set_flag('CHRle_2__105__106' / 5, leash, notrace).
'CHRle_2__105__107'(le(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHRle_2__105__107__108'(F, C, le(A, B), D, E).
'CHRle_2__105__107'(le(A, B), C, D, E) :-
	'CHRle_2__105__107__109'(le(A, B), C, D, E).
:- set_flag('CHRle_2__105__107' / 4, leash, notrace).
'CHRle_2__105__107__108'(['CHR::_2'(A :: [B|C], D, E, F)|G], H, le(A, I), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("31"), H, D, J, E),
	coca(try_double(K, le(A, I), F, A :: [B|C], le(L, M), L :: [N|O], augmentation, var(M), le(N, M), anonymous("31"))),
	no_delayed_goals(var(I)),
	!,
	coca(fired_rule(anonymous("31"))),
	'CHRle_2__105__107__108'(G, H, le(A, I), J, K),
	le(B, I).
'CHRle_2__105__107__108'([A|B], C, D, E, F) :-
	'CHRle_2__105__107__108'(B, C, D, E, F).
'CHRle_2__105__107__108'([], A, B, C, D) :-
	'CHRle_2__105__107__109'(B, A, C, D).
:- set_flag('CHRle_2__105__107__108' / 5, leash, notrace).
'CHRle_2__105__107__109'(le(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRle_2__105__107__109__110'(F, C, le(A, B), D, E).
'CHRle_2__105__107__109'(le(A, B), C, D, E) :-
	'CHRle_2__105__107__109__111'(le(A, B), C, D, E).
:- set_flag('CHRle_2__105__107__109' / 4, leash, notrace).
'CHRle_2__105__107__109__110'(['CHR::_2'(A :: B : C, D, E, F)|G], H, le(I, A), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("45"), H, D, J, E),
	coca(try_double(K, le(I, A), F, A :: B : C, le(L, M), M :: N : O, augmentation, var(L), le(L, O), anonymous("45"))),
	no_delayed_goals(var(I)),
	!,
	coca(fired_rule(anonymous("45"))),
	'CHRle_2__105__107__109__110'(G, H, le(I, A), J, K),
	le(I, C).
'CHRle_2__105__107__109__110'([A|B], C, D, E, F) :-
	'CHRle_2__105__107__109__110'(B, C, D, E, F).
'CHRle_2__105__107__109__110'([], A, B, C, D) :-
	'CHRle_2__105__107__109__111'(B, A, C, D).
:- set_flag('CHRle_2__105__107__109__110' / 5, leash, notrace).
'CHRle_2__105__107__109__111'(le(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(A, F),
	'CHRle_2__105__107__109__111__112'(F, C, le(A, B), D, E).
'CHRle_2__105__107__109__111'(le(A, B), C, D, E) :-
	'CHRle_2__105__107__109__111__113'(le(A, B), C, D, E).
:- set_flag('CHRle_2__105__107__109__111' / 4, leash, notrace).
'CHRle_2__105__107__109__111__112'(['CHR::_2'(A :: B : C, D, E, F)|G], H, le(A, I), J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("46"), H, D, J, E),
	coca(try_double(K, le(A, I), F, A :: B : C, le(L, M), L :: N : O, augmentation, var(M), le(N, M), anonymous("46"))),
	no_delayed_goals(var(I)),
	!,
	coca(fired_rule(anonymous("46"))),
	'CHRle_2__105__107__109__111__112'(G, H, le(A, I), J, K),
	le(B, I).
'CHRle_2__105__107__109__111__112'([A|B], C, D, E, F) :-
	'CHRle_2__105__107__109__111__112'(B, C, D, E, F).
'CHRle_2__105__107__109__111__112'([], A, B, C, D) :-
	'CHRle_2__105__107__109__111__113'(B, A, C, D).
:- set_flag('CHRle_2__105__107__109__111__112' / 5, leash, notrace).
'CHRle_2__105__107__109__111__113'(le(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, le(A, B)], 'CHRle_2'(le(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRle_2__105__107__109__111__113' / 4, leash, notrace).
ne(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, ne(A, B))),
	'CHRne_2'(ne(A, B), D, E, C).



%%% Rules handling for ne / 2

'CHRne_2'(ne(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRne_2'(ne(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRne_2'(ne(A, A), B, C, D) ?-
	coca(try_rule(D, ne(A, A), anonymous("2"), ne(E, E), replacement, true, fail)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("2"))),
	fail.
'CHRne_2'(ne(A, B), C, D, E) ?-
	coca(try_rule(E, ne(A, B), anonymous("10"), ne(F, G), replacement, (domain(number), ground(F), not number(F)), (H is F, ne(H, G)))),
	no_delayed_goals((domain(number), ground(A), not number(A))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("10"))),
	I is A,
	ne(I, B).
'CHRne_2'(ne(A, B), C, D, E) ?-
	coca(try_rule(E, ne(A, B), anonymous("11"), ne(F, G), replacement, (domain(number), ground(G), not number(G)), (H is G, ne(F, H)))),
	no_delayed_goals((domain(number), ground(B), not number(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("11"))),
	I is B,
	ne(A, I).
'CHRne_2'(ne(A, B), C, D, E) ?-
	coca(try_rule(E, ne(A, B), anonymous("14"), ne(F, G), replacement, (ground(F), ground(G)), domain(number) -> F =\= G ; F \== G)),
	no_delayed_goals((ground(A), ground(B))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("14"))),
	(
	    domain(number)
	->
	    A =\= B
	;
	    A \== B
	).
'CHRne_2'(ne(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRne_2__115'(F, [B, A], [], G),
	coca(try_double(E, ne(A, B), G, ne(B, A), ne(H, I), ne(I, H), keep_second, true, true, anonymous("5"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("5"))).
'CHRne_2'(ne(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHRne_2__116'(F, [A], [G, H, I], J),
	coca(try_double(E, ne(A, B), J, A :: [I|H], ne(K, L), K :: [M|N], keep_second, ((ground(L), remove(L, [M|N], O)), 'CHRkeep_heads_checking'(K :: [M|N], P, K :: O, Q)), 'CHRhead_not_kept'(Q) -> K :: O ; true, anonymous("23"))),
	no_delayed_goals(((ground(B), remove(B, [I|H], R)), 'CHRkeep_heads_checking'(A :: [I|H], G, A :: R, S))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("23"))),
	(
	    'CHRhead_not_kept'(S)
	->
	    A :: R
	;
	    true
	).
'CHRne_2'(ne(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRne_2__117'(F, [B], [G, H, I], J),
	coca(try_double(E, ne(A, B), J, B :: [I|H], ne(K, L), L :: [M|N], keep_second, ((ground(K), remove(K, [M|N], O)), 'CHRkeep_heads_checking'(L :: [M|N], P, L :: O, Q)), 'CHRhead_not_kept'(Q) -> L :: O ; true, anonymous("24"))),
	no_delayed_goals(((ground(A), remove(A, [I|H], R)), 'CHRkeep_heads_checking'(B :: [I|H], G, B :: R, S))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("24"))),
	(
	    'CHRhead_not_kept'(S)
	->
	    B :: R
	;
	    true
	).
'CHRne_2'(ne(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(A, F),
	'CHRne_2__118'(F, [A], [G, H], I),
	coca(try_double(E, ne(A, B), I, A :: H : G, ne(J, K), J :: L : M, keep_second, (ground(K), (domain(number) -> (K < L ; K > M) ; K @< L ; K @> M)), true, anonymous("39"))),
	no_delayed_goals((ground(B), (domain(number) -> (B < H ; B > G) ; B @< H ; B @> G))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("39"))).
'CHRne_2'(ne(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRne_2__119'(F, [B], [G, H], I),
	coca(try_double(E, ne(A, B), I, B :: H : G, ne(J, K), K :: L : M, keep_second, (ground(J), (domain(number) -> (J < L ; J > M) ; J @< L ; J @> M)), true, anonymous("40"))),
	no_delayed_goals((ground(A), (domain(number) -> (A < H ; A > G) ; A @< H ; A @> G))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("40"))).
'CHRne_2'(ne(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRne_2__120'(F, [B, A], [G, H], I),
	coca(try_double(E, ne(A, B), I, B - A :: H, ne(J, K), K - J :: L, keep_second, ((M = N - O, findall(M, (member(M, L), N \== O), P)), 'CHRkeep_heads_checking'(K - J :: L, Q, K - J :: P, R)), 'CHRhead_not_kept'(R) -> K - J :: P ; true, anonymous("64"))),
	no_delayed_goals(((S = T - U, findall(S, (member(S, H), T \== U), V)), 'CHRkeep_heads_checking'(B - A :: H, G, B - A :: V, W))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("64"))),
	(
	    'CHRhead_not_kept'(W)
	->
	    B - A :: V
	;
	    true
	).
'CHRne_2'(ne(A, B), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRne_2__121'(F, [B, A], [G, H], I),
	coca(try_double(E, ne(A, B), I, A - B :: H, ne(J, K), J - K :: L, keep_second, ((M = N - O, findall(M, (member(M, L), O \== N), P)), 'CHRkeep_heads_checking'(J - K :: L, Q, J - K :: P, R)), 'CHRhead_not_kept'(R) -> J - K :: P ; true, anonymous("65"))),
	no_delayed_goals(((S = T - U, findall(S, (member(S, H), U \== T), V)), 'CHRkeep_heads_checking'(A - B :: H, G, A - B :: V, W))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("65"))),
	(
	    'CHRhead_not_kept'(W)
	->
	    A - B :: V
	;
	    true
	).
'CHRne_2'(ne(A, B), C, D, E) :-
	'CHRne_2__114'(ne(A, B), C, D, E).
'CHRne_2__115'(['CHRne_2'(ne(A, B), C, D, E)|F], [A, B], [], G) ?-
	'CHRvar'(C),
	'CHR='([], []),
	'CHR='(E, G).
'CHRne_2__115'([A|B], C, D, E) :-
	'CHRne_2__115'(B, C, D, E).
:- set_flag('CHRne_2__115' / 4, leash, notrace).
'CHRne_2__116'(['CHR::_2'(A :: [B|C], D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRne_2__116'([A|B], C, D, E) :-
	'CHRne_2__116'(B, C, D, E).
:- set_flag('CHRne_2__116' / 4, leash, notrace).
'CHRne_2__117'(['CHR::_2'(A :: [B|C], D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRne_2__117'([A|B], C, D, E) :-
	'CHRne_2__117'(B, C, D, E).
:- set_flag('CHRne_2__117' / 4, leash, notrace).
'CHRne_2__118'(['CHR::_2'(A :: B : C, D, E, F)|G], [A], [H, I], J) ?-
	'CHRvar'(D),
	'CHR='([C, B], [H, I]),
	'CHR='(F, J).
'CHRne_2__118'([A|B], C, D, E) :-
	'CHRne_2__118'(B, C, D, E).
:- set_flag('CHRne_2__118' / 4, leash, notrace).
'CHRne_2__119'(['CHR::_2'(A :: B : C, D, E, F)|G], [A], [H, I], J) ?-
	'CHRvar'(D),
	'CHR='([C, B], [H, I]),
	'CHR='(F, J).
'CHRne_2__119'([A|B], C, D, E) :-
	'CHRne_2__119'(B, C, D, E).
:- set_flag('CHRne_2__119' / 4, leash, notrace).
'CHRne_2__120'(['CHR::_2'(A - B :: C, D, E, F)|G], [A, B], [H, I], J) ?-
	'CHRvar'(D),
	'CHR='([D, C], [H, I]),
	'CHR='(F, J).
'CHRne_2__120'([A|B], C, D, E) :-
	'CHRne_2__120'(B, C, D, E).
:- set_flag('CHRne_2__120' / 4, leash, notrace).
'CHRne_2__121'(['CHR::_2'(A - B :: C, D, E, F)|G], [B, A], [H, I], J) ?-
	'CHRvar'(D),
	'CHR='([D, C], [H, I]),
	'CHR='(F, J).
'CHRne_2__121'([A|B], C, D, E) :-
	'CHRne_2__121'(B, C, D, E).
:- set_flag('CHRne_2__121' / 4, leash, notrace).
:- set_flag('CHRne_2' / 4, leash, notrace).
:- current_macro('CHRne_2' / 4, _38917, _38918, _38919) -> true ; define_macro('CHRne_2' / 4, tr_chr / 2, [write]).
'CHRne_2__114'(A, B, C, D) :-
	'CHRne_2__122'(A, B, C, D).
:- set_flag('CHRne_2__114' / 4, leash, notrace).
'CHRne_2__122'(ne(A, B), C, D, E) ?-
	'CHRvar'(C),
	!,
	'CHRget_delayed_goals'(B, F),
	'CHRne_2__122__123'(F, C, ne(A, B), D, E).
'CHRne_2__122'(ne(A, B), C, D, E) :-
	'CHRne_2__122__124'(ne(A, B), C, D, E).
:- set_flag('CHRne_2__122' / 4, leash, notrace).
'CHRne_2__122__123'(['CHRne_2'(ne(A, B), C, D, E)|F], G, ne(B, A), H, I) ?-
	'CHRvar'(C),
	coca(try_double(I, ne(B, A), E, ne(A, B), ne(J, K), ne(K, J), keep_first, true, true, anonymous("5"))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("5"))),
	'CHRne_2__122__123'(F, G, ne(B, A), H, I).
'CHRne_2__122__123'([A|B], C, D, E, F) :-
	'CHRne_2__122__123'(B, C, D, E, F).
'CHRne_2__122__123'([], A, B, C, D) :-
	'CHRne_2__122__124'(B, A, C, D).
:- set_flag('CHRne_2__122__123' / 5, leash, notrace).
'CHRne_2__122__124'(ne(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, ne(A, B)], 'CHRne_2'(ne(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRne_2__122__124' / 4, leash, notrace).



%%% Prolog clauses for :: / 2

'clause_::'(A, [B|C]) :-
	member(A, [B|C]).
'clause_::'(A, B : C) :-
	D is (B + C) / 2,
	(
	    A :: B : D
	;
	    ne(A, D),
	    A :: D : C
	).
:- current_macro('clause_::' / 2, _39665, _39666, _39667) -> true ; define_macro('clause_::' / 2, tr_chr / 2, [write]).
A :: B :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, A :: B)),
	'CHR::_2'(A :: B, D, E, C).



%%% Rules handling for :: / 2

'CHR::_2'(A :: B, C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHR::_2'(A :: B, C, D, E)),
	    coca(already_in)
	),
	!.
'CHR::_2'(A :: B, C, D, E) ?-
	coca(try_rule(E, A :: B, anonymous("15"), F :: G, replacement, nonground(G), (write('ERROR: Nonground Domain in '), writeln(F :: G), abort))),
	no_delayed_goals(nonground(B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("15"))),
	write('ERROR: Nonground Domain in '),
	writeln(A :: B),
	abort.
'CHR::_2'([A|B] :: C, D, E, F) ?-
	coca(try_rule(F, [A|B] :: C, anonymous("16"), [G|H] :: I, replacement, true, makedom([G|H], I))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("16"))),
	makedom([A|B], C).
'CHR::_2'(A :: B .. C, D, E, F) ?-
	coca(try_rule(F, A :: B .. C, anonymous("17"), G :: H .. I, replacement, true, (J is H, (J =:= round(J) -> K is fix(J) ; K is fix(J + 1)), L is fix(I), interval(K, L, M), G :: M))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("17"))),
	N is B,
	(
	    N =:= round(N)
	->
	    O is fix(N)
	;
	    O is fix(N + 1)
	),
	P is fix(C),
	interval(O, P, Q),
	A :: Q.
'CHR::_2'(A :: [B|C], D, E, F) ?-
	coca(try_rule(F, A :: [B|C], anonymous("18"), G :: [H|I], replacement, (domain(number), member(G, [H|I]), not number(G)), (eval_list([H|I], J), sort(J, K), G :: K))),
	no_delayed_goals((domain(number), member(A, [B|C]), not number(A))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("18"))),
	eval_list([B|C], L),
	sort(L, M),
	A :: M.
'CHR::_2'(A :: [], B, C, D) ?-
	coca(try_rule(D, A :: [], anonymous("19"), E :: [], replacement, true, fail)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("19"))),
	fail.
'CHR::_2'(A :: [B], C, D, E) ?-
	coca(try_rule(E, A :: [B], anonymous("20"), F :: [G], replacement, true, F = G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("20"))),
	A = B.
'CHR::_2'(A :: [B|C], D, E, F) ?-
	coca(try_rule(F, A :: [B|C], anonymous("21"), G :: [H|I], replacement, ground(G), member(G, [H|I]) -> true)),
	no_delayed_goals(ground(A)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("21"))),
	(
	    member(A, [B|C])
	->
	    true
	).
'CHR::_2'(A :: B : C, D, E, F) ?-
	coca(try_rule(F, A :: B : C, anonymous("34"), G :: H : I, replacement, (domain(number), not (number(H), number(I))), (J is H, K is I, G :: J : K))),
	no_delayed_goals((domain(number), not (number(B), number(C)))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("34"))),
	L is B,
	M is C,
	A :: L : M.
'CHR::_2'(A :: B : B, C, D, E) ?-
	coca(try_rule(E, A :: B : B, anonymous("35"), F :: G : G, replacement, true, F = G)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("35"))),
	A = B.
'CHR::_2'(A :: B : C, D, E, F) ?-
	coca(try_rule(F, A :: B : C, anonymous("36"), G :: H : I, replacement, domain(number) -> H > I ; H @> I, fail)),
	no_delayed_goals(domain(number) -> B > C ; B @> C),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("36"))),
	fail.
'CHR::_2'(A :: B : C, D, E, F) ?-
	coca(try_rule(F, A :: B : C, anonymous("37"), G :: H : I, replacement, ground(G), domain(number) -> (H =< G, G =< I) ; (H @=< G, G @=< I))),
	no_delayed_goals(ground(A)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("37"))),
	(
	    domain(number)
	->
	    B =< A,
	    A =< C
	;
	    B @=< A,
	    A @=< C
	).
'CHR::_2'(A - A :: B, C, D, E) ?-
	coca(try_rule(E, A - A :: B, anonymous("53"), F - F :: G, replacement, true, (setof(H, member(H - H, G), I), F :: I))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("53"))),
	setof(J, member(J - J, B), K),
	A :: K.
'CHR::_2'(A - B :: C, D, E, F) ?-
	coca(try_rule(F, A - B :: C, anonymous("54"), G - H :: I, replacement, ground(G), (setof(J, member(G - J, I), K), H :: K))),
	no_delayed_goals(ground(A)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("54"))),
	setof(L, member(A - L, C), M),
	B :: M.
'CHR::_2'(A - B :: C, D, E, F) ?-
	coca(try_rule(F, A - B :: C, anonymous("55"), G - H :: I, replacement, ground(H), (setof(J, member(J - H, I), K), G :: K))),
	no_delayed_goals(ground(B)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("55"))),
	setof(L, member(L - B, C), M),
	A :: M.
'CHR::_2'(A :: B, C, D, E) :-
	'CHR::_2__125'(A :: B, C, D, E).
:- set_flag('CHR::_2' / 4, leash, notrace).
:- current_macro('CHR::_2' / 4, _42135, _42136, _42137) -> true ; define_macro('CHR::_2' / 4, tr_chr / 2, [write]).
'CHR::_2__125'(A - B :: C, D, E, F) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('CHR::_2__125', E),
	coca(try_rule(F, A - B :: C, anonymous("66"), G - H :: I, augmentation, true, (J = K - L, setof(K, J ^ member(J, I), M), G :: M, setof(L, J ^ member(J, I), N), H :: N))),
	!,
	'CHR::_2__125__127'(A - B :: C, D, E, F),
	coca(fired_rule(anonymous("66"))),
	O = P - Q,
	setof(P, O ^ member(O, C), R),
	A :: R,
	setof(Q, O ^ member(O, C), S),
	B :: S.
'CHR::_2__125'(A, B, C, D) ?-
	'CHR::_2__125__127'(A, B, C, D).
:- set_flag('CHR::_2__125' / 4, leash, notrace).
'CHR::_2__125__127'(A, B, C, D) :-
	'CHR::_2__126'(A, B, C, D).
:- set_flag('CHR::_2__125__127' / 4, leash, notrace).
'CHR::_2__126'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__128'(G, D, A :: [B|C], E, F).
'CHR::_2__126'(A :: B, C, D, E) :-
	'CHR::_2__126__129'(A :: B, C, D, E).
:- set_flag('CHR::_2__126' / 4, leash, notrace).
'CHR::_2__126__128'(['CHRne_2'(ne(A, B), C, D, E)|F], G, A :: [H|I], J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, A :: [H|I], E, ne(A, B), L :: [M|N], ne(L, O), keep_first, ((ground(O), remove(O, [M|N], P)), 'CHRkeep_heads_checking'(L :: [M|N], Q, L :: P, R)), 'CHRhead_not_kept'(R) -> L :: P ; true, anonymous("23"))),
	no_delayed_goals(((ground(B), remove(B, [H|I], S)), 'CHRkeep_heads_checking'(A :: [H|I], G, A :: S, T))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("23"))),
	'CHR::_2__126__128'(F, G, A :: [H|I], J, K),
	(
	    'CHRhead_not_kept'(T)
	->
	    A :: S
	;
	    true
	).
'CHR::_2__126__128'([A|B], C, D, E, F) :-
	'CHR::_2__126__128'(B, C, D, E, F).
'CHR::_2__126__128'([], A, B, C, D) :-
	'CHR::_2__126__129'(B, A, C, D).
:- set_flag('CHR::_2__126__128' / 5, leash, notrace).
'CHR::_2__126__129'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__130'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129' / 4, leash, notrace).
'CHR::_2__126__129__130'(['CHRne_2'(ne(A, B), C, D, E)|F], G, B :: [H|I], J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, B :: [H|I], E, ne(A, B), L :: [M|N], ne(O, L), keep_first, ((ground(O), remove(O, [M|N], P)), 'CHRkeep_heads_checking'(L :: [M|N], Q, L :: P, R)), 'CHRhead_not_kept'(R) -> L :: P ; true, anonymous("24"))),
	no_delayed_goals(((ground(A), remove(A, [H|I], S)), 'CHRkeep_heads_checking'(B :: [H|I], G, B :: S, T))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("24"))),
	'CHR::_2__126__129__130'(F, G, B :: [H|I], J, K),
	(
	    'CHRhead_not_kept'(T)
	->
	    B :: S
	;
	    true
	).
'CHR::_2__126__129__130'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__130'(B, C, D, E, F).
'CHR::_2__126__129__130'([], A, B, C, D) :-
	'CHR::_2__126__129__131'(B, A, C, D).
:- set_flag('CHR::_2__126__129__130' / 5, leash, notrace).
'CHR::_2__126__129__131'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__132'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129__131'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131' / 4, leash, notrace).
'CHR::_2__126__129__131__132'(['CHRle_2'(le(A, B), C, D, E)|F], G, B :: [H|I], J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, B :: [H|I], E, le(A, B), L :: [M|N], le(O, L), keep_first, ((ground(O), remove_lower(O, [M|N], P)), 'CHRkeep_heads_checking'(L :: [M|N], Q, L :: P, R)), 'CHRhead_not_kept'(R) -> L :: P ; true, anonymous("25"))),
	no_delayed_goals(((ground(A), remove_lower(A, [H|I], S)), 'CHRkeep_heads_checking'(B :: [H|I], G, B :: S, T))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("25"))),
	'CHR::_2__126__129__131__132'(F, G, B :: [H|I], J, K),
	(
	    'CHRhead_not_kept'(T)
	->
	    B :: S
	;
	    true
	).
'CHR::_2__126__129__131__132'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__132'(B, C, D, E, F).
'CHR::_2__126__129__131__132'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__132' / 5, leash, notrace).
'CHR::_2__126__129__131__133'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__134'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129__131__133'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133' / 4, leash, notrace).
'CHR::_2__126__129__131__133__134'(['CHRle_2'(le(A, B), C, D, E)|F], G, A :: [H|I], J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, A :: [H|I], E, le(A, B), L :: [M|N], le(L, O), keep_first, ((ground(O), remove_higher(O, [M|N], P)), 'CHRkeep_heads_checking'(L :: [M|N], Q, L :: P, R)), 'CHRhead_not_kept'(R) -> L :: P ; true, anonymous("26"))),
	no_delayed_goals(((ground(B), remove_higher(B, [H|I], S)), 'CHRkeep_heads_checking'(A :: [H|I], G, A :: S, T))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("26"))),
	'CHR::_2__126__129__131__133__134'(F, G, A :: [H|I], J, K),
	(
	    'CHRhead_not_kept'(T)
	->
	    A :: S
	;
	    true
	).
'CHR::_2__126__129__131__133__134'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__134'(B, C, D, E, F).
'CHR::_2__126__129__131__133__134'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__134' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__136'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129__131__133__135'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__136'(['CHRlt_2'(lt(A, B), C, D, E)|F], G, B :: [H|I], J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, B :: [H|I], E, lt(A, B), L :: [M|N], lt(O, L), keep_first, ((ground(O), remove_lower(O, [M|N], P), remove(O, P, Q)), 'CHRkeep_heads_checking'(L :: [M|N], R, L :: Q, S)), 'CHRhead_not_kept'(S) -> L :: Q ; true, anonymous("27"))),
	no_delayed_goals(((ground(A), remove_lower(A, [H|I], T), remove(A, T, U)), 'CHRkeep_heads_checking'(B :: [H|I], G, B :: U, V))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("27"))),
	'CHR::_2__126__129__131__133__135__136'(F, G, B :: [H|I], J, K),
	(
	    'CHRhead_not_kept'(V)
	->
	    B :: U
	;
	    true
	).
'CHR::_2__126__129__131__133__135__136'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__136'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__136'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__136' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__138'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129__131__133__135__137'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__138'(['CHRlt_2'(lt(A, B), C, D, E)|F], G, A :: [H|I], J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, A :: [H|I], E, lt(A, B), L :: [M|N], lt(L, O), keep_first, ((ground(O), remove_higher(O, [M|N], P), remove(O, P, Q)), 'CHRkeep_heads_checking'(L :: [M|N], R, L :: Q, S)), 'CHRhead_not_kept'(S) -> L :: Q ; true, anonymous("28"))),
	no_delayed_goals(((ground(B), remove_higher(B, [H|I], T), remove(B, T, U)), 'CHRkeep_heads_checking'(A :: [H|I], G, A :: U, V))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("28"))),
	'CHR::_2__126__129__131__133__135__137__138'(F, G, A :: [H|I], J, K),
	(
	    'CHRhead_not_kept'(V)
	->
	    A :: U
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__138'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__138'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__138'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__138' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__140'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__140'(['CHRne_2'(ne(A, B), C, D, E)|F], G, A :: H : I, J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, A :: H : I, E, ne(A, B), L :: M : N, ne(L, O), keep_first, (ground(O), (domain(number) -> (O < M ; O > N) ; O @< M ; O @> N)), true, anonymous("39"))),
	no_delayed_goals((ground(B), (domain(number) -> (B < H ; B > I) ; B @< H ; B @> I))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("39"))),
	'CHR::_2__126__129__131__133__135__137__139__140'(F, G, A :: H : I, J, K).
'CHR::_2__126__129__131__133__135__137__139__140'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__140'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__140'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__140' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__142'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__142'(['CHRne_2'(ne(A, B), C, D, E)|F], G, B :: H : I, J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, B :: H : I, E, ne(A, B), L :: M : N, ne(O, L), keep_first, (ground(O), (domain(number) -> (O < M ; O > N) ; O @< M ; O @> N)), true, anonymous("40"))),
	no_delayed_goals((ground(A), (domain(number) -> (A < H ; A > I) ; A @< H ; A @> I))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("40"))),
	'CHR::_2__126__129__131__133__135__137__139__141__142'(F, G, B :: H : I, J, K).
'CHR::_2__126__129__131__133__135__137__139__141__142'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__142'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__142'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__142' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__144'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__144'(['CHRle_2'(le(A, B), C, D, E)|F], G, B :: H : I, J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, B :: H : I, E, le(A, B), L :: M : N, le(O, L), keep_first, ((ground(O), maximum(M, O, P)), 'CHRkeep_heads_checking'(L :: M : N, Q, L :: P : N, R)), 'CHRhead_not_kept'(R) -> L :: P : N ; true, anonymous("41"))),
	no_delayed_goals(((ground(A), maximum(H, A, S)), 'CHRkeep_heads_checking'(B :: H : I, G, B :: S : I, T))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("41"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__144'(F, G, B :: H : I, J, K),
	(
	    'CHRhead_not_kept'(T)
	->
	    B :: S : I
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__144'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__144'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__144'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__144' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__146'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__146'(['CHRle_2'(le(A, B), C, D, E)|F], G, A :: H : I, J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, A :: H : I, E, le(A, B), L :: M : N, le(L, O), keep_first, ((ground(O), minimum(N, O, P)), 'CHRkeep_heads_checking'(L :: M : N, Q, L :: M : P, R)), 'CHRhead_not_kept'(R) -> L :: M : P ; true, anonymous("42"))),
	no_delayed_goals(((ground(B), minimum(I, B, S)), 'CHRkeep_heads_checking'(A :: H : I, G, A :: H : S, T))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("42"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__146'(F, G, A :: H : I, J, K),
	(
	    'CHRhead_not_kept'(T)
	->
	    A :: H : S
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__146'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__146'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__146'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__146' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__148'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__148'(['CHRlt_2'(lt(A, B), C, D, E)|F], G, B :: H : I, J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, B :: H : I, E, lt(A, B), L :: M : N, lt(O, L), keep_first, ((ground(O), maximum(M, O, P)), 'CHRkeep_heads_checking'(L :: M : N, Q, L :: P : N, R)), (('CHRhead_not_kept'(R) -> L :: P : N ; true), ne(L, P)), anonymous("43"))),
	no_delayed_goals(((ground(A), maximum(H, A, S)), 'CHRkeep_heads_checking'(B :: H : I, G, B :: S : I, T))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("43"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__148'(F, G, B :: H : I, J, K),
	(
	    'CHRhead_not_kept'(T)
	->
	    B :: S : I
	;
	    true
	),
	ne(B, S).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__148'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__148'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__148'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__148' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__150'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__150'(['CHRlt_2'(lt(A, B), C, D, E)|F], G, A :: H : I, J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, A :: H : I, E, lt(A, B), L :: M : N, lt(L, O), keep_first, ((ground(O), minimum(N, O, P)), 'CHRkeep_heads_checking'(L :: M : N, Q, L :: M : P, R)), (('CHRhead_not_kept'(R) -> L :: M : P ; true), ne(L, P)), anonymous("44"))),
	no_delayed_goals(((ground(B), minimum(I, B, S)), 'CHRkeep_heads_checking'(A :: H : I, G, A :: H : S, T))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("44"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__150'(F, G, A :: H : I, J, K),
	(
	    'CHRhead_not_kept'(T)
	->
	    A :: H : S
	;
	    true
	),
	ne(A, S).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__150'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__150'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__150'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__150' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151'(A - B :: C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__152'(G, D, A - B :: C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__152'(['CHRlt_2'(lt(A, B), C, D, E)|F], G, B - A :: H, I, J) ?-
	'CHRvar'(C),
	coca(try_double(J, B - A :: H, E, lt(A, B), K - L :: M, lt(L, K), keep_first, ((N = O - P, findall(N, (member(N, M), O @< P), Q)), 'CHRkeep_heads_checking'(K - L :: M, R, K - L :: Q, S)), 'CHRhead_not_kept'(S) -> K - L :: Q ; true, anonymous("60"))),
	no_delayed_goals(((T = U - V, findall(T, (member(T, H), U @< V), W)), 'CHRkeep_heads_checking'(B - A :: H, G, B - A :: W, X))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("60"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__152'(F, G, B - A :: H, I, J),
	(
	    'CHRhead_not_kept'(X)
	->
	    B - A :: W
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__152'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__152'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__152'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__152' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153'(A - B :: C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__154'(G, D, A - B :: C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__154'(['CHRlt_2'(lt(A, B), C, D, E)|F], G, A - B :: H, I, J) ?-
	'CHRvar'(C),
	coca(try_double(J, A - B :: H, E, lt(A, B), K - L :: M, lt(K, L), keep_first, ((N = O - P, findall(N, (member(N, M), P @< O), Q)), 'CHRkeep_heads_checking'(K - L :: M, R, K - L :: Q, S)), 'CHRhead_not_kept'(S) -> K - L :: Q ; true, anonymous("61"))),
	no_delayed_goals(((T = U - V, findall(T, (member(T, H), V @< U), W)), 'CHRkeep_heads_checking'(A - B :: H, G, A - B :: W, X))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("61"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__154'(F, G, A - B :: H, I, J),
	(
	    'CHRhead_not_kept'(X)
	->
	    A - B :: W
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__154'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__154'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__154'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__154' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155'(A - B :: C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__156'(G, D, A - B :: C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__156'(['CHRle_2'(le(A, B), C, D, E)|F], G, B - A :: H, I, J) ?-
	'CHRvar'(C),
	coca(try_double(J, B - A :: H, E, le(A, B), K - L :: M, le(L, K), keep_first, ((N = O - P, findall(N, (member(N, M), O @=< P), Q)), 'CHRkeep_heads_checking'(K - L :: M, R, K - L :: Q, S)), 'CHRhead_not_kept'(S) -> K - L :: Q ; true, anonymous("62"))),
	no_delayed_goals(((T = U - V, findall(T, (member(T, H), U @=< V), W)), 'CHRkeep_heads_checking'(B - A :: H, G, B - A :: W, X))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("62"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__156'(F, G, B - A :: H, I, J),
	(
	    'CHRhead_not_kept'(X)
	->
	    B - A :: W
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__156'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__156'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__156'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__156' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157'(A - B :: C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__158'(G, D, A - B :: C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__158'(['CHRle_2'(le(A, B), C, D, E)|F], G, A - B :: H, I, J) ?-
	'CHRvar'(C),
	coca(try_double(J, A - B :: H, E, le(A, B), K - L :: M, le(K, L), keep_first, ((N = O - P, findall(N, (member(N, M), P @=< O), Q)), 'CHRkeep_heads_checking'(K - L :: M, R, K - L :: Q, S)), 'CHRhead_not_kept'(S) -> K - L :: Q ; true, anonymous("63"))),
	no_delayed_goals(((T = U - V, findall(T, (member(T, H), V @=< U), W)), 'CHRkeep_heads_checking'(A - B :: H, G, A - B :: W, X))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("63"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__158'(F, G, A - B :: H, I, J),
	(
	    'CHRhead_not_kept'(X)
	->
	    A - B :: W
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__158'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__158'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__158'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__158' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159'(A - B :: C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__160'(G, D, A - B :: C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__160'(['CHRne_2'(ne(A, B), C, D, E)|F], G, B - A :: H, I, J) ?-
	'CHRvar'(C),
	coca(try_double(J, B - A :: H, E, ne(A, B), K - L :: M, ne(L, K), keep_first, ((N = O - P, findall(N, (member(N, M), O \== P), Q)), 'CHRkeep_heads_checking'(K - L :: M, R, K - L :: Q, S)), 'CHRhead_not_kept'(S) -> K - L :: Q ; true, anonymous("64"))),
	no_delayed_goals(((T = U - V, findall(T, (member(T, H), U \== V), W)), 'CHRkeep_heads_checking'(B - A :: H, G, B - A :: W, X))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("64"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__160'(F, G, B - A :: H, I, J),
	(
	    'CHRhead_not_kept'(X)
	->
	    B - A :: W
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__160'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__160'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__160'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__160' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161'(A - B :: C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__162'(G, D, A - B :: C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__162'(['CHRne_2'(ne(A, B), C, D, E)|F], G, A - B :: H, I, J) ?-
	'CHRvar'(C),
	coca(try_double(J, A - B :: H, E, ne(A, B), K - L :: M, ne(K, L), keep_first, ((N = O - P, findall(N, (member(N, M), P \== O), Q)), 'CHRkeep_heads_checking'(K - L :: M, R, K - L :: Q, S)), 'CHRhead_not_kept'(S) -> K - L :: Q ; true, anonymous("65"))),
	no_delayed_goals(((T = U - V, findall(T, (member(T, H), V \== U), W)), 'CHRkeep_heads_checking'(A - B :: H, G, A - B :: W, X))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("65"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__162'(F, G, A - B :: H, I, J),
	(
	    'CHRhead_not_kept'(X)
	->
	    A - B :: W
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__162'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__162'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__162'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__162' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__164'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__164'(['CHRalldistinct1_2'(alldistinct1(A, [B]), C, D, E)|F], G, B :: [H|I], J, K) ?-
	'CHRvar'(C),
	coca(try_double(K, B :: [H|I], E, alldistinct1(A, [B]), L :: [M|N], alldistinct1(O, [L]), keep_first, (ground(O), 'CHRkeep_heads_checking'(L :: [M|N], P, L :: Q, R)), (remove_list(O, [M|N], Q), ('CHRhead_not_kept'(R) -> L :: Q ; true)), anonymous("75"))),
	no_delayed_goals((ground(A), 'CHRkeep_heads_checking'(B :: [H|I], G, B :: S, T))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("75"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__164'(F, G, B :: [H|I], J, K),
	remove_list(A, [H|I], S),
	(
	    'CHRhead_not_kept'(T)
	->
	    B :: S
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__164'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__164'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__164'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__164' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__166'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__166'(['CHR::_2'(A :: [B|C], D, E, F)|G], H, A :: [I|J], K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(anonymous("22")), H, D, K, E),
	coca(try_double(L, A :: [I|J], F, A :: [B|C], M :: [N|O], M :: [P|Q], augmentation, (intersection([N|O], [P|Q], R), 'CHRkeep_heads_checking'(M :: [N|O], S, M :: [P|Q], T, M :: R, U)), 'CHRhead_not_kept'(U) -> M :: R ; true, anonymous("22"))),
	no_delayed_goals((intersection([I|J], [B|C], V), 'CHRkeep_heads_checking'(A :: [I|J], H, A :: [B|C], D, A :: V, W))),
	!,
	coca(fired_rule(anonymous("22"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__166'(G, H, A :: [I|J], K, L),
	(
	    'CHRhead_not_kept'(W)
	->
	    A :: V
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__166'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__166'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__166'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__166' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__168'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__168'(['CHR::_2'(A :: [B|C], D, E, F)|G], H, A :: [I|J], K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('21'(anonymous("22")), H, D, K, E),
	coca(try_double(L, A :: [I|J], F, A :: [B|C], M :: [N|O], M :: [P|Q], augmentation, (intersection([P|Q], [N|O], R), 'CHRkeep_heads_checking'(M :: [P|Q], S, M :: [N|O], T, M :: R, U)), 'CHRhead_not_kept'(U) -> M :: R ; true, anonymous("22"))),
	no_delayed_goals((intersection([B|C], [I|J], V), 'CHRkeep_heads_checking'(A :: [B|C], D, A :: [I|J], H, A :: V, W))),
	!,
	coca(fired_rule(anonymous("22"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__168'(G, H, A :: [I|J], K, L),
	(
	    'CHRhead_not_kept'(W)
	->
	    A :: V
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__168'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__168'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__168'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__168' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__170'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__170'(['CHR::_2'(A :: [B|C], D, E, F)|G], H, A :: I : J, K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("29"), H, D, K, E),
	coca(try_double(L, A :: I : J, F, A :: [B|C], M :: N : O, M :: [P|Q], augmentation, ((remove_lower(N, [P|Q], R), remove_higher(O, R, S)), 'CHRkeep_heads_checking'(M :: N : O, T, M :: [P|Q], U, M :: S, V)), 'CHRhead_not_kept'(V) -> M :: S ; true, anonymous("29"))),
	no_delayed_goals(((remove_lower(I, [B|C], W), remove_higher(J, W, X)), 'CHRkeep_heads_checking'(A :: I : J, H, A :: [B|C], D, A :: X, Y))),
	!,
	coca(fired_rule(anonymous("29"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__170'(G, H, A :: I : J, K, L),
	(
	    'CHRhead_not_kept'(Y)
	->
	    A :: X
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__170'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__170'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__170'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__170' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__172'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__172'(['CHR::_2'(A :: B : C, D, E, F)|G], H, A :: [I|J], K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("29"), H, D, K, E),
	coca(try_double(L, A :: [I|J], F, A :: B : C, M :: [N|O], M :: P : Q, augmentation, ((remove_lower(P, [N|O], R), remove_higher(Q, R, S)), 'CHRkeep_heads_checking'(M :: P : Q, T, M :: [N|O], U, M :: S, V)), 'CHRhead_not_kept'(V) -> M :: S ; true, anonymous("29"))),
	no_delayed_goals(((remove_lower(B, [I|J], W), remove_higher(C, W, X)), 'CHRkeep_heads_checking'(A :: B : C, D, A :: [I|J], H, A :: X, Y))),
	!,
	coca(fired_rule(anonymous("29"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__172'(G, H, A :: [I|J], K, L),
	(
	    'CHRhead_not_kept'(Y)
	->
	    A :: X
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__172'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__172'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__172'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__172' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__174'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__174'(['CHRle_2'(le(A, B), C, D, E)|F], G, B :: [H|I], J, K) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(anonymous("30"), G, C, J, D),
	coca(try_double(K, B :: [H|I], E, le(A, B), L :: [M|N], le(O, L), augmentation, var(O), (last([M|N], P), le(O, P)), anonymous("30"))),
	no_delayed_goals(var(A)),
	!,
	coca(fired_rule(anonymous("30"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__174'(F, G, B :: [H|I], J, K),
	last([H|I], Q),
	le(A, Q).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__174'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__174'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__174'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__174' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__176'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__176'(['CHRle_2'(le(A, B), C, D, E)|F], G, A :: [H|I], J, K) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(anonymous("31"), G, C, J, D),
	coca(try_double(K, A :: [H|I], E, le(A, B), L :: [M|N], le(L, O), augmentation, var(O), le(M, O), anonymous("31"))),
	no_delayed_goals(var(B)),
	!,
	coca(fired_rule(anonymous("31"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__176'(F, G, A :: [H|I], J, K),
	le(H, B).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__176'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__176'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__176'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__176' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__178'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__178'(['CHRlt_2'(lt(A, B), C, D, E)|F], G, B :: [H|I], J, K) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(anonymous("32"), G, C, J, D),
	coca(try_double(K, B :: [H|I], E, lt(A, B), L :: [M|N], lt(O, L), augmentation, var(O), (last([M|N], P), lt(O, P)), anonymous("32"))),
	no_delayed_goals(var(A)),
	!,
	coca(fired_rule(anonymous("32"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__178'(F, G, B :: [H|I], J, K),
	last([H|I], Q),
	lt(A, Q).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__178'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__178'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__178'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__178' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__180'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__180'(['CHRlt_2'(lt(A, B), C, D, E)|F], G, A :: [H|I], J, K) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(anonymous("33"), G, C, J, D),
	coca(try_double(K, A :: [H|I], E, lt(A, B), L :: [M|N], lt(L, O), augmentation, var(O), lt(M, O), anonymous("33"))),
	no_delayed_goals(var(B)),
	!,
	coca(fired_rule(anonymous("33"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__180'(F, G, A :: [H|I], J, K),
	lt(H, B).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__180'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__180'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__180'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__180' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__182'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__182'(['CHR::_2'(A :: B : C, D, E, F)|G], H, A :: I : J, K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(anonymous("38")), H, D, K, E),
	coca(try_double(L, A :: I : J, F, A :: B : C, M :: N : O, M :: P : Q, augmentation, ((maximum(N, P, R), minimum(O, Q, S)), 'CHRkeep_heads_checking'(M :: N : O, T, M :: P : Q, U, M :: R : S, V)), 'CHRhead_not_kept'(V) -> M :: R : S ; true, anonymous("38"))),
	no_delayed_goals(((maximum(I, B, W), minimum(J, C, X)), 'CHRkeep_heads_checking'(A :: I : J, H, A :: B : C, D, A :: W : X, Y))),
	!,
	coca(fired_rule(anonymous("38"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__182'(G, H, A :: I : J, K, L),
	(
	    'CHRhead_not_kept'(Y)
	->
	    A :: W : X
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__182'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__182'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__182'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__182' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__184'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__184'(['CHR::_2'(A :: B : C, D, E, F)|G], H, A :: I : J, K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('21'(anonymous("38")), H, D, K, E),
	coca(try_double(L, A :: I : J, F, A :: B : C, M :: N : O, M :: P : Q, augmentation, ((maximum(P, N, R), minimum(Q, O, S)), 'CHRkeep_heads_checking'(M :: P : Q, T, M :: N : O, U, M :: R : S, V)), 'CHRhead_not_kept'(V) -> M :: R : S ; true, anonymous("38"))),
	no_delayed_goals(((maximum(B, I, W), minimum(C, J, X)), 'CHRkeep_heads_checking'(A :: B : C, D, A :: I : J, H, A :: W : X, Y))),
	!,
	coca(fired_rule(anonymous("38"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__184'(G, H, A :: I : J, K, L),
	(
	    'CHRhead_not_kept'(Y)
	->
	    A :: W : X
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__184'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__184'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__184'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__184' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__186'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__186'(['CHRle_2'(le(A, B), C, D, E)|F], G, B :: H : I, J, K) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(anonymous("45"), G, C, J, D),
	coca(try_double(K, B :: H : I, E, le(A, B), L :: M : N, le(O, L), augmentation, var(O), le(O, N), anonymous("45"))),
	no_delayed_goals(var(A)),
	!,
	coca(fired_rule(anonymous("45"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__186'(F, G, B :: H : I, J, K),
	le(A, I).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__186'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__186'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__186'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__186' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__188'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__188'(['CHRle_2'(le(A, B), C, D, E)|F], G, A :: H : I, J, K) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(anonymous("46"), G, C, J, D),
	coca(try_double(K, A :: H : I, E, le(A, B), L :: M : N, le(L, O), augmentation, var(O), le(M, O), anonymous("46"))),
	no_delayed_goals(var(B)),
	!,
	coca(fired_rule(anonymous("46"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__188'(F, G, A :: H : I, J, K),
	le(H, B).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__188'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__188'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__188'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__188' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__190'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__190'(['CHRlt_2'(lt(A, B), C, D, E)|F], G, B :: H : I, J, K) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(anonymous("47"), G, C, J, D),
	coca(try_double(K, B :: H : I, E, lt(A, B), L :: M : N, lt(O, L), augmentation, var(O), lt(O, N), anonymous("47"))),
	no_delayed_goals(var(A)),
	!,
	coca(fired_rule(anonymous("47"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__190'(F, G, B :: H : I, J, K),
	lt(A, I).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__190'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__190'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__190'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__190' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__192'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__192'(['CHRlt_2'(lt(A, B), C, D, E)|F], G, A :: H : I, J, K) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'(anonymous("48"), G, C, J, D),
	coca(try_double(K, A :: H : I, E, lt(A, B), L :: M : N, lt(L, O), augmentation, var(O), lt(M, O), anonymous("48"))),
	no_delayed_goals(var(B)),
	!,
	coca(fired_rule(anonymous("48"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__192'(F, G, A :: H : I, J, K),
	lt(H, B).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__192'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__192'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__192'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__192' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__194'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__194'(['CHRmult_3'(mult(A, B, C), D, E, F)|G], H, A :: I : J, K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("51"), H, D, K, E),
	coca(try_double(L, A :: I : J, F, mult(A, B, C), M :: N : O, mult(M, P, Q), augmentation, true, (R is (Q - 1) // O + 1, S is Q // N, P :: R : S), anonymous("51"))),
	!,
	coca(fired_rule(anonymous("51"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__194'(G, H, A :: I : J, K, L),
	T is (C - 1) // J + 1,
	U is C // I,
	B :: T : U.
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__194'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__194'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__194'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__194' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195'(A :: B : C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__196'(G, D, A :: B : C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__196'(['CHRmult_3'(mult(A, B, C), D, E, F)|G], H, B :: I : J, K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("52"), H, D, K, E),
	coca(try_double(L, B :: I : J, F, mult(A, B, C), M :: N : O, mult(P, M, Q), augmentation, true, (R is (Q - 1) // O + 1, S is Q // N, P :: R : S), anonymous("52"))),
	!,
	coca(fired_rule(anonymous("52"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__196'(G, H, B :: I : J, K, L),
	T is (C - 1) // J + 1,
	U is C // I,
	A :: T : U.
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__196'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__196'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__196'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__196' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__198'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__198'(['CHR::_2'(A - B :: C, D, E, F)|G], H, A :: [I|J], K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(anonymous("56")), H, D, K, E),
	coca(try_double(L, A :: [I|J], F, A - B :: C, M :: [N|O], M - P :: Q, augmentation, ((intersect(R :: [N|O], R - S :: Q, R - S :: T), length(Q, U), length(T, V), U > V), 'CHRkeep_heads_checking'(M :: [N|O], W, M - P :: Q, X, M - P :: T, Y)), 'CHRhead_not_kept'(Y) -> M - P :: T ; true, anonymous("56"))),
	no_delayed_goals(((intersect(Z :: [I|J], Z - A1 :: C, Z - A1 :: B1), length(C, C1), length(B1, D1), C1 > D1), 'CHRkeep_heads_checking'(A :: [I|J], H, A - B :: C, D, A - B :: B1, E1))),
	!,
	coca(fired_rule(anonymous("56"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__198'(G, H, A :: [I|J], K, L),
	(
	    'CHRhead_not_kept'(E1)
	->
	    A - B :: B1
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__198'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__198'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__198'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__198' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199'(A - B :: C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__200'(G, D, A - B :: C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__200'(['CHR::_2'(A :: [B|C], D, E, F)|G], H, A - I :: J, K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('21'(anonymous("56")), H, D, K, E),
	coca(try_double(L, A - I :: J, F, A :: [B|C], M - N :: O, M :: [P|Q], augmentation, ((intersect(R :: [P|Q], R - S :: O, R - S :: T), length(O, U), length(T, V), U > V), 'CHRkeep_heads_checking'(M :: [P|Q], W, M - N :: O, X, M - N :: T, Y)), 'CHRhead_not_kept'(Y) -> M - N :: T ; true, anonymous("56"))),
	no_delayed_goals(((intersect(Z :: [B|C], Z - A1 :: J, Z - A1 :: B1), length(J, C1), length(B1, D1), C1 > D1), 'CHRkeep_heads_checking'(A :: [B|C], D, A - I :: J, H, A - I :: B1, E1))),
	!,
	coca(fired_rule(anonymous("56"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__200'(G, H, A - I :: J, K, L),
	(
	    'CHRhead_not_kept'(E1)
	->
	    A - I :: B1
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__200'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__200'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__200'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__200' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201'(A :: [B|C], D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__202'(G, D, A :: [B|C], E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__202'(['CHR::_2'(A - B :: C, D, E, F)|G], H, B :: [I|J], K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(anonymous("57")), H, D, K, E),
	coca(try_double(L, B :: [I|J], F, A - B :: C, M :: [N|O], P - M :: Q, augmentation, ((intersect(R :: [N|O], S - R :: Q, S - R :: T), length(Q, U), length(T, V), U > V), 'CHRkeep_heads_checking'(M :: [N|O], W, P - M :: Q, X, P - M :: T, Y)), 'CHRhead_not_kept'(Y) -> P - M :: T ; true, anonymous("57"))),
	no_delayed_goals(((intersect(Z :: [I|J], A1 - Z :: C, A1 - Z :: B1), length(C, C1), length(B1, D1), C1 > D1), 'CHRkeep_heads_checking'(B :: [I|J], H, A - B :: C, D, A - B :: B1, E1))),
	!,
	coca(fired_rule(anonymous("57"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__202'(G, H, B :: [I|J], K, L),
	(
	    'CHRhead_not_kept'(E1)
	->
	    A - B :: B1
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__202'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__202'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__202'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__202' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203'(A - B :: C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__204'(G, D, A - B :: C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__204'(['CHR::_2'(A :: [B|C], D, E, F)|G], H, I - A :: J, K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('21'(anonymous("57")), H, D, K, E),
	coca(try_double(L, I - A :: J, F, A :: [B|C], M - N :: O, N :: [P|Q], augmentation, ((intersect(R :: [P|Q], S - R :: O, S - R :: T), length(O, U), length(T, V), U > V), 'CHRkeep_heads_checking'(N :: [P|Q], W, M - N :: O, X, M - N :: T, Y)), 'CHRhead_not_kept'(Y) -> M - N :: T ; true, anonymous("57"))),
	no_delayed_goals(((intersect(Z :: [B|C], A1 - Z :: J, A1 - Z :: B1), length(J, C1), length(B1, D1), C1 > D1), 'CHRkeep_heads_checking'(A :: [B|C], D, I - A :: J, H, I - A :: B1, E1))),
	!,
	coca(fired_rule(anonymous("57"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__204'(G, H, I - A :: J, K, L),
	(
	    'CHRhead_not_kept'(E1)
	->
	    I - A :: B1
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__204'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__204'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__204'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__204' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205'(A - B :: C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__206'(G, D, A - B :: C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__206'(['CHR::_2'(A - B :: C, D, E, F)|G], H, B - A :: I, J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(anonymous("58")), H, D, J, E),
	coca(try_double(K, B - A :: I, F, A - B :: C, L - M :: N, M - L :: O, augmentation, (intersect(P - Q :: N, Q - P :: O, P - Q :: R), 'CHRkeep_heads_checking'(L - M :: N, S, M - L :: O, T, L - M :: R, U)), 'CHRhead_not_kept'(U) -> L - M :: R ; true, anonymous("58"))),
	no_delayed_goals((intersect(V - W :: I, W - V :: C, V - W :: X), 'CHRkeep_heads_checking'(B - A :: I, H, A - B :: C, D, B - A :: X, Y))),
	!,
	coca(fired_rule(anonymous("58"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__206'(G, H, B - A :: I, J, K),
	(
	    'CHRhead_not_kept'(Y)
	->
	    B - A :: X
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__206'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__206'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__206'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__206' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207'(A - B :: C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__208'(G, D, A - B :: C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__208'(['CHR::_2'(A - B :: C, D, E, F)|G], H, B - A :: I, J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('21'(anonymous("58")), H, D, J, E),
	coca(try_double(K, B - A :: I, F, A - B :: C, L - M :: N, M - L :: O, augmentation, (intersect(P - Q :: O, Q - P :: N, P - Q :: R), 'CHRkeep_heads_checking'(M - L :: O, S, L - M :: N, T, M - L :: R, U)), 'CHRhead_not_kept'(U) -> M - L :: R ; true, anonymous("58"))),
	no_delayed_goals((intersect(V - W :: C, W - V :: I, V - W :: X), 'CHRkeep_heads_checking'(A - B :: C, D, B - A :: I, H, A - B :: X, Y))),
	!,
	coca(fired_rule(anonymous("58"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__208'(G, H, B - A :: I, J, K),
	(
	    'CHRhead_not_kept'(Y)
	->
	    A - B :: X
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__208'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__208'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__208'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__208' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209'(A - B :: C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__210'(G, D, A - B :: C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__210'(['CHR::_2'(A - B :: C, D, E, F)|G], H, A - B :: I, J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('12'(anonymous("59")), H, D, J, E),
	coca(try_double(K, A - B :: I, F, A - B :: C, L - M :: N, L - M :: O, augmentation, (intersect(P - Q :: N, P - Q :: O, P - Q :: R), 'CHRkeep_heads_checking'(L - M :: N, S, L - M :: O, T, L - M :: R, U)), 'CHRhead_not_kept'(U) -> L - M :: R ; true, anonymous("59"))),
	no_delayed_goals((intersect(V - W :: I, V - W :: C, V - W :: X), 'CHRkeep_heads_checking'(A - B :: I, H, A - B :: C, D, A - B :: X, Y))),
	!,
	coca(fired_rule(anonymous("59"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__210'(G, H, A - B :: I, J, K),
	(
	    'CHRhead_not_kept'(Y)
	->
	    A - B :: X
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__210'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__210'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__210'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__210' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211'(A - B :: C, D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211__212'(G, D, A - B :: C, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211'(A :: B, C, D, E) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211__213'(A :: B, C, D, E).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211' / 4, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211__212'(['CHR::_2'(A - B :: C, D, E, F)|G], H, A - B :: I, J, K) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('21'(anonymous("59")), H, D, J, E),
	coca(try_double(K, A - B :: I, F, A - B :: C, L - M :: N, L - M :: O, augmentation, (intersect(P - Q :: O, P - Q :: N, P - Q :: R), 'CHRkeep_heads_checking'(L - M :: O, S, L - M :: N, T, L - M :: R, U)), 'CHRhead_not_kept'(U) -> L - M :: R ; true, anonymous("59"))),
	no_delayed_goals((intersect(V - W :: C, V - W :: I, V - W :: X), 'CHRkeep_heads_checking'(A - B :: C, D, A - B :: I, H, A - B :: X, Y))),
	!,
	coca(fired_rule(anonymous("59"))),
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211__212'(G, H, A - B :: I, J, K),
	(
	    'CHRhead_not_kept'(Y)
	->
	    A - B :: X
	;
	    true
	).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211__212'([A|B], C, D, E, F) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211__212'(B, C, D, E, F).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211__212'([], A, B, C, D) :-
	'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211__213'(B, A, C, D).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211__212' / 5, leash, notrace).
'CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211__213'(A :: B, C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, A :: B], 'CHR::_2'(A :: B, C, D, E))
	;
	    true
	).
:- set_flag('CHR::_2__126__129__131__133__135__137__139__141__143__145__147__149__151__153__155__157__159__161__163__165__167__169__171__173__175__177__179__181__183__185__187__189__191__193__195__197__199__201__203__205__207__209__211__213' / 4, leash, notrace).
mult(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, mult(A, B, C))),
	'CHRmult_3'(mult(A, B, C), E, F, D).



%%% Rules handling for mult / 3

'CHRmult_3'(mult(A, B, C), D, E, F) :-
	(
	    'CHRnonvar'(D)
	;
	    'CHRalready_in'('CHRmult_3'(mult(A, B, C), D, E, F)),
	    coca(already_in)
	),
	!.
'CHRmult_3'(mult(A, B, C), D, E, F) ?-
	coca(try_rule(F, mult(A, B, C), anonymous("49"), mult(G, H, I), replacement, ground(G), G =:= 0 -> I =:= 0 ; (0 =:= I mod G, H is I // G))),
	no_delayed_goals(ground(A)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("49"))),
	(
	    A =:= 0
	->
	    C =:= 0
	;
	    0 =:= C mod A,
	    B is C // A
	).
'CHRmult_3'(mult(A, B, C), D, E, F) ?-
	coca(try_rule(F, mult(A, B, C), anonymous("50"), mult(G, H, I), replacement, ground(H), H =:= 0 -> I =:= 0 ; (0 =:= I mod H, G is I // H))),
	no_delayed_goals(ground(B)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("50"))),
	(
	    B =:= 0
	->
	    C =:= 0
	;
	    0 =:= C mod B,
	    A is C // B
	).
'CHRmult_3'(mult(A, B, C), D, E, F) :-
	'CHRmult_3__214'(mult(A, B, C), D, E, F).
:- set_flag('CHRmult_3' / 4, leash, notrace).
:- current_macro('CHRmult_3' / 4, _68465, _68466, _68467) -> true ; define_macro('CHRmult_3' / 4, tr_chr / 2, [write]).
'CHRmult_3__214'(A, B, C, D) :-
	'CHRmult_3__215'(A, B, C, D).
:- set_flag('CHRmult_3__214' / 4, leash, notrace).
'CHRmult_3__215'(mult(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRmult_3__215__216'(G, D, mult(A, B, C), E, F).
'CHRmult_3__215'(mult(A, B, C), D, E, F) :-
	'CHRmult_3__215__217'(mult(A, B, C), D, E, F).
:- set_flag('CHRmult_3__215' / 4, leash, notrace).
'CHRmult_3__215__216'(['CHR::_2'(A :: B : C, D, E, F)|G], H, mult(A, I, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("51"), H, D, K, E),
	coca(try_double(L, mult(A, I, J), F, A :: B : C, mult(M, N, O), M :: P : Q, augmentation, true, (R is (O - 1) // Q + 1, S is O // P, N :: R : S), anonymous("51"))),
	!,
	coca(fired_rule(anonymous("51"))),
	'CHRmult_3__215__216'(G, H, mult(A, I, J), K, L),
	T is (J - 1) // C + 1,
	U is J // B,
	I :: T : U.
'CHRmult_3__215__216'([A|B], C, D, E, F) :-
	'CHRmult_3__215__216'(B, C, D, E, F).
'CHRmult_3__215__216'([], A, B, C, D) :-
	'CHRmult_3__215__217'(B, A, C, D).
:- set_flag('CHRmult_3__215__216' / 5, leash, notrace).
'CHRmult_3__215__217'(mult(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRmult_3__215__217__218'(G, D, mult(A, B, C), E, F).
'CHRmult_3__215__217'(mult(A, B, C), D, E, F) :-
	'CHRmult_3__215__217__219'(mult(A, B, C), D, E, F).
:- set_flag('CHRmult_3__215__217' / 4, leash, notrace).
'CHRmult_3__215__217__218'(['CHR::_2'(A :: B : C, D, E, F)|G], H, mult(I, A, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("52"), H, D, K, E),
	coca(try_double(L, mult(I, A, J), F, A :: B : C, mult(M, N, O), N :: P : Q, augmentation, true, (R is (O - 1) // Q + 1, S is O // P, M :: R : S), anonymous("52"))),
	!,
	coca(fired_rule(anonymous("52"))),
	'CHRmult_3__215__217__218'(G, H, mult(I, A, J), K, L),
	T is (J - 1) // C + 1,
	U is J // B,
	I :: T : U.
'CHRmult_3__215__217__218'([A|B], C, D, E, F) :-
	'CHRmult_3__215__217__218'(B, C, D, E, F).
'CHRmult_3__215__217__218'([], A, B, C, D) :-
	'CHRmult_3__215__217__219'(B, A, C, D).
:- set_flag('CHRmult_3__215__217__218' / 5, leash, notrace).
'CHRmult_3__215__217__219'(mult(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, mult(A, B, C)], 'CHRmult_3'(mult(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRmult_3__215__217__219' / 4, leash, notrace).
atmost(A, B, C, D) :-
	'CHRgen_num'(E),
	coca(add_one_constraint(E, atmost(A, B, C, D))),
	'CHRatmost_4'(atmost(A, B, C, D), F, G, E).



%%% Rules handling for atmost / 4

'CHRatmost_4'(atmost(A, B, C, D), E, F, G) :-
	(
	    'CHRnonvar'(E)
	;
	    'CHRalready_in'('CHRatmost_4'(atmost(A, B, C, D), E, F, G)),
	    coca(already_in)
	),
	!.
'CHRatmost_4'(atmost(A, B, C, D), E, F, G) ?-
	coca(try_rule(G, atmost(A, B, C, D), anonymous("67"), atmost(H, I, J, K), replacement, K =< H, true)),
	no_delayed_goals(D =< A),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("67"))).
'CHRatmost_4'(atmost(0, A, B, C), D, E, F) ?-
	coca(try_rule(F, atmost(0, A, B, C), anonymous("68"), atmost(0, G, H, I), replacement, ground(H) ; ground(G), outof(H, G))),
	no_delayed_goals(ground(B) ; ground(A)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("68"))),
	outof(B, A).
'CHRatmost_4'(atmost(A, B, C, D), E, F, G) ?-
	coca(try_rule(G, atmost(A, B, C, D), anonymous("69"), atmost(H, I, J, K), replacement, (K > H, ground(J), delete_ground(L, I, M)), ((L == J -> N is H - 1 ; N = H), O is K - 1, atmost(N, M, J, O)))),
	no_delayed_goals((D > A, ground(C), delete_ground(P, B, Q))),
	!,
	'CHRkill'(E),
	coca(fired_rule(anonymous("69"))),
	(
	    P == C
	->
	    R is A - 1
	;
	    R = A
	),
	S is D - 1,
	atmost(R, Q, C, S).
'CHRatmost_4'(atmost(A, B, C, D), E, F, G) :-
	'CHRatmost_4__220'(atmost(A, B, C, D), E, F, G).
:- set_flag('CHRatmost_4' / 4, leash, notrace).
:- current_macro('CHRatmost_4' / 4, _70503, _70504, _70505) -> true ; define_macro('CHRatmost_4' / 4, tr_chr / 2, [write]).
'CHRatmost_4__220'(A, B, C, D) :-
	'CHRatmost_4__221'(A, B, C, D).
:- set_flag('CHRatmost_4__220' / 4, leash, notrace).
'CHRatmost_4__221'(atmost(A, B, C, D), E, F, G) :-
	(
	    'CHRvar'(E)
	->
	    'CHRdelay'([E, atmost(A, B, C, D)], 'CHRatmost_4'(atmost(A, B, C, D), E, F, G))
	;
	    true
	).
:- set_flag('CHRatmost_4__221' / 4, leash, notrace).



%%% Prolog clauses for alldistinct / 1

clause_alldistinct([]).
clause_alldistinct([A|B]) :-
	outof(A, B),
	alldistinct(B).
:- current_macro(clause_alldistinct / 1, _70773, _70774, _70775) -> true ; define_macro(clause_alldistinct / 1, tr_chr / 2, [write]).
alldistinct(A) :-
	'CHRgen_num'(B),
	coca(add_one_constraint(B, alldistinct(A))),
	'CHRalldistinct_1'(alldistinct(A), C, D, B).



%%% Rules handling for alldistinct / 1

'CHRalldistinct_1'(alldistinct(A), B, C, D) :-
	(
	    'CHRnonvar'(B)
	;
	    'CHRalready_in'('CHRalldistinct_1'(alldistinct(A), B, C, D)),
	    coca(already_in)
	),
	!.
'CHRalldistinct_1'(alldistinct([]), A, B, C) ?-
	coca(try_rule(C, alldistinct([]), anonymous("70"), alldistinct([]), replacement, true, true)),
	!,
	'CHRkill'(A),
	coca(fired_rule(anonymous("70"))).
'CHRalldistinct_1'(alldistinct([A]), B, C, D) ?-
	coca(try_rule(D, alldistinct([A]), anonymous("71"), alldistinct([E]), replacement, true, true)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("71"))).
'CHRalldistinct_1'(alldistinct([A, B]), C, D, E) ?-
	coca(try_rule(E, alldistinct([A, B]), anonymous("72"), alldistinct([F, G]), replacement, true, ne(F, G))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("72"))),
	ne(A, B).
'CHRalldistinct_1'(alldistinct([A|B]), C, D, E) ?-
	coca(try_rule(E, alldistinct([A|B]), anonymous("73"), alldistinct([F|G]), replacement, delete_ground(H, [F|G], I), (outof(H, I), alldistinct(I)))),
	no_delayed_goals(delete_ground(J, [A|B], K)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("73"))),
	outof(J, K),
	alldistinct(K).
'CHRalldistinct_1'(alldistinct(A), B, C, D) :-
	'CHRalldistinct_1__222'(alldistinct(A), B, C, D).
:- set_flag('CHRalldistinct_1' / 4, leash, notrace).
:- current_macro('CHRalldistinct_1' / 4, _71513, _71514, _71515) -> true ; define_macro('CHRalldistinct_1' / 4, tr_chr / 2, [write]).
'CHRalldistinct_1__222'(A, B, C, D) :-
	'CHRalldistinct_1__223'(A, B, C, D).
:- set_flag('CHRalldistinct_1__222' / 4, leash, notrace).
'CHRalldistinct_1__223'(alldistinct(A), B, C, D) :-
	(
	    'CHRvar'(B)
	->
	    'CHRdelay'([B, alldistinct(A)], 'CHRalldistinct_1'(alldistinct(A), B, C, D))
	;
	    true
	).
:- set_flag('CHRalldistinct_1__223' / 4, leash, notrace).
alldistinct1(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, alldistinct1(A, B))),
	'CHRalldistinct1_2'(alldistinct1(A, B), D, E, C).



%%% Rules handling for alldistinct1 / 2

'CHRalldistinct1_2'(alldistinct1(A, B), C, D, E) :-
	(
	    'CHRnonvar'(C)
	;
	    'CHRalready_in'('CHRalldistinct1_2'(alldistinct1(A, B), C, D, E)),
	    coca(already_in)
	),
	!.
'CHRalldistinct1_2'(alldistinct1(A, []), B, C, D) ?-
	coca(try_rule(D, alldistinct1(A, []), anonymous("74"), alldistinct1(E, []), replacement, true, true)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("74"))).
'CHRalldistinct1_2'(alldistinct1(A, [B]), C, D, E) ?-
	coca(try_rule(E, alldistinct1(A, [B]), anonymous("76"), alldistinct1(F, [G]), replacement, ground(F) ; ground(G), outof(G, F))),
	no_delayed_goals(ground(A) ; ground(B)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("76"))),
	outof(B, A).
'CHRalldistinct1_2'(alldistinct1(A, [B|C]), D, E, F) ?-
	coca(try_rule(F, alldistinct1(A, [B|C]), anonymous("77"), alldistinct1(G, [H|I]), replacement, (ground(G), delete_ground(J, [H|I], K)), member(J, G) -> fail ; alldistinct1([J|G], K))),
	no_delayed_goals((ground(A), delete_ground(L, [B|C], M))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("77"))),
	(
	    member(L, A)
	->
	    fail
	;
	    alldistinct1([L|A], M)
	).
'CHRalldistinct1_2'(alldistinct1(A, [B]), C, D, E) ?-
	'CHRget_delayed_goals'(B, F),
	'CHRalldistinct1_2__225'(F, [B], [G, H, I], J),
	coca(try_double(E, alldistinct1(A, [B]), J, B :: [I|H], alldistinct1(K, [L]), L :: [M|N], keep_second, (ground(K), 'CHRkeep_heads_checking'(L :: [M|N], O, L :: P, Q)), (remove_list(K, [M|N], P), ('CHRhead_not_kept'(Q) -> L :: P ; true)), anonymous("75"))),
	no_delayed_goals((ground(A), 'CHRkeep_heads_checking'(B :: [I|H], G, B :: R, S))),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("75"))),
	remove_list(A, [I|H], R),
	(
	    'CHRhead_not_kept'(S)
	->
	    B :: R
	;
	    true
	).
'CHRalldistinct1_2'(alldistinct1(A, B), C, D, E) :-
	'CHRalldistinct1_2__224'(alldistinct1(A, B), C, D, E).
'CHRalldistinct1_2__225'(['CHR::_2'(A :: [B|C], D, E, F)|G], [A], [H, I, J], K) ?-
	'CHRvar'(D),
	'CHR='([D, C, B], [H, I, J]),
	'CHR='(F, K).
'CHRalldistinct1_2__225'([A|B], C, D, E) :-
	'CHRalldistinct1_2__225'(B, C, D, E).
:- set_flag('CHRalldistinct1_2__225' / 4, leash, notrace).
:- set_flag('CHRalldistinct1_2' / 4, leash, notrace).
:- current_macro('CHRalldistinct1_2' / 4, _72865, _72866, _72867) -> true ; define_macro('CHRalldistinct1_2' / 4, tr_chr / 2, [write]).
'CHRalldistinct1_2__224'(A, B, C, D) :-
	'CHRalldistinct1_2__226'(A, B, C, D).
:- set_flag('CHRalldistinct1_2__224' / 4, leash, notrace).
'CHRalldistinct1_2__226'(alldistinct1(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, alldistinct1(A, B)], 'CHRalldistinct1_2'(alldistinct1(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRalldistinct1_2__226' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
