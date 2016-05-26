
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- op(100, xfx, unif).
unif(_534, [_533|_532]) :- chr_unif(_534, _533, _532).
nth_member(1, [_2399|_2401], _2399).
nth_member(_2429, [_2437|_2423], _2422) :- _2429 > 1, plus(_2424, 1, _2429), nth_member(_2424, _2423, _2422).
islist([]) ?- true.
islist([_2468|_2463]) ?- islist(_2463).
unify1(_3171, _3170) :- chr_var(_3171), chr_var(_3170), _3171 = _3170.
unify1(_3199, _3198) :- chr_var(_3199), chr_nonvar(_3198), _3199 = _3198.
unify1(_3227, _3226) :- chr_nonvar(_3227), chr_var(_3226), _3227 = _3226.
unify1(_3256, _3255) :- chr_functor(_3256, _3262, _3257), chr_functor(_3255, _3262, _3257), unify_args(_3257, _3256, _3255).
unify_args(0, _3282, _3281).
unify_args(_3324, _3313, _3312) :- _3324 > 0, plus(_3314, 1, _3324), chr_arg(_3324, _3313, _3319), chr_arg(_3324, _3312, _3318), unify1(_3319, _3318), unify_args(_3314, _3313, _3312).
unify2(_3360, _3359) :- chr_var(_3360), chr_var(_3359), _3360 = _3359.
unify2(_3388, _3387) :- chr_var(_3388), chr_nonvar(_3387), _3388 = _3387.
unify2(_3415, _3416) :- chr_nonvar(_3415), chr_var(_3416), _3416 = _3415.
unify2(_3457, _3451) :- unif(_3457, [_3450|_3444]), unif(_3451, [_3450|_3443]), unify_list(_3444, _3443).
unify_list([], []).
unify_list([_3493|_3488], [_3492|_3487]) :- unify2(_3493, _3492), unify_list(_3488, _3487).
varlist(_3516, _3515) :- varlist(_3516, [], _3515).
varlist(_3537, _3536, [_3537|_3536]) :- chr_var(_3537), diff_list(_3537, _3536).
varlist(_3563, _3562, _3562) :- chr_var(_3563), member(_3563, _3562).
varlist(_3590, _3589, _3588) :- chr_functor(_3590, _3594, _3591), varlist(_3591, _3590, _3589, _3588).
varlist(0, _3613, _3612, _3612).
varlist(_3655, _3643, _3649, _3641) :- _3655 > 0, plus(_3644, 1, _3655), chr_arg(_3655, _3643, _3650), varlist(_3650, _3649, _3642), varlist(_3644, _3643, _3642, _3641).
common_var(_3703, _3699, _3708) :- varlist(_3703, _3695), varlist(_3699, _3690), member(_3691, _3695), member(_3691, _3690).
ground0(_3722) :- varlist(_3722, []).
ground1(_3741) :- chr_functor(_3741, _3745, _3742), ground1(_3742, _3741).
ground1(0, _3758).
ground1(_3793, _3784) :- _3793 > 0, plus(_3785, 1, _3793), chr_arg(_3793, _3784, _3788), ground1(_3788), ground1(_3785, _3784).
ground2(_3825) :- unif(_3825, [_3823|_3819]), ground2l(_3819).
ground2l([]).
ground2l([_3855|_3852]) :- ground2(_3855), ground2l(_3852).
number_vars(_3889, _3898, _3903) :- var(_3889), plus(_3898, 1, _3903), name(_3898, _3887), name('V', [_3888]), name(_3889, [_3888|_3887]).
number_vars(_3935, _3928, _3927) :- unif(_3935, [_3933|_3929]), number_list(_3929, _3928, _3927).
number_list([], _3951, _3951).
number_list([_3977|_3971], _3976, _3969) :- number_vars(_3977, _3976, _3970), number_list(_3971, _3970, _3969).
undupvar(_4004, _4003, _4002, _4001) :- undupvar(_4004, _4003, [], _4002, [], _4001).
undupvar(_4027, _4027, _4026, [_4027|_4026], _4037, _4037) :- chr_var(_4027), diff_list(_4027, _4026).
undupvar(_4057, _4073, _4056, _4056, _4069, [_4073 = _4057|_4069]) :- chr_var(_4057), member(_4057, _4056).
undupvar(_4103, _4102, _4101, _4100, _4099, _4098) :- chr_functor(_4103, _4109, _4104), chr_functor(_4102, _4109, _4104), undupvar(_4104, _4103, _4102, _4101, _4100, _4099, _4098).
undupvar(0, _4139, _4138, _4137, _4137, _4136, _4136).
undupvar(_4195, _4178, _4177, _4188, _4175, _4187, _4173) :- _4195 > 0, plus(_4179, 1, _4195), chr_arg(_4195, _4178, _4190), chr_arg(_4195, _4177, _4189), undupvar(_4190, _4189, _4188, _4176, _4187, _4174), undupvar(_4179, _4178, _4177, _4176, _4175, _4174, _4173).
substitute(_4258, _4244, _4243, _4252) :- _4258 = _4244, _4252 = _4243 ; diff(_4258, _4244), unif(_4258, [_4251|_4245]), unif(_4252, [_4251|_4242]), substitute_list(_4245, _4244, _4243, _4242).
substitute_list([], _4285, _4286, []).
substitute_list([_4316|_4309], _4308, _4307, [_4315|_4306]) :- substitute(_4316, _4308, _4307, _4315), substitute_list(_4309, _4308, _4307, _4306).
pos(_4428, t(_4411, _4376, _4375, _4420, _4400) - _4421, _4368, _4367 - _4366) :- unif(_4428, [_4411|_4422]), pos_list(_4422, _4421, _4420, _4367 - _4384), string_length1(_4411, _4375), posdiff(_4384 - _4367, _4375, _4383), _4400 is (_4383 + (_4384 - _4367) mod 2) // 2, _4366 is _4384 + _4383, _4376 is _4375 // 2, _4368 is (_4367 + _4366) // 2.
pos_list([], [], [], _4461 - _4461).
pos_list([_4507|_4490], [_4506|_4489], [_4505|_4488], _4504 - _4486) :- pos(_4507, _4506, _4505, _4504 - _4495), plus(_4495, 2, _4487), pos_list(_4490, _4489, _4488, _4487 - _4486).
string_length1(_4546, _4541) :- atomic(_4546), name(_4546, _4542), length(_4542, _4541).
posdiff(_4575, _4576, 0) :- _4568 is _4576 - _4575, _4568 =< 0.
posdiff(_4604, _4605, _4597) :- _4597 is _4605 - _4604, _4597 > 0.
lsu(_4643, _4642, _4641) :- map(_4643, _4642, _4641, [], _4634), sort(0, =<, _4634, _4628), unify_duplicates(_4628).
map(_4682, _4681, _4680, _4679, _4678) :- chr_functor(_4682, _4687, _4677), chr_functor(_4681, _4687, _4677), chr_functor(_4680, _4687, _4677), map_arg(_4682, _4681, _4680, _4679, _4678, _4677 - 0).
map(_4746, _4741, _4764, _4759, [subst(_4746, _4741, _4764)|_4759]) :- chr_var(_4746) ; chr_var(_4741) ; chr_functor(_4746, _4736, _4731), chr_functor(_4741, _4735, _4730), (diff(_4736, _4735) ; diff(_4731, _4730)).
map_arg(_4800, _4799, _4798, _4782, _4781, _4787 - _4786) :- _4787 = _4786, _4782 = _4781.
map_arg(_4845, _4844, _4843, _4852, _4841, _4840 - _4872) :- _4840 > _4872, plus(_4872, 1, _4839), chr_arg(_4839, _4845, _4855), chr_arg(_4839, _4844, _4854), chr_arg(_4839, _4843, _4853), map(_4855, _4854, _4853, _4852, _4842), map_arg(_4845, _4844, _4843, _4842, _4841, _4840 - _4839).
unify_duplicates([subst(_4920, _4915, _4931)|_4904]) :- _4904 = [subst(_4919, _4914, _4930)|_4942], (_4920 = _4919, _4915 = _4914, _4931 = _4930 ; diff(_4920, _4919) ; diff(_4915, _4914)), unify_duplicates(_4904).
unify_duplicates([_4966]).
unify_duplicates([]).
chr_functor(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, chr_functor(A, B, C))),
	'CHRchr_functor_3'(chr_functor(A, B, C), E, F, D).



%%% Rules handling for chr_functor / 3

'CHRchr_functor_3'(chr_functor(A, B, C), D, E, F) :-
	'CHRnonvar'(D),
	!.
'CHRchr_functor_3'(chr_functor(A, B, C), D, E, F) ?-
	coca(try_rule(F, chr_functor(A, B, C), anonymous("0"), chr_functor(G, H, I), replacement, nonvar(G) ; (nonvar(H), nonvar(I)), functor(G, H, I))),
	no_delayed_goals(nonvar(A) ; (nonvar(B), nonvar(C))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("0"))),
	functor(A, B, C).
'CHRchr_functor_3'(chr_functor(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(A, G),
	'CHRchr_functor_3__26'(G, [A], [H, I], J),
	coca(try_double(F, chr_functor(A, B, C), J, chr_functor(A, I, H), chr_functor(K, L, M), chr_functor(K, N, O), keep_second, true, (L = N, M = O), anonymous("4"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("4"))),
	[B, C] = [I, H].
'CHRchr_functor_3'(chr_functor(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(A, G),
	'CHRchr_functor_3__27'(G, [A], [H, I], J),
	coca(try_double(F, chr_functor(A, B, C), J, chr_unif(A, I, H), chr_functor(K, L, M), chr_unif(K, N, O), keep_second, nonvar(M) ; islist(O), (L = N, length(O, M)), anonymous("15"))),
	no_delayed_goals(nonvar(C) ; islist(H)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("15"))),
	B = I,
	length(H, C).
'CHRchr_functor_3'(chr_functor(A, B, C), D, E, F) :-
	'CHRchr_functor_3__25'(chr_functor(A, B, C), D, E, F).
'CHRchr_functor_3__26'(['CHRchr_functor_3'(chr_functor(A, B, C), D, E, F)|G], [A], [H, I], J) ?-
	'CHRvar'(D),
	'CHR='([C, B], [H, I]),
	'CHR='(F, J).
'CHRchr_functor_3__26'([A|B], C, D, E) :-
	'CHRchr_functor_3__26'(B, C, D, E).
:- set_flag('CHRchr_functor_3__26' / 4, leash, notrace).
'CHRchr_functor_3__27'(['CHRchr_unif_3'(chr_unif(A, B, C), D, E, F)|G], [A], [H, I], J) ?-
	'CHRvar'(D),
	'CHR='([C, B], [H, I]),
	'CHR='(F, J).
'CHRchr_functor_3__27'([A|B], C, D, E) :-
	'CHRchr_functor_3__27'(B, C, D, E).
:- set_flag('CHRchr_functor_3__27' / 4, leash, notrace).
:- set_flag('CHRchr_functor_3' / 4, leash, notrace).
:- current_macro('CHRchr_functor_3' / 4, _6708, _6709, _6710) -> true ; define_macro('CHRchr_functor_3' / 4, tr_chr / 2, [write]).
'CHRchr_functor_3__25'(chr_functor(A, A, B), C, D, E) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'('CHRchr_functor_3__25', D),
	coca(try_rule(E, chr_functor(A, A, B), anonymous("1"), chr_functor(F, F, G), augmentation, true, G = 0)),
	!,
	'CHRchr_functor_3__25__29'(chr_functor(A, A, B), C, D, E),
	coca(fired_rule(anonymous("1"))),
	B = 0.
'CHRchr_functor_3__25'(A, B, C, D) ?-
	'CHRchr_functor_3__25__29'(A, B, C, D).
:- set_flag('CHRchr_functor_3__25' / 4, leash, notrace).
'CHRchr_functor_3__25__29'(chr_functor(A, B, 0), C, D, E) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'('CHRchr_functor_3__25__29', D),
	coca(try_rule(E, chr_functor(A, B, 0), anonymous("2"), chr_functor(F, G, 0), augmentation, true, F = G)),
	!,
	'CHRchr_functor_3__25__29__30'(chr_functor(A, B, 0), C, D, E),
	coca(fired_rule(anonymous("2"))),
	A = B.
'CHRchr_functor_3__25__29'(A, B, C, D) ?-
	'CHRchr_functor_3__25__29__30'(A, B, C, D).
:- set_flag('CHRchr_functor_3__25__29' / 4, leash, notrace).
'CHRchr_functor_3__25__29__30'(chr_functor(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('CHRchr_functor_3__25__29__30', E),
	coca(try_rule(F, chr_functor(A, B, C), anonymous("3"), chr_functor(G, H, I), augmentation, true, chr_nonvar(G))),
	!,
	'CHRchr_functor_3__25__29__30__31'(chr_functor(A, B, C), D, E, F),
	coca(fired_rule(anonymous("3"))),
	chr_nonvar(A).
'CHRchr_functor_3__25__29__30'(A, B, C, D) ?-
	'CHRchr_functor_3__25__29__30__31'(A, B, C, D).
:- set_flag('CHRchr_functor_3__25__29__30' / 4, leash, notrace).
'CHRchr_functor_3__25__29__30__31'(A, B, C, D) :-
	'CHRchr_functor_3__28'(A, B, C, D).
:- set_flag('CHRchr_functor_3__25__29__30__31' / 4, leash, notrace).
'CHRchr_functor_3__28'(chr_functor(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRchr_functor_3__28__32'(G, D, chr_functor(A, B, C), E, F).
'CHRchr_functor_3__28'(chr_functor(A, B, C), D, E, F) :-
	'CHRchr_functor_3__28__33'(chr_functor(A, B, C), D, E, F).
:- set_flag('CHRchr_functor_3__28' / 4, leash, notrace).
'CHRchr_functor_3__28__32'(['CHRchr_functor_3'(chr_functor(A, B, C), D, E, F)|G], H, chr_functor(A, I, J), K, L) ?-
	'CHRvar'(D),
	coca(try_double(L, chr_functor(A, I, J), F, chr_functor(A, B, C), chr_functor(M, N, O), chr_functor(M, P, Q), keep_first, true, (P = N, Q = O), anonymous("4"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("4"))),
	'CHRchr_functor_3__28__32'(G, H, chr_functor(A, I, J), K, L),
	[B, C] = [I, J].
'CHRchr_functor_3__28__32'([A|B], C, D, E, F) :-
	'CHRchr_functor_3__28__32'(B, C, D, E, F).
'CHRchr_functor_3__28__32'([], A, B, C, D) :-
	'CHRchr_functor_3__28__33'(B, A, C, D).
:- set_flag('CHRchr_functor_3__28__32' / 5, leash, notrace).
'CHRchr_functor_3__28__33'(chr_functor(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRchr_functor_3__28__33__34'(G, D, chr_functor(A, B, C), E, F).
'CHRchr_functor_3__28__33'(chr_functor(A, B, C), D, E, F) :-
	'CHRchr_functor_3__28__33__35'(chr_functor(A, B, C), D, E, F).
:- set_flag('CHRchr_functor_3__28__33' / 4, leash, notrace).
'CHRchr_functor_3__28__33__34'(['CHRchr_arg_3'(chr_arg(A, B, C), D, E, F)|G], H, chr_functor(B, I, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("5"), H, D, K, E),
	coca(try_double(L, chr_functor(B, I, J), F, chr_arg(A, B, C), chr_functor(M, N, O), chr_arg(P, M, Q), augmentation, true, (O >= P, O > 0), anonymous("5"))),
	!,
	coca(fired_rule(anonymous("5"))),
	'CHRchr_functor_3__28__33__34'(G, H, chr_functor(B, I, J), K, L),
	J >= A,
	J > 0.
'CHRchr_functor_3__28__33__34'([A|B], C, D, E, F) :-
	'CHRchr_functor_3__28__33__34'(B, C, D, E, F).
'CHRchr_functor_3__28__33__34'([], A, B, C, D) :-
	'CHRchr_functor_3__28__33__35'(B, A, C, D).
:- set_flag('CHRchr_functor_3__28__33__34' / 5, leash, notrace).
'CHRchr_functor_3__28__33__35'(chr_functor(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, chr_functor(A, B, C)], 'CHRchr_functor_3'(chr_functor(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRchr_functor_3__28__33__35' / 4, leash, notrace).
chr_arg(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, chr_arg(A, B, C))),
	'CHRchr_arg_3'(chr_arg(A, B, C), E, F, D).



%%% Rules handling for chr_arg / 3

'CHRchr_arg_3'(chr_arg(A, B, C), D, E, F) :-
	'CHRnonvar'(D),
	!.
'CHRchr_arg_3'(chr_arg(0, A, B), C, D, E) ?-
	coca(try_rule(E, chr_arg(0, A, B), anonymous("6"), chr_arg(0, F, G), replacement, true, fail)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("6"))),
	fail.
'CHRchr_arg_3'(chr_arg(A, B, C), D, E, F) ?-
	coca(try_rule(F, chr_arg(A, B, C), anonymous("7"), chr_arg(G, H, I), replacement, (nonvar(G), nonvar(H)), arg(G, H, I))),
	no_delayed_goals((nonvar(A), nonvar(B))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("7"))),
	arg(A, B, C).
'CHRchr_arg_3'(chr_arg(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRchr_arg_3__37'(G, [B, A], [H], I),
	coca(try_double(F, chr_arg(A, B, C), I, chr_arg(A, B, H), chr_arg(J, K, L), chr_arg(J, K, M), keep_second, true, L = M, anonymous("8"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("8"))),
	C = H.
'CHRchr_arg_3'(chr_arg(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(B, G),
	'CHRchr_arg_3__38'(G, [B], [H], I),
	coca(try_double(F, chr_arg(A, B, C), I, chr_unif(B, J, H), chr_arg(K, L, M), chr_unif(L, N, O), keep_second, nonvar(K), nth_member(K, O, M), anonymous("16"))),
	no_delayed_goals(nonvar(A)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("16"))),
	nth_member(A, H, C).
'CHRchr_arg_3'(chr_arg(A, B, C), D, E, F) :-
	'CHRchr_arg_3__36'(chr_arg(A, B, C), D, E, F).
'CHRchr_arg_3__37'(['CHRchr_arg_3'(chr_arg(A, B, C), D, E, F)|G], [B, A], [H], I) ?-
	'CHRvar'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHRchr_arg_3__37'([A|B], C, D, E) :-
	'CHRchr_arg_3__37'(B, C, D, E).
:- set_flag('CHRchr_arg_3__37' / 4, leash, notrace).
'CHRchr_arg_3__38'(['CHRchr_unif_3'(chr_unif(A, B, C), D, E, F)|G], [A], [H], I) ?-
	'CHRvar'(D),
	'CHR='([C], [H]),
	'CHR='(F, I).
'CHRchr_arg_3__38'([A|B], C, D, E) :-
	'CHRchr_arg_3__38'(B, C, D, E).
:- set_flag('CHRchr_arg_3__38' / 4, leash, notrace).
:- set_flag('CHRchr_arg_3' / 4, leash, notrace).
:- current_macro('CHRchr_arg_3' / 4, _10634, _10635, _10636) -> true ; define_macro('CHRchr_arg_3' / 4, tr_chr / 2, [write]).
'CHRchr_arg_3__36'(A, B, C, D) :-
	'CHRchr_arg_3__39'(A, B, C, D).
:- set_flag('CHRchr_arg_3__36' / 4, leash, notrace).
'CHRchr_arg_3__39'(chr_arg(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRchr_arg_3__39__40'(G, D, chr_arg(A, B, C), E, F).
'CHRchr_arg_3__39'(chr_arg(A, B, C), D, E, F) :-
	'CHRchr_arg_3__39__41'(chr_arg(A, B, C), D, E, F).
:- set_flag('CHRchr_arg_3__39' / 4, leash, notrace).
'CHRchr_arg_3__39__40'(['CHRchr_arg_3'(chr_arg(A, B, C), D, E, F)|G], H, chr_arg(A, B, I), J, K) ?-
	'CHRvar'(D),
	coca(try_double(K, chr_arg(A, B, I), F, chr_arg(A, B, C), chr_arg(L, M, N), chr_arg(L, M, O), keep_first, true, O = N, anonymous("8"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("8"))),
	'CHRchr_arg_3__39__40'(G, H, chr_arg(A, B, I), J, K),
	C = I.
'CHRchr_arg_3__39__40'([A|B], C, D, E, F) :-
	'CHRchr_arg_3__39__40'(B, C, D, E, F).
'CHRchr_arg_3__39__40'([], A, B, C, D) :-
	'CHRchr_arg_3__39__41'(B, A, C, D).
:- set_flag('CHRchr_arg_3__39__40' / 5, leash, notrace).
'CHRchr_arg_3__39__41'(chr_arg(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(B, G),
	'CHRchr_arg_3__39__41__42'(G, D, chr_arg(A, B, C), E, F).
'CHRchr_arg_3__39__41'(chr_arg(A, B, C), D, E, F) :-
	'CHRchr_arg_3__39__41__43'(chr_arg(A, B, C), D, E, F).
:- set_flag('CHRchr_arg_3__39__41' / 4, leash, notrace).
'CHRchr_arg_3__39__41__42'(['CHRchr_functor_3'(chr_functor(A, B, C), D, E, F)|G], H, chr_arg(I, A, J), K, L) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'(anonymous("5"), H, D, K, E),
	coca(try_double(L, chr_arg(I, A, J), F, chr_functor(A, B, C), chr_arg(M, N, O), chr_functor(N, P, Q), augmentation, true, (Q >= M, Q > 0), anonymous("5"))),
	!,
	coca(fired_rule(anonymous("5"))),
	'CHRchr_arg_3__39__41__42'(G, H, chr_arg(I, A, J), K, L),
	C >= I,
	C > 0.
'CHRchr_arg_3__39__41__42'([A|B], C, D, E, F) :-
	'CHRchr_arg_3__39__41__42'(B, C, D, E, F).
'CHRchr_arg_3__39__41__42'([], A, B, C, D) :-
	'CHRchr_arg_3__39__41__43'(B, A, C, D).
:- set_flag('CHRchr_arg_3__39__41__42' / 5, leash, notrace).
'CHRchr_arg_3__39__41__43'(chr_arg(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, chr_arg(A, B, C)], 'CHRchr_arg_3'(chr_arg(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRchr_arg_3__39__41__43' / 4, leash, notrace).
chr_unif(A, B, C) :-
	'CHRgen_num'(D),
	coca(add_one_constraint(D, chr_unif(A, B, C))),
	'CHRchr_unif_3'(chr_unif(A, B, C), E, F, D).



%%% Rules handling for chr_unif / 3

'CHRchr_unif_3'(chr_unif(A, B, C), D, E, F) :-
	'CHRnonvar'(D),
	!.
'CHRchr_unif_3'(chr_unif(A, B, C), D, E, F) ?-
	coca(try_rule(F, chr_unif(A, B, C), anonymous("9"), chr_unif(G, H, I), replacement, nonvar(G) ; (nonvar(H), islist(I)), G =.. [H|I])),
	no_delayed_goals(nonvar(A) ; (nonvar(B), islist(C))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("9"))),
	A =.. [B|C].
'CHRchr_unif_3'(chr_unif(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(A, G),
	'CHRchr_unif_3__45'(G, [A], [H, I], J),
	coca(try_double(F, chr_unif(A, B, C), J, chr_unif(A, I, H), chr_unif(K, L, M), chr_unif(K, N, O), keep_second, true, (L = N, M = O), anonymous("13"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("13"))),
	[B, C] = [I, H].
'CHRchr_unif_3'(chr_unif(A, B, C), D, E, F) ?-
	'CHRget_delayed_goals'(C, G),
	'CHRchr_unif_3__46'(G, [C, B], [H], I),
	coca(try_double(F, chr_unif(A, B, C), I, chr_unif(H, B, C), chr_unif(J, K, L), chr_unif(M, K, L), keep_second, true, J = M, anonymous("14"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("14"))),
	A = H.
'CHRchr_unif_3'(chr_unif(A, B, C), D, E, F) :-
	'CHRchr_unif_3__44'(chr_unif(A, B, C), D, E, F).
'CHRchr_unif_3__45'(['CHRchr_unif_3'(chr_unif(A, B, C), D, E, F)|G], [A], [H, I], J) ?-
	'CHRvar'(D),
	'CHR='([C, B], [H, I]),
	'CHR='(F, J).
'CHRchr_unif_3__45'([A|B], C, D, E) :-
	'CHRchr_unif_3__45'(B, C, D, E).
:- set_flag('CHRchr_unif_3__45' / 4, leash, notrace).
'CHRchr_unif_3__46'(['CHRchr_unif_3'(chr_unif(A, B, C), D, E, F)|G], [C, B], [H], I) ?-
	'CHRvar'(D),
	'CHR='([A], [H]),
	'CHR='(F, I).
'CHRchr_unif_3__46'([A|B], C, D, E) :-
	'CHRchr_unif_3__46'(B, C, D, E).
:- set_flag('CHRchr_unif_3__46' / 4, leash, notrace).
:- set_flag('CHRchr_unif_3' / 4, leash, notrace).
:- current_macro('CHRchr_unif_3' / 4, _13621, _13622, _13623) -> true ; define_macro('CHRchr_unif_3' / 4, tr_chr / 2, [write]).
'CHRchr_unif_3__44'(chr_unif(A, A, B), C, D, E) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'('CHRchr_unif_3__44', D),
	coca(try_rule(E, chr_unif(A, A, B), anonymous("10"), chr_unif(F, F, G), augmentation, true, G = [])),
	!,
	'CHRchr_unif_3__44__48'(chr_unif(A, A, B), C, D, E),
	coca(fired_rule(anonymous("10"))),
	B = [].
'CHRchr_unif_3__44'(A, B, C, D) ?-
	'CHRchr_unif_3__44__48'(A, B, C, D).
:- set_flag('CHRchr_unif_3__44' / 4, leash, notrace).
'CHRchr_unif_3__44__48'(chr_unif(A, B, []), C, D, E) ?-
	'CHRvar'(C),
	'CHRcheck_and_mark_applied'('CHRchr_unif_3__44__48', D),
	coca(try_rule(E, chr_unif(A, B, []), anonymous("11"), chr_unif(F, G, []), augmentation, true, F = G)),
	!,
	'CHRchr_unif_3__44__48__49'(chr_unif(A, B, []), C, D, E),
	coca(fired_rule(anonymous("11"))),
	A = B.
'CHRchr_unif_3__44__48'(A, B, C, D) ?-
	'CHRchr_unif_3__44__48__49'(A, B, C, D).
:- set_flag('CHRchr_unif_3__44__48' / 4, leash, notrace).
'CHRchr_unif_3__44__48__49'(chr_unif(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	'CHRcheck_and_mark_applied'('CHRchr_unif_3__44__48__49', E),
	coca(try_rule(F, chr_unif(A, B, C), anonymous("12"), chr_unif(G, H, I), augmentation, true, (chr_nonvar(G), chr_nonvar(I)))),
	!,
	'CHRchr_unif_3__44__48__49__50'(chr_unif(A, B, C), D, E, F),
	coca(fired_rule(anonymous("12"))),
	chr_nonvar(A),
	chr_nonvar(C).
'CHRchr_unif_3__44__48__49'(A, B, C, D) ?-
	'CHRchr_unif_3__44__48__49__50'(A, B, C, D).
:- set_flag('CHRchr_unif_3__44__48__49' / 4, leash, notrace).
'CHRchr_unif_3__44__48__49__50'(A, B, C, D) :-
	'CHRchr_unif_3__47'(A, B, C, D).
:- set_flag('CHRchr_unif_3__44__48__49__50' / 4, leash, notrace).
'CHRchr_unif_3__47'(chr_unif(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRchr_unif_3__47__51'(G, D, chr_unif(A, B, C), E, F).
'CHRchr_unif_3__47'(chr_unif(A, B, C), D, E, F) :-
	'CHRchr_unif_3__47__52'(chr_unif(A, B, C), D, E, F).
:- set_flag('CHRchr_unif_3__47' / 4, leash, notrace).
'CHRchr_unif_3__47__51'(['CHRchr_unif_3'(chr_unif(A, B, C), D, E, F)|G], H, chr_unif(A, I, J), K, L) ?-
	'CHRvar'(D),
	coca(try_double(L, chr_unif(A, I, J), F, chr_unif(A, B, C), chr_unif(M, N, O), chr_unif(M, P, Q), keep_first, true, (P = N, Q = O), anonymous("13"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("13"))),
	'CHRchr_unif_3__47__51'(G, H, chr_unif(A, I, J), K, L),
	[B, C] = [I, J].
'CHRchr_unif_3__47__51'([A|B], C, D, E, F) :-
	'CHRchr_unif_3__47__51'(B, C, D, E, F).
'CHRchr_unif_3__47__51'([], A, B, C, D) :-
	'CHRchr_unif_3__47__52'(B, A, C, D).
:- set_flag('CHRchr_unif_3__47__51' / 5, leash, notrace).
'CHRchr_unif_3__47__52'(chr_unif(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(C, G),
	'CHRchr_unif_3__47__52__53'(G, D, chr_unif(A, B, C), E, F).
'CHRchr_unif_3__47__52'(chr_unif(A, B, C), D, E, F) :-
	'CHRchr_unif_3__47__52__54'(chr_unif(A, B, C), D, E, F).
:- set_flag('CHRchr_unif_3__47__52' / 4, leash, notrace).
'CHRchr_unif_3__47__52__53'(['CHRchr_unif_3'(chr_unif(A, B, C), D, E, F)|G], H, chr_unif(I, B, C), J, K) ?-
	'CHRvar'(D),
	coca(try_double(K, chr_unif(I, B, C), F, chr_unif(A, B, C), chr_unif(L, M, N), chr_unif(O, M, N), keep_first, true, O = L, anonymous("14"))),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("14"))),
	'CHRchr_unif_3__47__52__53'(G, H, chr_unif(I, B, C), J, K),
	A = I.
'CHRchr_unif_3__47__52__53'([A|B], C, D, E, F) :-
	'CHRchr_unif_3__47__52__53'(B, C, D, E, F).
'CHRchr_unif_3__47__52__53'([], A, B, C, D) :-
	'CHRchr_unif_3__47__52__54'(B, A, C, D).
:- set_flag('CHRchr_unif_3__47__52__53' / 5, leash, notrace).
'CHRchr_unif_3__47__52__54'(chr_unif(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRchr_unif_3__47__52__54__55'(G, D, chr_unif(A, B, C), E, F).
'CHRchr_unif_3__47__52__54'(chr_unif(A, B, C), D, E, F) :-
	'CHRchr_unif_3__47__52__54__56'(chr_unif(A, B, C), D, E, F).
:- set_flag('CHRchr_unif_3__47__52__54' / 4, leash, notrace).
'CHRchr_unif_3__47__52__54__55'(['CHRchr_functor_3'(chr_functor(A, B, C), D, E, F)|G], H, chr_unif(A, I, J), K, L) ?-
	'CHRvar'(D),
	coca(try_double(L, chr_unif(A, I, J), F, chr_functor(A, B, C), chr_unif(M, N, O), chr_functor(M, P, Q), keep_first, nonvar(Q) ; islist(O), (P = N, length(O, Q)), anonymous("15"))),
	no_delayed_goals(nonvar(C) ; islist(J)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("15"))),
	'CHRchr_unif_3__47__52__54__55'(G, H, chr_unif(A, I, J), K, L),
	B = I,
	length(J, C).
'CHRchr_unif_3__47__52__54__55'([A|B], C, D, E, F) :-
	'CHRchr_unif_3__47__52__54__55'(B, C, D, E, F).
'CHRchr_unif_3__47__52__54__55'([], A, B, C, D) :-
	'CHRchr_unif_3__47__52__54__56'(B, A, C, D).
:- set_flag('CHRchr_unif_3__47__52__54__55' / 5, leash, notrace).
'CHRchr_unif_3__47__52__54__56'(chr_unif(A, B, C), D, E, F) ?-
	'CHRvar'(D),
	!,
	'CHRget_delayed_goals'(A, G),
	'CHRchr_unif_3__47__52__54__56__57'(G, D, chr_unif(A, B, C), E, F).
'CHRchr_unif_3__47__52__54__56'(chr_unif(A, B, C), D, E, F) :-
	'CHRchr_unif_3__47__52__54__56__58'(chr_unif(A, B, C), D, E, F).
:- set_flag('CHRchr_unif_3__47__52__54__56' / 4, leash, notrace).
'CHRchr_unif_3__47__52__54__56__57'(['CHRchr_arg_3'(chr_arg(A, B, C), D, E, F)|G], H, chr_unif(B, I, J), K, L) ?-
	'CHRvar'(D),
	coca(try_double(L, chr_unif(B, I, J), F, chr_arg(A, B, C), chr_unif(M, N, O), chr_arg(P, M, Q), keep_first, nonvar(P), nth_member(P, O, Q), anonymous("16"))),
	no_delayed_goals(nonvar(A)),
	!,
	'CHRkill'(D),
	coca(fired_rule(anonymous("16"))),
	'CHRchr_unif_3__47__52__54__56__57'(G, H, chr_unif(B, I, J), K, L),
	nth_member(A, J, C).
'CHRchr_unif_3__47__52__54__56__57'([A|B], C, D, E, F) :-
	'CHRchr_unif_3__47__52__54__56__57'(B, C, D, E, F).
'CHRchr_unif_3__47__52__54__56__57'([], A, B, C, D) :-
	'CHRchr_unif_3__47__52__54__56__58'(B, A, C, D).
:- set_flag('CHRchr_unif_3__47__52__54__56__57' / 5, leash, notrace).
'CHRchr_unif_3__47__52__54__56__58'(chr_unif(A, B, C), D, E, F) :-
	(
	    'CHRvar'(D)
	->
	    'CHRdelay'([D, chr_unif(A, B, C)], 'CHRchr_unif_3'(chr_unif(A, B, C), D, E, F))
	;
	    true
	).
:- set_flag('CHRchr_unif_3__47__52__54__56__58' / 4, leash, notrace).
chr_var(A) :-
	'CHRgen_num'(B),
	coca(add_one_constraint(B, chr_var(A))),
	'CHRchr_var_1'(chr_var(A), C, D, B).



%%% Rules handling for chr_var / 1

'CHRchr_var_1'(chr_var(A), B, C, D) :-
	'CHRnonvar'(B),
	!.
'CHRchr_var_1'(chr_var(A), B, C, D) ?-
	coca(try_rule(D, chr_var(A), anonymous("17"), chr_var(E), replacement, nonvar(E), fail)),
	no_delayed_goals(nonvar(A)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("17"))),
	fail.
'CHRchr_var_1'(chr_var(A), B, C, D) ?-
	'CHRget_delayed_goals'(A, E),
	'CHRchr_var_1__60'(E, [A], [], F),
	coca(try_double(D, chr_var(A), F, chr_nonvar(A), chr_var(G), chr_nonvar(G), replacement, true, fail, anonymous("19"))),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("19"))),
	fail.
'CHRchr_var_1'(chr_var(A), B, C, D) ?-
	'CHRget_delayed_goals'(A, E),
	'CHRchr_var_1__61'(E, [A], [], F),
	coca(try_double(D, chr_var(A), F, chr_var(A), chr_var(G), chr_var(G), keep_second, true, true, anonymous("20"))),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("20"))).
'CHRchr_var_1'(chr_var(A), B, C, D) :-
	'CHRchr_var_1__59'(chr_var(A), B, C, D).
'CHRchr_var_1__60'(['CHRchr_nonvar_1'(chr_nonvar(A), B, C, D)|E], [A], [], F) ?-
	'CHRvar'(B),
	'CHRkill'(B),
	'CHR='([], []),
	'CHR='(D, F).
'CHRchr_var_1__60'([A|B], C, D, E) :-
	'CHRchr_var_1__60'(B, C, D, E).
:- set_flag('CHRchr_var_1__60' / 4, leash, notrace).
'CHRchr_var_1__61'(['CHRchr_var_1'(chr_var(A), B, C, D)|E], [A], [], F) ?-
	'CHRvar'(B),
	'CHR='([], []),
	'CHR='(D, F).
'CHRchr_var_1__61'([A|B], C, D, E) :-
	'CHRchr_var_1__61'(B, C, D, E).
:- set_flag('CHRchr_var_1__61' / 4, leash, notrace).
:- set_flag('CHRchr_var_1' / 4, leash, notrace).
:- current_macro('CHRchr_var_1' / 4, _18484, _18485, _18486) -> true ; define_macro('CHRchr_var_1' / 4, tr_chr / 2, [write]).
'CHRchr_var_1__59'(A, B, C, D) :-
	'CHRchr_var_1__62'(A, B, C, D).
:- set_flag('CHRchr_var_1__59' / 4, leash, notrace).
'CHRchr_var_1__62'(chr_var(A), B, C, D) ?-
	'CHRvar'(B),
	!,
	'CHRget_delayed_goals'(A, E),
	'CHRchr_var_1__62__63'(E, B, chr_var(A), C, D).
'CHRchr_var_1__62'(chr_var(A), B, C, D) :-
	'CHRchr_var_1__62__64'(chr_var(A), B, C, D).
:- set_flag('CHRchr_var_1__62' / 4, leash, notrace).
'CHRchr_var_1__62__63'(['CHRchr_var_1'(chr_var(A), B, C, D)|E], F, chr_var(A), G, H) ?-
	'CHRvar'(B),
	coca(try_double(H, chr_var(A), D, chr_var(A), chr_var(I), chr_var(I), keep_first, true, true, anonymous("20"))),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("20"))),
	'CHRchr_var_1__62__63'(E, F, chr_var(A), G, H).
'CHRchr_var_1__62__63'([A|B], C, D, E, F) :-
	'CHRchr_var_1__62__63'(B, C, D, E, F).
'CHRchr_var_1__62__63'([], A, B, C, D) :-
	'CHRchr_var_1__62__64'(B, A, C, D).
:- set_flag('CHRchr_var_1__62__63' / 5, leash, notrace).
'CHRchr_var_1__62__64'(chr_var(A), B, C, D) :-
	(
	    'CHRvar'(B)
	->
	    'CHRdelay'([B, chr_var(A)], 'CHRchr_var_1'(chr_var(A), B, C, D))
	;
	    true
	).
:- set_flag('CHRchr_var_1__62__64' / 4, leash, notrace).
chr_nonvar(A) :-
	'CHRgen_num'(B),
	coca(add_one_constraint(B, chr_nonvar(A))),
	'CHRchr_nonvar_1'(chr_nonvar(A), C, D, B).



%%% Rules handling for chr_nonvar / 1

'CHRchr_nonvar_1'(chr_nonvar(A), B, C, D) :-
	'CHRnonvar'(B),
	!.
'CHRchr_nonvar_1'(chr_nonvar(A), B, C, D) ?-
	coca(try_rule(D, chr_nonvar(A), anonymous("18"), chr_nonvar(E), replacement, nonvar(E), true)),
	no_delayed_goals(nonvar(A)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("18"))).
'CHRchr_nonvar_1'(chr_nonvar(A), B, C, D) ?-
	'CHRget_delayed_goals'(A, E),
	'CHRchr_nonvar_1__66'(E, [A], [], F),
	coca(try_double(D, chr_nonvar(A), F, chr_var(A), chr_nonvar(G), chr_var(G), replacement, true, fail, anonymous("19"))),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("19"))),
	fail.
'CHRchr_nonvar_1'(chr_nonvar(A), B, C, D) ?-
	'CHRget_delayed_goals'(A, E),
	'CHRchr_nonvar_1__67'(E, [A], [], F),
	coca(try_double(D, chr_nonvar(A), F, chr_nonvar(A), chr_nonvar(G), chr_nonvar(G), keep_second, true, true, anonymous("21"))),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("21"))).
'CHRchr_nonvar_1'(chr_nonvar(A), B, C, D) :-
	'CHRchr_nonvar_1__65'(chr_nonvar(A), B, C, D).
'CHRchr_nonvar_1__66'(['CHRchr_var_1'(chr_var(A), B, C, D)|E], [A], [], F) ?-
	'CHRvar'(B),
	'CHRkill'(B),
	'CHR='([], []),
	'CHR='(D, F).
'CHRchr_nonvar_1__66'([A|B], C, D, E) :-
	'CHRchr_nonvar_1__66'(B, C, D, E).
:- set_flag('CHRchr_nonvar_1__66' / 4, leash, notrace).
'CHRchr_nonvar_1__67'(['CHRchr_nonvar_1'(chr_nonvar(A), B, C, D)|E], [A], [], F) ?-
	'CHRvar'(B),
	'CHR='([], []),
	'CHR='(D, F).
'CHRchr_nonvar_1__67'([A|B], C, D, E) :-
	'CHRchr_nonvar_1__67'(B, C, D, E).
:- set_flag('CHRchr_nonvar_1__67' / 4, leash, notrace).
:- set_flag('CHRchr_nonvar_1' / 4, leash, notrace).
:- current_macro('CHRchr_nonvar_1' / 4, _20619, _20620, _20621) -> true ; define_macro('CHRchr_nonvar_1' / 4, tr_chr / 2, [write]).
'CHRchr_nonvar_1__65'(A, B, C, D) :-
	'CHRchr_nonvar_1__68'(A, B, C, D).
:- set_flag('CHRchr_nonvar_1__65' / 4, leash, notrace).
'CHRchr_nonvar_1__68'(chr_nonvar(A), B, C, D) ?-
	'CHRvar'(B),
	!,
	'CHRget_delayed_goals'(A, E),
	'CHRchr_nonvar_1__68__69'(E, B, chr_nonvar(A), C, D).
'CHRchr_nonvar_1__68'(chr_nonvar(A), B, C, D) :-
	'CHRchr_nonvar_1__68__70'(chr_nonvar(A), B, C, D).
:- set_flag('CHRchr_nonvar_1__68' / 4, leash, notrace).
'CHRchr_nonvar_1__68__69'(['CHRchr_nonvar_1'(chr_nonvar(A), B, C, D)|E], F, chr_nonvar(A), G, H) ?-
	'CHRvar'(B),
	coca(try_double(H, chr_nonvar(A), D, chr_nonvar(A), chr_nonvar(I), chr_nonvar(I), keep_first, true, true, anonymous("21"))),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("21"))),
	'CHRchr_nonvar_1__68__69'(E, F, chr_nonvar(A), G, H).
'CHRchr_nonvar_1__68__69'([A|B], C, D, E, F) :-
	'CHRchr_nonvar_1__68__69'(B, C, D, E, F).
'CHRchr_nonvar_1__68__69'([], A, B, C, D) :-
	'CHRchr_nonvar_1__68__70'(B, A, C, D).
:- set_flag('CHRchr_nonvar_1__68__69' / 5, leash, notrace).
'CHRchr_nonvar_1__68__70'(chr_nonvar(A), B, C, D) :-
	(
	    'CHRvar'(B)
	->
	    'CHRdelay'([B, chr_nonvar(A)], 'CHRchr_nonvar_1'(chr_nonvar(A), B, C, D))
	;
	    true
	).
:- set_flag('CHRchr_nonvar_1__68__70' / 4, leash, notrace).
diff(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, diff(A, B))),
	'CHRdiff_2'(diff(A, B), D, E, C).



%%% Rules handling for diff / 2

'CHRdiff_2'(diff(A, B), C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHRdiff_2'(diff(A, A), B, C, D) ?-
	coca(try_rule(D, diff(A, A), anonymous("22"), diff(E, E), replacement, true, fail)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("22"))),
	fail.
'CHRdiff_2'(diff(A, B), C, D, E) :-
	'CHRdiff_2__71'(diff(A, B), C, D, E).
:- set_flag('CHRdiff_2' / 4, leash, notrace).
:- current_macro('CHRdiff_2' / 4, _21923, _21924, _21925) -> true ; define_macro('CHRdiff_2' / 4, tr_chr / 2, [write]).
'CHRdiff_2__71'(A, B, C, D) :-
	'CHRdiff_2__72'(A, B, C, D).
:- set_flag('CHRdiff_2__71' / 4, leash, notrace).
'CHRdiff_2__72'(diff(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, diff(A, B)], 'CHRdiff_2'(diff(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRdiff_2__72' / 4, leash, notrace).
diff_list(A, B) :-
	'CHRgen_num'(C),
	coca(add_one_constraint(C, diff_list(A, B))),
	'CHRdiff_list_2'(diff_list(A, B), D, E, C).



%%% Rules handling for diff_list / 2

'CHRdiff_list_2'(diff_list(A, B), C, D, E) :-
	'CHRnonvar'(C),
	!.
'CHRdiff_list_2'(diff_list(A, []), B, C, D) ?-
	coca(try_rule(D, diff_list(A, []), anonymous("23"), diff_list(E, []), replacement, true, true)),
	!,
	'CHRkill'(B),
	coca(fired_rule(anonymous("23"))).
'CHRdiff_list_2'(diff_list(A, B), C, D, E) ?-
	coca(try_rule(E, diff_list(A, B), anonymous("24"), diff_list(F, G), replacement, (member(H, G), F == H), fail)),
	no_delayed_goals((member(I, B), A == I)),
	!,
	'CHRkill'(C),
	coca(fired_rule(anonymous("24"))),
	fail.
'CHRdiff_list_2'(diff_list(A, B), C, D, E) :-
	'CHRdiff_list_2__73'(diff_list(A, B), C, D, E).
:- set_flag('CHRdiff_list_2' / 4, leash, notrace).
:- current_macro('CHRdiff_list_2' / 4, _22818, _22819, _22820) -> true ; define_macro('CHRdiff_list_2' / 4, tr_chr / 2, [write]).
'CHRdiff_list_2__73'(A, B, C, D) :-
	'CHRdiff_list_2__74'(A, B, C, D).
:- set_flag('CHRdiff_list_2__73' / 4, leash, notrace).
'CHRdiff_list_2__74'(diff_list(A, B), C, D, E) :-
	(
	    'CHRvar'(C)
	->
	    'CHRdelay'([C, diff_list(A, B)], 'CHRdiff_list_2'(diff_list(A, B), C, D, E))
	;
	    true
	).
:- set_flag('CHRdiff_list_2__74' / 4, leash, notrace).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
