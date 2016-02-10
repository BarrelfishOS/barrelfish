
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
globalize(_g1068) :- delay(_g1068 - _g1054, true), _g1054 = fired.
path1(1, _g1274, _g1272, [_g1270], p - p, _g1290) :- check_pp(_g1274, _g1272, _g1270).
path1(1, _g1332, _g1330, [_g1328], p - i, _g1348) :- check_pi(_g1332, _g1330, _g1328).
path1(1, _g1388, _g1390, [_g1386], i - p, _g1406) :- check_ip(_g1390, _g1388, _g1386).
path1(1, _g1448, _g1446, [_g1444], i - i, _g1464) :- check_ii(_g1448, _g1446, _g1444).
ground(_g1502) :- not nonground(_g1502).



%%% Callables for path / 6

'CHRlabel_with'(path(_g4712, _g4714, _g4716, _g4718, _g4720, _g4722), Goal_g4864, Nb_g4866) ?-
	coca(try_clause(Nb_g4866, path(_g4712, _g4714, _g4716, _g4718, _g4720, _g4722), path(_g4822, _g4824, _g4826, _g4828, _g4830, _g4832), >(_g4822, 1))),
	no_delayed_goals(>(_g4712, 1)),
	coca(clause_fired(Nb_g4866)),
	'CHR='(Goal_g4864, clause_path(_g4712, _g4714, _g4716, _g4718, _g4720, _g4722)).

arc(_g5084, _g5086, _g5088, _g5090) :-
	'CHRgen_num'(Nb_g5078),
	coca(add_one_constraint(Nb_g5078, arc(_g5084, _g5086, _g5088, _g5090))),
	'CHRarc_4'(arc(_g5084, _g5086, _g5088, _g5090), _KF_g5104, _PA_g5110, Nb_g5078).




%%% Rules handling for arc / 4

'CHRarc_4'(arc(_g5238, _g5240, _g5242, _g5244), KF_g5052, _PA_g5262, _Nb_g5268) :-
	'CHRnonvar'(KF_g5052),
	!.

'CHRarc_4'(arc(_g5428, _g5430, _g5432, _g5434), _KF_g5436, _PA_g5522, Nb_g5528) ?-
	coca(try_rule(Nb_g5528, arc(_g5428, _g5430, _g5432, _g5434), add_path, arc(_g5590, _g5592, _g5594, _g5596), replacement, ','(ground(_g5594), ','(ground(_g5596), length(_g5594, N_g5630))), ','(globalize(-(_g5590, _g5592)), path(N_g5630, _g5590, _g5592, _g5594, _g5596, 1)))),
	no_delayed_goals(','(ground(_g5432), ','(ground(_g5434), length(_g5432, N_g5464)))),
	!,
	'CHRkill'(_KF_g5436),
	coca(fired_rule(add_path)),
	globalize(-(_g5428, _g5430)),
	path(N_g5464, _g5428, _g5430, _g5432, _g5434, 1).

'CHRarc_4'(arc(_g5912, _g5914, _g5916, _g5918), KF_g6000, PA_g5998, Nb_g5996) :-
	'CHRarc_4__0'(arc(_g5912, _g5914, _g5916, _g5918), KF_g6000, PA_g5998, Nb_g5996).

:-
	set_flag(/('CHRarc_4', 4), leash, notrace).

:-
	(;(->(current_macro(/('CHRarc_4', 4), _g6158, _g6160, _g6162), true), define_macro(/('CHRarc_4', 4), /(tr_chr, 2), [write]))).

'CHRarc_4__0'(Constraint_g6304, KF_g6310, PA_g6316, Nb_g6322) :-
	'CHRarc_4__1'(Constraint_g6304, KF_g6310, PA_g6316, Nb_g6322).

:-
	set_flag(/('CHRarc_4__0', 4), leash, notrace).

'CHRarc_4__1'(arc(_g6436, _g6438, _g6440, _g6442), KF_g6538, PA_g6544, Nb_g6550) :-
	(;(->('CHRvar'(KF_g6538), 'CHRdelay'([KF_g6538, arc(_g6436, _g6438, _g6440, _g6442)], 'CHRarc_4'(arc(_g6436, _g6438, _g6440, _g6442), KF_g6538, PA_g6544, Nb_g6550))), true)).

:-
	set_flag(/('CHRarc_4__1', 4), leash, notrace).




%%% Prolog clauses for path / 6

clause_path(_g6760, _g6762, _g6764, _g6766, _g6768, _g6770) :-
	member(R_g6784, _g6766),
	path(1, _g6762, _g6764, [R_g6784], _g6768, _g6770).

:-
	(;(->(current_macro(/(clause_path, 6), _g6880, _g6882, _g6884), true), define_macro(/(clause_path, 6), /(tr_chr, 2), [write]))).

path(_g6970, _g6972, _g6974, _g6976, _g6978, _g6980) :-
	'CHRgen_num'(Nb_g6964),
	coca(add_one_constraint(Nb_g6964, path(_g6970, _g6972, _g6974, _g6976, _g6978, _g6980))),
	'CHRpath_6'(path(_g6970, _g6972, _g6974, _g6976, _g6978, _g6980), _KF_g6994, _PA_g7000, Nb_g6964).




%%% Rules handling for path / 6

'CHRpath_6'(path(_g7128, _g7130, _g7132, _g7134, _g7136, _g7138), KF_g6938, _PA_g7156, _Nb_g7162) :-
	'CHRnonvar'(KF_g6938),
	!.

'CHRpath_6'(path(_g7326, _g7328, _g7330, _g7332, _g7334, _g7336), _KF_g7338, _PA_g7902, Nb_g7908) ?-
	coca(try_rule(Nb_g7908, path(_g7326, _g7328, _g7330, _g7332, _g7334, _g7336), ground, path(_g7970, _g7972, _g7974, _g7976, _g7978, _g7980), replacement, ground(-(-(-(_g7972, _g7974), _g7976), _g7978)), path1(_g7970, _g7972, _g7974, _g7976, _g7978, _g7980))),
	no_delayed_goals(ground(-(-(-(_g7328, _g7330), _g7332), _g7334))),
	!,
	'CHRkill'(_KF_g7338),
	coca(fired_rule(ground)),
	path1(_g7326, _g7328, _g7330, _g7332, _g7334, _g7336).

'CHRpath_6'(path(_g7400, _g7402, _g7404, _g7406, _g7408, _g7410), _KF_g7412, _PA_g8122, Nb_g8128) ?-
	coca(try_rule(Nb_g8128, path(_g7400, _g7402, _g7404, _g7406, _g7408, _g7410), empty, path(_g8190, _g8192, _g8194, _g8196, _g8198, _g8200), replacement, empty(_g8190, _g8196, _g8198), fail)),
	no_delayed_goals(empty(_g7400, _g7406, _g7408)),
	!,
	'CHRkill'(_KF_g7412),
	coca(fired_rule(empty)),
	fail.

'CHRpath_6'(path(_g7446, _g7448, _g7450, _g7452, _g7454, _g7456), _KF_g7458, _PA_g8314, Nb_g8320) ?-
	coca(try_rule(Nb_g8320, path(_g7446, _g7448, _g7450, _g7452, _g7454, _g7456), universal, path(_g8382, _g8384, _g8386, _g8388, _g8390, _g8392), replacement, universal(_g8382, _g8388, _g8390), true)),
	no_delayed_goals(universal(_g7446, _g7452, _g7454)),
	!,
	'CHRkill'(_KF_g7458),
	coca(fired_rule(universal)).

'CHRpath_6'(path(_g7492, _g7494, _g7494, _g7498, _g7500, _g7502), _KF_g7504, _PA_g8506, Nb_g8512) ?-
	coca(try_rule(Nb_g8512, path(_g7492, _g7494, _g7494, _g7498, _g7500, _g7502), equality, path(_g8574, _g8576, _g8576, _g8580, _g8582, _g8584), replacement, true, equality(_g8580, _g8582))),
	!,
	'CHRkill'(_KF_g7504),
	coca(fired_rule(equality)),
	equality(_g7498, _g7500).

'CHRpath_6'(path(1, _g7538, _g7540, _g7542, _g7544, _g7546), _KF_g7548, _PA_g8696, Nb_g8702) ?-
	coca(try_rule(Nb_g8702, path(1, _g7538, _g7540, _g7542, _g7544, _g7546), unify, path(1, _g8766, _g8768, _g8770, _g8772, _g8774), replacement, ','(unique(_g8770), equality(_g8770, _g8772)), =(_g8766, _g8768))),
	no_delayed_goals(','(unique(_g7542), equality(_g7542, _g7544))),
	!,
	'CHRkill'(_KF_g7548),
	coca(fired_rule(unify)),
	=(_g7538, _g7540).

'CHRpath_6'(path(1, _g7598, _g7600, _g7602, -(p, p), _g7606), _KF_g7614, _PA_g8902, Nb_g8908) ?-
	coca(try_rule(Nb_g8908, path(1, _g7598, _g7600, _g7602, -(p, p), _g7606), findom_unique, path(1, _g8972, _g8974, _g8976, -(p, p), _g8980), replacement, ','(number(_g8972), unique(_g8976)), bind_value(_g8972, _g8974, _g8976))),
	no_delayed_goals(','(number(_g7598), unique(_g7602))),
	!,
	'CHRkill'(_KF_g7614),
	coca(fired_rule(findom_unique)),
	bind_value(_g7598, _g7600, _g7602).

'CHRpath_6'(path(_g7662, _g7664, _g7666, _g7668, -(p, p), _g7672), _KF_g7680, _PA_g9108, Nb_g9114) ?-
	coca(try_rule(Nb_g9114, path(_g7662, _g7664, _g7666, _g7668, -(p, p), _g7672), findom_x, path(_g9176, _g9178, _g9180, _g9182, -(p, p), _g9186), replacement, ','(number(_g9178), ','(=\=(_g9178, 0), shift_interval(_g9178, _g9182, L1_g9224))), path(_g9176, 0, _g9180, L1_g9224, -(p, p), _g9186))),
	no_delayed_goals(','(number(_g7664), ','(=\=(_g7664, 0), shift_interval(_g7664, _g7668, L1_g7712)))),
	!,
	'CHRkill'(_KF_g7680),
	coca(fired_rule(findom_x)),
	path(_g7662, 0, _g7666, L1_g7712, -(p, p), _g7672).

'CHRpath_6'(path(_g7758, _g7760, _g7762, _g7764, -(p, p), _g7768), _KF_g7776, _PA_g9338, Nb_g9344) ?-
	coca(try_rule(Nb_g9344, path(_g7758, _g7760, _g7762, _g7764, -(p, p), _g7768), findom_y, path(_g9406, _g9408, _g9410, _g9412, -(p, p), _g9416), replacement, ','(number(_g9410), ','(equality([Eq_g9450], -(p, p)), ','(transl(_g9412, L2_g9468, [Eq_g9450], -(-(p, p), p)), shift_interval(_g9410, L2_g9468, L1_g9482)))), path(_g9406, 0, _g9408, L1_g9482, -(p, p), _g9416))),
	no_delayed_goals(','(number(_g7762), ','(equality([Eq_g7804], -(p, p)), ','(transl(_g7764, L2_g7828, [Eq_g7804], -(-(p, p), p)), shift_interval(_g7762, L2_g7828, L1_g7854))))),
	!,
	'CHRkill'(_KF_g7776),
	coca(fired_rule(findom_y)),
	path(_g7758, 0, _g7760, L1_g7854, -(p, p), _g7768).

'CHRpath_6'(path(_g10541, _g10543, _g10545, _g10547, _g10549, _g10551), KF_g10633, PA_g10631, Nb_g10629) :-
	'CHRpath_6__2'(path(_g10541, _g10543, _g10545, _g10547, _g10549, _g10551), KF_g10633, PA_g10631, Nb_g10629).

:-
	set_flag(/('CHRpath_6', 4), leash, notrace).

:-
	(;(->(current_macro(/('CHRpath_6', 4), _g10791, _g10793, _g10795), true), define_macro(/('CHRpath_6', 4), /(tr_chr, 2), [write]))).

'CHRpath_6__2'(Constraint_g10942, KF_g10948, PA_g10954, Nb_g10960) :-
	'CHRpath_6__3'(Constraint_g10942, KF_g10948, PA_g10954, Nb_g10960).

:-
	set_flag(/('CHRpath_6__2', 4), leash, notrace).

'CHRpath_6__3'(path(N1_g11208, X_g11210, Y_g11212, L1_g11214, -(U_g11222, V_g11224), I_g11226), KF1_g11228, PA_g13700, Nb1_g13706) ?-
	'CHRvar'(KF1_g11228),
	!,
	'CHRget_delayed_goals'(V_g11224, Cs_g13756),
	'CHRpath_6__3__4'(Cs_g13756, KF1_g11228, path(N1_g11208, X_g11210, Y_g11212, L1_g11214, -(U_g11222, V_g11224), I_g11226), PA_g13700, Nb1_g13706).

'CHRpath_6__3'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF1_g11228, PA_g13700, Nb1_g13706) :-
	'CHRpath_6__3__5'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF1_g11228, PA_g13700, Nb1_g13706).

:-
	set_flag(/('CHRpath_6__3', 4), leash, notrace).

'CHRpath_6__3__4'(['CHRpath_6'(path(N2_g11250, X_g11210, Y_g11212, L2_g11252, -(U_g11222, V_g11224), J_g11260), KF2_g11262, PA2_g13758, Nb2_g13748)|Cs_g13756], KF1_g11228, path(N1_g11208, X_g11210, Y_g11212, L1_g11214, -(U_g11222, V_g11224), I_g11226), PA1_g13754, Nb1_g13706) ?-
	'CHRvar'(KF2_g11262),
	'CHRcheck_and_mark_applied'('12'(intersect_xy_xy), KF1_g11228, KF2_g11262, PA1_g13754, PA2_g13758),
	coca(try_double(Nb1_g13706, path(N1_g11208, X_g11210, Y_g11212, L1_g11214, -(U_g11222, V_g11224), I_g11226), Nb2_g13748, path(N2_g11250, X_g11210, Y_g11212, L2_g11252, -(U_g11222, V_g11224), J_g11260), path(N1_g14220, X_g14222, Y_g14224, L1_g14226, -(U_g14234, V_g14236), I_g14238), path(N2_g14254, X_g14222, Y_g14224, L2_g14256, -(U_g14234, V_g14236), J_g14264), augmentation, ','(','(intersection(L1_g14226, L2_g14256, L3_g14294, -(U_g14234, V_g14236)), ','(length(L3_g14294, N3_g14314), is(K_g14322, min(I_g14238, J_g14264)))), 'CHRkeep_heads_checking'(path(N1_g14220, X_g14222, Y_g14224, L1_g14226, -(U_g14234, V_g14236), I_g14238), KF1_g14364, path(N2_g14254, X_g14222, Y_g14224, L2_g14256, -(U_g14234, V_g14236), J_g14264), KF2_g14386, path(N3_g14314, X_g14222, Y_g14224, L3_g14294, -(U_g14234, V_g14236), K_g14322), CallG_g14408)), ;(->('CHRhead_not_kept'(CallG_g14408), path(N3_g14314, X_g14222, Y_g14224, L3_g14294, -(U_g14234, V_g14236), K_g14322)), true), intersect_xy_xy)),
	no_delayed_goals(','(','(intersection(L1_g11214, L2_g11252, L3_g11286, -(U_g11222, V_g11224)), ','(length(L3_g11286, N3_g11306), is(K_g11314, min(I_g11226, J_g11260)))), 'CHRkeep_heads_checking'(path(N1_g11208, X_g11210, Y_g11212, L1_g11214, -(U_g11222, V_g11224), I_g11226), KF1_g11228, path(N2_g11250, X_g11210, Y_g11212, L2_g11252, -(U_g11222, V_g11224), J_g11260), KF2_g11262, path(N3_g11306, X_g11210, Y_g11212, L3_g11286, -(U_g11222, V_g11224), K_g11314), CallG_g11396))),
	!,
	coca(fired_rule(intersect_xy_xy)),
	'CHRpath_6__3__4'(Cs_g13756, KF1_g11228, path(N1_g11208, X_g11210, Y_g11212, L1_g11214, -(U_g11222, V_g11224), I_g11226), PA1_g13754, Nb1_g13706),
	(;(->('CHRhead_not_kept'(CallG_g11396), path(N3_g11306, X_g11210, Y_g11212, L3_g11286, -(U_g11222, V_g11224), K_g11314)), true)).

'CHRpath_6__3__4'([_g14876|Cs_g14884], KF1_g14886, First_Head_g14892, PA_g14898, Nb1_g14904) :-
	'CHRpath_6__3__4'(Cs_g14884, KF1_g14886, First_Head_g14892, PA_g14898, Nb1_g14904).

'CHRpath_6__3__4'([], KF1_g14886, Constraint_g14968, PA_g14898, Nb1_g14904) :-
	'CHRpath_6__3__5'(Constraint_g14968, KF1_g14886, PA_g14898, Nb1_g14904).

:-
	set_flag(/('CHRpath_6__3__4', 5), leash, notrace).

'CHRpath_6__3__5'(path(N2_g11474, X_g11476, Y_g11478, L2_g11480, -(U_g11488, V_g11490), J_g11492), KF2_g11494, PA_g15152, Nb1_g15158) ?-
	'CHRvar'(KF2_g11494),
	!,
	'CHRget_delayed_goals'(V_g11490, Cs_g15208),
	'CHRpath_6__3__5__6'(Cs_g15208, KF2_g11494, path(N2_g11474, X_g11476, Y_g11478, L2_g11480, -(U_g11488, V_g11490), J_g11492), PA_g15152, Nb1_g15158).

'CHRpath_6__3__5'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF2_g11494, PA_g15152, Nb1_g15158) :-
	'CHRpath_6__3__5__7'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF2_g11494, PA_g15152, Nb1_g15158).

:-
	set_flag(/('CHRpath_6__3__5', 4), leash, notrace).

'CHRpath_6__3__5__6'(['CHRpath_6'(path(N1_g11516, X_g11476, Y_g11478, L1_g11518, -(U_g11488, V_g11490), I_g11526), KF1_g11528, PA2_g15210, Nb2_g15200)|Cs_g15208], KF2_g11494, path(N2_g11474, X_g11476, Y_g11478, L2_g11480, -(U_g11488, V_g11490), J_g11492), PA1_g15206, Nb1_g15158) ?-
	'CHRvar'(KF1_g11528),
	'CHRcheck_and_mark_applied'('21'(intersect_xy_xy), KF2_g11494, KF1_g11528, PA1_g15206, PA2_g15210),
	coca(try_double(Nb1_g15158, path(N2_g11474, X_g11476, Y_g11478, L2_g11480, -(U_g11488, V_g11490), J_g11492), Nb2_g15200, path(N1_g11516, X_g11476, Y_g11478, L1_g11518, -(U_g11488, V_g11490), I_g11526), path(N2_g15674, X_g15676, Y_g15678, L2_g15680, -(U_g15688, V_g15690), J_g15692), path(N1_g15708, X_g15676, Y_g15678, L1_g15710, -(U_g15688, V_g15690), I_g15718), augmentation, ','(','(intersection(L1_g15710, L2_g15680, L3_g15748, -(U_g15688, V_g15690)), ','(length(L3_g15748, N3_g15768), is(K_g15776, min(I_g15718, J_g15692)))), 'CHRkeep_heads_checking'(path(N1_g15708, X_g15676, Y_g15678, L1_g15710, -(U_g15688, V_g15690), I_g15718), KF1_g15818, path(N2_g15674, X_g15676, Y_g15678, L2_g15680, -(U_g15688, V_g15690), J_g15692), KF2_g15840, path(N3_g15768, X_g15676, Y_g15678, L3_g15748, -(U_g15688, V_g15690), K_g15776), CallG_g15862)), ;(->('CHRhead_not_kept'(CallG_g15862), path(N3_g15768, X_g15676, Y_g15678, L3_g15748, -(U_g15688, V_g15690), K_g15776)), true), intersect_xy_xy)),
	no_delayed_goals(','(','(intersection(L1_g11518, L2_g11480, L3_g11552, -(U_g11488, V_g11490)), ','(length(L3_g11552, N3_g11572), is(K_g11580, min(I_g11526, J_g11492)))), 'CHRkeep_heads_checking'(path(N1_g11516, X_g11476, Y_g11478, L1_g11518, -(U_g11488, V_g11490), I_g11526), KF1_g11528, path(N2_g11474, X_g11476, Y_g11478, L2_g11480, -(U_g11488, V_g11490), J_g11492), KF2_g11494, path(N3_g11572, X_g11476, Y_g11478, L3_g11552, -(U_g11488, V_g11490), K_g11580), CallG_g11662))),
	!,
	coca(fired_rule(intersect_xy_xy)),
	'CHRpath_6__3__5__6'(Cs_g15208, KF2_g11494, path(N2_g11474, X_g11476, Y_g11478, L2_g11480, -(U_g11488, V_g11490), J_g11492), PA1_g15206, Nb1_g15158),
	(;(->('CHRhead_not_kept'(CallG_g11662), path(N3_g11572, X_g11476, Y_g11478, L3_g11552, -(U_g11488, V_g11490), K_g11580)), true)).

'CHRpath_6__3__5__6'([_g16330|Cs_g16338], KF1_g16340, First_Head_g16346, PA_g16352, Nb1_g16358) :-
	'CHRpath_6__3__5__6'(Cs_g16338, KF1_g16340, First_Head_g16346, PA_g16352, Nb1_g16358).

'CHRpath_6__3__5__6'([], KF1_g16340, Constraint_g16422, PA_g16352, Nb1_g16358) :-
	'CHRpath_6__3__5__7'(Constraint_g16422, KF1_g16340, PA_g16352, Nb1_g16358).

:-
	set_flag(/('CHRpath_6__3__5__6', 5), leash, notrace).

'CHRpath_6__3__5__7'(path(N1_g11740, Y_g11742, X_g11744, L1_g11746, -(U_g11754, V_g11756), I_g11758), KF1_g11760, PA_g16606, Nb1_g16612) ?-
	'CHRvar'(KF1_g11760),
	!,
	'CHRget_delayed_goals'(V_g11756, Cs_g16662),
	'CHRpath_6__3__5__7__8'(Cs_g16662, KF1_g11760, path(N1_g11740, Y_g11742, X_g11744, L1_g11746, -(U_g11754, V_g11756), I_g11758), PA_g16606, Nb1_g16612).

'CHRpath_6__3__5__7'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF1_g11760, PA_g16606, Nb1_g16612) :-
	'CHRpath_6__3__5__7__9'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF1_g11760, PA_g16606, Nb1_g16612).

:-
	set_flag(/('CHRpath_6__3__5__7', 4), leash, notrace).

'CHRpath_6__3__5__7__8'(['CHRpath_6'(path(N2_g11782, X_g11744, Y_g11742, L_g11784, -(V_g11756, U_g11754), J_g11792), KF2_g11794, PA2_g16664, Nb2_g16654)|Cs_g16662], KF1_g11760, path(N1_g11740, Y_g11742, X_g11744, L1_g11746, -(U_g11754, V_g11756), I_g11758), PA1_g16660, Nb1_g16612) ?-
	'CHRvar'(KF2_g11794),
	'CHRcheck_and_mark_applied'('12'(intersect_yx_xy), KF1_g11760, KF2_g11794, PA1_g16660, PA2_g16664),
	coca(try_double(Nb1_g16612, path(N1_g11740, Y_g11742, X_g11744, L1_g11746, -(U_g11754, V_g11756), I_g11758), Nb2_g16654, path(N2_g11782, X_g11744, Y_g11742, L_g11784, -(V_g11756, U_g11754), J_g11792), path(N1_g17130, Y_g17132, X_g17134, L1_g17136, -(U_g17144, V_g17146), I_g17148), path(N2_g17164, X_g17134, Y_g17132, L_g17166, -(V_g17146, U_g17144), J_g17174), augmentation, ','(','(equality([Eq_g17204], -(V_g17146, V_g17146)), ','(transl(L_g17166, L2_g17228, [Eq_g17204], -(-(V_g17146, U_g17144), V_g17146)), ','(intersection(L1_g17136, L2_g17228, L3_g17262, -(U_g17144, V_g17146)), ','(length(L3_g17262, N3_g17282), is(K_g17290, min(I_g17148, J_g17174)))))), 'CHRkeep_heads_checking'(path(N1_g17130, Y_g17132, X_g17134, L1_g17136, -(U_g17144, V_g17146), I_g17148), KF1_g17332, path(N2_g17164, X_g17134, Y_g17132, L_g17166, -(V_g17146, U_g17144), J_g17174), KF2_g17354, path(N3_g17282, Y_g17132, X_g17134, L3_g17262, -(U_g17144, V_g17146), K_g17290), CallG_g17376)), ;(->('CHRhead_not_kept'(CallG_g17376), path(N3_g17282, Y_g17132, X_g17134, L3_g17262, -(U_g17144, V_g17146), K_g17290)), true), intersect_yx_xy)),
	no_delayed_goals(','(','(equality([Eq_g11818], -(V_g11756, V_g11756)), ','(transl(L_g11784, L2_g11842, [Eq_g11818], -(-(V_g11756, U_g11754), V_g11756)), ','(intersection(L1_g11746, L2_g11842, L3_g11876, -(U_g11754, V_g11756)), ','(length(L3_g11876, N3_g11896), is(K_g11904, min(I_g11758, J_g11792)))))), 'CHRkeep_heads_checking'(path(N1_g11740, Y_g11742, X_g11744, L1_g11746, -(U_g11754, V_g11756), I_g11758), KF1_g11760, path(N2_g11782, X_g11744, Y_g11742, L_g11784, -(V_g11756, U_g11754), J_g11792), KF2_g11794, path(N3_g11896, Y_g11742, X_g11744, L3_g11876, -(U_g11754, V_g11756), K_g11904), CallG_g11986))),
	!,
	coca(fired_rule(intersect_yx_xy)),
	'CHRpath_6__3__5__7__8'(Cs_g16662, KF1_g11760, path(N1_g11740, Y_g11742, X_g11744, L1_g11746, -(U_g11754, V_g11756), I_g11758), PA1_g16660, Nb1_g16612),
	(;(->('CHRhead_not_kept'(CallG_g11986), path(N3_g11896, Y_g11742, X_g11744, L3_g11876, -(U_g11754, V_g11756), K_g11904)), true)).

'CHRpath_6__3__5__7__8'([_g17844|Cs_g17852], KF1_g17854, First_Head_g17860, PA_g17866, Nb1_g17872) :-
	'CHRpath_6__3__5__7__8'(Cs_g17852, KF1_g17854, First_Head_g17860, PA_g17866, Nb1_g17872).

'CHRpath_6__3__5__7__8'([], KF1_g17854, Constraint_g17936, PA_g17866, Nb1_g17872) :-
	'CHRpath_6__3__5__7__9'(Constraint_g17936, KF1_g17854, PA_g17866, Nb1_g17872).

:-
	set_flag(/('CHRpath_6__3__5__7__8', 5), leash, notrace).

'CHRpath_6__3__5__7__9'(path(N2_g12064, X_g12066, Y_g12068, L_g12070, -(V_g12078, U_g12080), J_g12082), KF2_g12084, PA_g18120, Nb1_g18126) ?-
	'CHRvar'(KF2_g12084),
	!,
	'CHRget_delayed_goals'(U_g12080, Cs_g18176),
	'CHRpath_6__3__5__7__9__10'(Cs_g18176, KF2_g12084, path(N2_g12064, X_g12066, Y_g12068, L_g12070, -(V_g12078, U_g12080), J_g12082), PA_g18120, Nb1_g18126).

'CHRpath_6__3__5__7__9'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF2_g12084, PA_g18120, Nb1_g18126) :-
	'CHRpath_6__3__5__7__9__11'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF2_g12084, PA_g18120, Nb1_g18126).

:-
	set_flag(/('CHRpath_6__3__5__7__9', 4), leash, notrace).

'CHRpath_6__3__5__7__9__10'(['CHRpath_6'(path(N1_g12106, Y_g12068, X_g12066, L1_g12108, -(U_g12080, V_g12078), I_g12116), KF1_g12118, PA2_g18178, Nb2_g18168)|Cs_g18176], KF2_g12084, path(N2_g12064, X_g12066, Y_g12068, L_g12070, -(V_g12078, U_g12080), J_g12082), PA1_g18174, Nb1_g18126) ?-
	'CHRvar'(KF1_g12118),
	'CHRcheck_and_mark_applied'('21'(intersect_yx_xy), KF2_g12084, KF1_g12118, PA1_g18174, PA2_g18178),
	coca(try_double(Nb1_g18126, path(N2_g12064, X_g12066, Y_g12068, L_g12070, -(V_g12078, U_g12080), J_g12082), Nb2_g18168, path(N1_g12106, Y_g12068, X_g12066, L1_g12108, -(U_g12080, V_g12078), I_g12116), path(N2_g18646, X_g18648, Y_g18650, L_g18652, -(V_g18660, U_g18662), J_g18664), path(N1_g18680, Y_g18650, X_g18648, L1_g18682, -(U_g18662, V_g18660), I_g18690), augmentation, ','(','(equality([Eq_g18720], -(V_g18660, V_g18660)), ','(transl(L_g18652, L2_g18744, [Eq_g18720], -(-(V_g18660, U_g18662), V_g18660)), ','(intersection(L1_g18682, L2_g18744, L3_g18778, -(U_g18662, V_g18660)), ','(length(L3_g18778, N3_g18798), is(K_g18806, min(I_g18690, J_g18664)))))), 'CHRkeep_heads_checking'(path(N1_g18680, Y_g18650, X_g18648, L1_g18682, -(U_g18662, V_g18660), I_g18690), KF1_g18848, path(N2_g18646, X_g18648, Y_g18650, L_g18652, -(V_g18660, U_g18662), J_g18664), KF2_g18870, path(N3_g18798, Y_g18650, X_g18648, L3_g18778, -(U_g18662, V_g18660), K_g18806), CallG_g18892)), ;(->('CHRhead_not_kept'(CallG_g18892), path(N3_g18798, Y_g18650, X_g18648, L3_g18778, -(U_g18662, V_g18660), K_g18806)), true), intersect_yx_xy)),
	no_delayed_goals(','(','(equality([Eq_g12142], -(V_g12078, V_g12078)), ','(transl(L_g12070, L2_g12166, [Eq_g12142], -(-(V_g12078, U_g12080), V_g12078)), ','(intersection(L1_g12108, L2_g12166, L3_g12200, -(U_g12080, V_g12078)), ','(length(L3_g12200, N3_g12220), is(K_g12228, min(I_g12116, J_g12082)))))), 'CHRkeep_heads_checking'(path(N1_g12106, Y_g12068, X_g12066, L1_g12108, -(U_g12080, V_g12078), I_g12116), KF1_g12118, path(N2_g12064, X_g12066, Y_g12068, L_g12070, -(V_g12078, U_g12080), J_g12082), KF2_g12084, path(N3_g12220, Y_g12068, X_g12066, L3_g12200, -(U_g12080, V_g12078), K_g12228), CallG_g12310))),
	!,
	coca(fired_rule(intersect_yx_xy)),
	'CHRpath_6__3__5__7__9__10'(Cs_g18176, KF2_g12084, path(N2_g12064, X_g12066, Y_g12068, L_g12070, -(V_g12078, U_g12080), J_g12082), PA1_g18174, Nb1_g18126),
	(;(->('CHRhead_not_kept'(CallG_g12310), path(N3_g12220, Y_g12068, X_g12066, L3_g12200, -(U_g12080, V_g12078), K_g12228)), true)).

'CHRpath_6__3__5__7__9__10'([_g19360|Cs_g19368], KF1_g19370, First_Head_g19376, PA_g19382, Nb1_g19388) :-
	'CHRpath_6__3__5__7__9__10'(Cs_g19368, KF1_g19370, First_Head_g19376, PA_g19382, Nb1_g19388).

'CHRpath_6__3__5__7__9__10'([], KF1_g19370, Constraint_g19452, PA_g19382, Nb1_g19388) :-
	'CHRpath_6__3__5__7__9__11'(Constraint_g19452, KF1_g19370, PA_g19382, Nb1_g19388).

:-
	set_flag(/('CHRpath_6__3__5__7__9__10', 5), leash, notrace).

'CHRpath_6__3__5__7__9__11'(path(N1_g12388, X_g12390, Y_g12392, L1_g12394, -(U_g12402, V_g12404), I_g12406), KF1_g12408, PA_g19636, Nb1_g19642) ?-
	'CHRvar'(KF1_g12408),
	!,
	'CHRget_delayed_goals'(V_g12404, Cs_g19692),
	'CHRpath_6__3__5__7__9__11__12'(Cs_g19692, KF1_g12408, path(N1_g12388, X_g12390, Y_g12392, L1_g12394, -(U_g12402, V_g12404), I_g12406), PA_g19636, Nb1_g19642).

'CHRpath_6__3__5__7__9__11'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF1_g12408, PA_g19636, Nb1_g19642) :-
	'CHRpath_6__3__5__7__9__11__13'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF1_g12408, PA_g19636, Nb1_g19642).

:-
	set_flag(/('CHRpath_6__3__5__7__9__11', 4), leash, notrace).

'CHRpath_6__3__5__7__9__11__12'(['CHRpath_6'(path(N2_g12430, Y_g12392, Z_g12432, L2_g12434, -(V_g12404, W_g12442), J_g12444), KF2_g12446, PA2_g19694, Nb2_g19684)|Cs_g19692], KF1_g12408, path(N1_g12388, X_g12390, Y_g12392, L1_g12394, -(U_g12402, V_g12404), I_g12406), PA1_g19690, Nb1_g19642) ?-
	'CHRvar'(KF2_g12446),
	'CHRcheck_and_mark_applied'('12'(propagate_xy_yz), KF1_g12408, KF2_g12446, PA1_g19690, PA2_g19694),
	coca(try_double(Nb1_g19642, path(N1_g12388, X_g12390, Y_g12392, L1_g12394, -(U_g12402, V_g12404), I_g12406), Nb2_g19684, path(N2_g12430, Y_g12392, Z_g12432, L2_g12434, -(V_g12404, W_g12442), J_g12444), path(N1_g20164, X_g20166, Y_g20168, L1_g20170, -(U_g20178, V_g20180), I_g20182), path(N2_g20198, Y_g20168, Z_g20200, L2_g20202, -(V_g20180, W_g20210), J_g20212), augmentation, ','(nonground(Y_g20168), ','(=(J_g20212, 1), ','(;(->(=(I_g20182, 1), @<(X_g20166, Z_g20200)), true), ','(transl(L1_g20170, L2_g20202, L3_g20288, -(-(U_g20178, V_g20180), W_g20210)), ','(length(L3_g20288, M_g20314), is(K_g20322, +(I_g20182, J_g20212))))))), path(M_g20314, X_g20166, Z_g20200, L3_g20288, -(U_g20178, W_g20210), K_g20322), propagate_xy_yz)),
	no_delayed_goals(','(nonground(Y_g12392), ','(=(J_g12444, 1), ','(;(->(=(I_g12406, 1), @<(X_g12390, Z_g12432)), true), ','(transl(L1_g12394, L2_g12434, L3_g12516, -(-(U_g12402, V_g12404), W_g12442)), ','(length(L3_g12516, M_g12542), is(K_g12550, +(I_g12406, J_g12444)))))))),
	!,
	coca(fired_rule(propagate_xy_yz)),
	'CHRpath_6__3__5__7__9__11__12'(Cs_g19692, KF1_g12408, path(N1_g12388, X_g12390, Y_g12392, L1_g12394, -(U_g12402, V_g12404), I_g12406), PA1_g19690, Nb1_g19642),
	path(M_g12542, X_g12390, Z_g12432, L3_g12516, -(U_g12402, W_g12442), K_g12550).

'CHRpath_6__3__5__7__9__11__12'([_g20764|Cs_g20772], KF1_g20774, First_Head_g20780, PA_g20786, Nb1_g20792) :-
	'CHRpath_6__3__5__7__9__11__12'(Cs_g20772, KF1_g20774, First_Head_g20780, PA_g20786, Nb1_g20792).

'CHRpath_6__3__5__7__9__11__12'([], KF1_g20774, Constraint_g20856, PA_g20786, Nb1_g20792) :-
	'CHRpath_6__3__5__7__9__11__13'(Constraint_g20856, KF1_g20774, PA_g20786, Nb1_g20792).

:-
	set_flag(/('CHRpath_6__3__5__7__9__11__12', 5), leash, notrace).

'CHRpath_6__3__5__7__9__11__13'(path(N2_g12618, Y_g12620, Z_g12622, L2_g12624, -(V_g12632, W_g12634), J_g12636), KF2_g12638, PA_g21040, Nb1_g21046) ?-
	'CHRvar'(KF2_g12638),
	!,
	'CHRget_delayed_goals'(V_g12632, Cs_g21096),
	'CHRpath_6__3__5__7__9__11__13__14'(Cs_g21096, KF2_g12638, path(N2_g12618, Y_g12620, Z_g12622, L2_g12624, -(V_g12632, W_g12634), J_g12636), PA_g21040, Nb1_g21046).

'CHRpath_6__3__5__7__9__11__13'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF2_g12638, PA_g21040, Nb1_g21046) :-
	'CHRpath_6__3__5__7__9__11__13__15'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF2_g12638, PA_g21040, Nb1_g21046).

:-
	set_flag(/('CHRpath_6__3__5__7__9__11__13', 4), leash, notrace).

'CHRpath_6__3__5__7__9__11__13__14'(['CHRpath_6'(path(N1_g12660, X_g12662, Y_g12620, L1_g12664, -(U_g12672, V_g12632), I_g12674), KF1_g12676, PA2_g21098, Nb2_g21088)|Cs_g21096], KF2_g12638, path(N2_g12618, Y_g12620, Z_g12622, L2_g12624, -(V_g12632, W_g12634), J_g12636), PA1_g21094, Nb1_g21046) ?-
	'CHRvar'(KF1_g12676),
	'CHRcheck_and_mark_applied'('21'(propagate_xy_yz), KF2_g12638, KF1_g12676, PA1_g21094, PA2_g21098),
	coca(try_double(Nb1_g21046, path(N2_g12618, Y_g12620, Z_g12622, L2_g12624, -(V_g12632, W_g12634), J_g12636), Nb2_g21088, path(N1_g12660, X_g12662, Y_g12620, L1_g12664, -(U_g12672, V_g12632), I_g12674), path(N2_g21570, Y_g21572, Z_g21574, L2_g21576, -(V_g21584, W_g21586), J_g21588), path(N1_g21604, X_g21606, Y_g21572, L1_g21608, -(U_g21616, V_g21584), I_g21618), augmentation, ','(nonground(Y_g21572), ','(=(J_g21588, 1), ','(;(->(=(I_g21618, 1), @<(X_g21606, Z_g21574)), true), ','(transl(L1_g21608, L2_g21576, L3_g21694, -(-(U_g21616, V_g21584), W_g21586)), ','(length(L3_g21694, M_g21720), is(K_g21728, +(I_g21618, J_g21588))))))), path(M_g21720, X_g21606, Z_g21574, L3_g21694, -(U_g21616, W_g21586), K_g21728), propagate_xy_yz)),
	no_delayed_goals(','(nonground(Y_g12620), ','(=(J_g12636, 1), ','(;(->(=(I_g12674, 1), @<(X_g12662, Z_g12622)), true), ','(transl(L1_g12664, L2_g12624, L3_g12746, -(-(U_g12672, V_g12632), W_g12634)), ','(length(L3_g12746, M_g12772), is(K_g12780, +(I_g12674, J_g12636)))))))),
	!,
	coca(fired_rule(propagate_xy_yz)),
	'CHRpath_6__3__5__7__9__11__13__14'(Cs_g21096, KF2_g12638, path(N2_g12618, Y_g12620, Z_g12622, L2_g12624, -(V_g12632, W_g12634), J_g12636), PA1_g21094, Nb1_g21046),
	path(M_g12772, X_g12662, Z_g12622, L3_g12746, -(U_g12672, W_g12634), K_g12780).

'CHRpath_6__3__5__7__9__11__13__14'([_g22170|Cs_g22178], KF1_g22180, First_Head_g22186, PA_g22192, Nb1_g22198) :-
	'CHRpath_6__3__5__7__9__11__13__14'(Cs_g22178, KF1_g22180, First_Head_g22186, PA_g22192, Nb1_g22198).

'CHRpath_6__3__5__7__9__11__13__14'([], KF1_g22180, Constraint_g22262, PA_g22192, Nb1_g22198) :-
	'CHRpath_6__3__5__7__9__11__13__15'(Constraint_g22262, KF1_g22180, PA_g22192, Nb1_g22198).

:-
	set_flag(/('CHRpath_6__3__5__7__9__11__13__14', 5), leash, notrace).

'CHRpath_6__3__5__7__9__11__13__15'(path(N1_g12848, X_g12850, Y_g12852, L1_g12854, -(U_g12862, V_g12864), I_g12866), KF1_g12868, PA_g22446, Nb1_g22452) ?-
	'CHRvar'(KF1_g12868),
	!,
	'CHRget_delayed_goals'(U_g12862, Cs_g22502),
	'CHRpath_6__3__5__7__9__11__13__15__16'(Cs_g22502, KF1_g12868, path(N1_g12848, X_g12850, Y_g12852, L1_g12854, -(U_g12862, V_g12864), I_g12866), PA_g22446, Nb1_g22452).

'CHRpath_6__3__5__7__9__11__13__15'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF1_g12868, PA_g22446, Nb1_g22452) :-
	'CHRpath_6__3__5__7__9__11__13__15__17'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF1_g12868, PA_g22446, Nb1_g22452).

:-
	set_flag(/('CHRpath_6__3__5__7__9__11__13__15', 4), leash, notrace).

'CHRpath_6__3__5__7__9__11__13__15__16'(['CHRpath_6'(path(N2_g12890, X_g12850, Z_g12892, L3_g12894, -(U_g12862, W_g12902), J_g12904), KF2_g12906, PA2_g22504, Nb2_g22494)|Cs_g22502], KF1_g12868, path(N1_g12848, X_g12850, Y_g12852, L1_g12854, -(U_g12862, V_g12864), I_g12866), PA1_g22500, Nb1_g22452) ?-
	'CHRvar'(KF2_g12906),
	'CHRcheck_and_mark_applied'('12'(propagate_xy_xz), KF1_g12868, KF2_g12906, PA1_g22500, PA2_g22504),
	coca(try_double(Nb1_g22452, path(N1_g12848, X_g12850, Y_g12852, L1_g12854, -(U_g12862, V_g12864), I_g12866), Nb2_g22494, path(N2_g12890, X_g12850, Z_g12892, L3_g12894, -(U_g12862, W_g12902), J_g12904), path(N1_g22978, X_g22980, Y_g22982, L1_g22984, -(U_g22992, V_g22994), I_g22996), path(N2_g23012, X_g22980, Z_g23014, L3_g23016, -(U_g22992, W_g23024), J_g23026), augmentation, ','(nonground(X_g22980), ','(min(I_g22996, J_g23026, 1), ','(@<(Y_g22982, Z_g23014), ','(transl(L1_g22984, L2_g23086, L3_g23016, -(-(U_g22992, V_g22994), W_g23024)), ','(length(L2_g23086, M_g23112), is(K_g23120, +(I_g22996, J_g23026))))))), path(M_g23112, Y_g22982, Z_g23014, L2_g23086, -(V_g22994, W_g23024), K_g23120), propagate_xy_xz)),
	no_delayed_goals(','(nonground(X_g12850), ','(min(I_g12866, J_g12904, 1), ','(@<(Y_g12852, Z_g12892), ','(transl(L1_g12854, L2_g12960, L3_g12894, -(-(U_g12862, V_g12864), W_g12902)), ','(length(L2_g12960, M_g12986), is(K_g12994, +(I_g12866, J_g12904)))))))),
	!,
	coca(fired_rule(propagate_xy_xz)),
	'CHRpath_6__3__5__7__9__11__13__15__16'(Cs_g22502, KF1_g12868, path(N1_g12848, X_g12850, Y_g12852, L1_g12854, -(U_g12862, V_g12864), I_g12866), PA1_g22500, Nb1_g22452),
	path(M_g12986, Y_g12852, Z_g12892, L2_g12960, -(V_g12864, W_g12902), K_g12994).

'CHRpath_6__3__5__7__9__11__13__15__16'([_g23562|Cs_g23570], KF1_g23572, First_Head_g23578, PA_g23584, Nb1_g23590) :-
	'CHRpath_6__3__5__7__9__11__13__15__16'(Cs_g23570, KF1_g23572, First_Head_g23578, PA_g23584, Nb1_g23590).

'CHRpath_6__3__5__7__9__11__13__15__16'([], KF1_g23572, Constraint_g23654, PA_g23584, Nb1_g23590) :-
	'CHRpath_6__3__5__7__9__11__13__15__17'(Constraint_g23654, KF1_g23572, PA_g23584, Nb1_g23590).

:-
	set_flag(/('CHRpath_6__3__5__7__9__11__13__15__16', 5), leash, notrace).

'CHRpath_6__3__5__7__9__11__13__15__17'(path(N2_g13062, X_g13064, Z_g13066, L3_g13068, -(U_g13076, W_g13078), J_g13080), KF2_g13082, PA_g23838, Nb1_g23844) ?-
	'CHRvar'(KF2_g13082),
	!,
	'CHRget_delayed_goals'(U_g13076, Cs_g23894),
	'CHRpath_6__3__5__7__9__11__13__15__17__18'(Cs_g23894, KF2_g13082, path(N2_g13062, X_g13064, Z_g13066, L3_g13068, -(U_g13076, W_g13078), J_g13080), PA_g23838, Nb1_g23844).

'CHRpath_6__3__5__7__9__11__13__15__17'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF2_g13082, PA_g23838, Nb1_g23844) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF2_g13082, PA_g23838, Nb1_g23844).

:-
	set_flag(/('CHRpath_6__3__5__7__9__11__13__15__17', 4), leash, notrace).

'CHRpath_6__3__5__7__9__11__13__15__17__18'(['CHRpath_6'(path(N1_g13104, X_g13064, Y_g13106, L1_g13108, -(U_g13076, V_g13116), I_g13118), KF1_g13120, PA2_g23896, Nb2_g23886)|Cs_g23894], KF2_g13082, path(N2_g13062, X_g13064, Z_g13066, L3_g13068, -(U_g13076, W_g13078), J_g13080), PA1_g23892, Nb1_g23844) ?-
	'CHRvar'(KF1_g13120),
	'CHRcheck_and_mark_applied'('21'(propagate_xy_xz), KF2_g13082, KF1_g13120, PA1_g23892, PA2_g23896),
	coca(try_double(Nb1_g23844, path(N2_g13062, X_g13064, Z_g13066, L3_g13068, -(U_g13076, W_g13078), J_g13080), Nb2_g23886, path(N1_g13104, X_g13064, Y_g13106, L1_g13108, -(U_g13076, V_g13116), I_g13118), path(N2_g24372, X_g24374, Z_g24376, L3_g24378, -(U_g24386, W_g24388), J_g24390), path(N1_g24406, X_g24374, Y_g24408, L1_g24410, -(U_g24386, V_g24418), I_g24420), augmentation, ','(nonground(X_g24374), ','(min(I_g24420, J_g24390, 1), ','(@<(Y_g24408, Z_g24376), ','(transl(L1_g24410, L2_g24480, L3_g24378, -(-(U_g24386, V_g24418), W_g24388)), ','(length(L2_g24480, M_g24506), is(K_g24514, +(I_g24420, J_g24390))))))), path(M_g24506, Y_g24408, Z_g24376, L2_g24480, -(V_g24418, W_g24388), K_g24514), propagate_xy_xz)),
	no_delayed_goals(','(nonground(X_g13064), ','(min(I_g13118, J_g13080, 1), ','(@<(Y_g13106, Z_g13066), ','(transl(L1_g13108, L2_g13174, L3_g13068, -(-(U_g13076, V_g13116), W_g13078)), ','(length(L2_g13174, M_g13200), is(K_g13208, +(I_g13118, J_g13080)))))))),
	!,
	coca(fired_rule(propagate_xy_xz)),
	'CHRpath_6__3__5__7__9__11__13__15__17__18'(Cs_g23894, KF2_g13082, path(N2_g13062, X_g13064, Z_g13066, L3_g13068, -(U_g13076, W_g13078), J_g13080), PA1_g23892, Nb1_g23844),
	path(M_g13200, Y_g13106, Z_g13066, L2_g13174, -(V_g13116, W_g13078), K_g13208).

'CHRpath_6__3__5__7__9__11__13__15__17__18'([_g24956|Cs_g24964], KF1_g24966, First_Head_g24972, PA_g24978, Nb1_g24984) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__18'(Cs_g24964, KF1_g24966, First_Head_g24972, PA_g24978, Nb1_g24984).

'CHRpath_6__3__5__7__9__11__13__15__17__18'([], KF1_g24966, Constraint_g25048, PA_g24978, Nb1_g24984) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19'(Constraint_g25048, KF1_g24966, PA_g24978, Nb1_g24984).

:-
	set_flag(/('CHRpath_6__3__5__7__9__11__13__15__17__18', 5), leash, notrace).

'CHRpath_6__3__5__7__9__11__13__15__17__19'(path(N1_g13276, X_g13278, Y_g13280, L3_g13282, -(U_g13290, V_g13292), I_g13294), KF1_g13296, PA_g25232, Nb1_g25238) ?-
	'CHRvar'(KF1_g13296),
	!,
	'CHRget_delayed_goals'(V_g13292, Cs_g25288),
	'CHRpath_6__3__5__7__9__11__13__15__17__19__20'(Cs_g25288, KF1_g13296, path(N1_g13276, X_g13278, Y_g13280, L3_g13282, -(U_g13290, V_g13292), I_g13294), PA_g25232, Nb1_g25238).

'CHRpath_6__3__5__7__9__11__13__15__17__19'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF1_g13296, PA_g25232, Nb1_g25238) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19__21'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF1_g13296, PA_g25232, Nb1_g25238).

:-
	set_flag(/('CHRpath_6__3__5__7__9__11__13__15__17__19', 4), leash, notrace).

'CHRpath_6__3__5__7__9__11__13__15__17__19__20'(['CHRpath_6'(path(N2_g13318, Z_g13320, Y_g13280, L2_g13322, -(W_g13330, V_g13292), J_g13332), KF2_g13334, PA2_g25290, Nb2_g25280)|Cs_g25288], KF1_g13296, path(N1_g13276, X_g13278, Y_g13280, L3_g13282, -(U_g13290, V_g13292), I_g13294), PA1_g25286, Nb1_g25238) ?-
	'CHRvar'(KF2_g13334),
	'CHRcheck_and_mark_applied'('12'(propagate_xy_zy), KF1_g13296, KF2_g13334, PA1_g25286, PA2_g25290),
	coca(try_double(Nb1_g25238, path(N1_g13276, X_g13278, Y_g13280, L3_g13282, -(U_g13290, V_g13292), I_g13294), Nb2_g25280, path(N2_g13318, Z_g13320, Y_g13280, L2_g13322, -(W_g13330, V_g13292), J_g13332), path(N1_g25768, X_g25770, Y_g25772, L3_g25774, -(U_g25782, V_g25784), I_g25786), path(N2_g25802, Z_g25804, Y_g25772, L2_g25806, -(W_g25814, V_g25784), J_g25816), augmentation, ','(nonground(Y_g25772), ','(min(I_g25786, J_g25816, 1), ','(@<(X_g25770, Z_g25804), ','(transl(L1_g25876, L2_g25806, L3_g25774, -(-(U_g25782, W_g25814), V_g25784)), ','(length(L1_g25876, M_g25902), is(K_g25910, +(I_g25786, J_g25816))))))), path(M_g25902, X_g25770, Z_g25804, L1_g25876, -(U_g25782, W_g25814), K_g25910), propagate_xy_zy)),
	no_delayed_goals(','(nonground(Y_g13280), ','(min(I_g13294, J_g13332, 1), ','(@<(X_g13278, Z_g13320), ','(transl(L1_g13388, L2_g13322, L3_g13282, -(-(U_g13290, W_g13330), V_g13292)), ','(length(L1_g13388, M_g13414), is(K_g13422, +(I_g13294, J_g13332)))))))),
	!,
	coca(fired_rule(propagate_xy_zy)),
	'CHRpath_6__3__5__7__9__11__13__15__17__19__20'(Cs_g25288, KF1_g13296, path(N1_g13276, X_g13278, Y_g13280, L3_g13282, -(U_g13290, V_g13292), I_g13294), PA1_g25286, Nb1_g25238),
	path(M_g13414, X_g13278, Z_g13320, L1_g13388, -(U_g13290, W_g13330), K_g13422).

'CHRpath_6__3__5__7__9__11__13__15__17__19__20'([_g26352|Cs_g26360], KF1_g26362, First_Head_g26368, PA_g26374, Nb1_g26380) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19__20'(Cs_g26360, KF1_g26362, First_Head_g26368, PA_g26374, Nb1_g26380).

'CHRpath_6__3__5__7__9__11__13__15__17__19__20'([], KF1_g26362, Constraint_g26444, PA_g26374, Nb1_g26380) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19__21'(Constraint_g26444, KF1_g26362, PA_g26374, Nb1_g26380).

:-
	set_flag(/('CHRpath_6__3__5__7__9__11__13__15__17__19__20', 5), leash, notrace).

'CHRpath_6__3__5__7__9__11__13__15__17__19__21'(path(N2_g13490, Z_g13492, Y_g13494, L2_g13496, -(W_g13504, V_g13506), J_g13508), KF2_g13510, PA_g26628, Nb1_g26634) ?-
	'CHRvar'(KF2_g13510),
	!,
	'CHRget_delayed_goals'(V_g13506, Cs_g26684),
	'CHRpath_6__3__5__7__9__11__13__15__17__19__21__22'(Cs_g26684, KF2_g13510, path(N2_g13490, Z_g13492, Y_g13494, L2_g13496, -(W_g13504, V_g13506), J_g13508), PA_g26628, Nb1_g26634).

'CHRpath_6__3__5__7__9__11__13__15__17__19__21'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF2_g13510, PA_g26628, Nb1_g26634) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19__21__23'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF2_g13510, PA_g26628, Nb1_g26634).

:-
	set_flag(/('CHRpath_6__3__5__7__9__11__13__15__17__19__21', 4), leash, notrace).

'CHRpath_6__3__5__7__9__11__13__15__17__19__21__22'(['CHRpath_6'(path(N1_g13532, X_g13534, Y_g13494, L3_g13536, -(U_g13544, V_g13506), I_g13546), KF1_g13548, PA2_g26686, Nb2_g26676)|Cs_g26684], KF2_g13510, path(N2_g13490, Z_g13492, Y_g13494, L2_g13496, -(W_g13504, V_g13506), J_g13508), PA1_g26682, Nb1_g26634) ?-
	'CHRvar'(KF1_g13548),
	'CHRcheck_and_mark_applied'('21'(propagate_xy_zy), KF2_g13510, KF1_g13548, PA1_g26682, PA2_g26686),
	coca(try_double(Nb1_g26634, path(N2_g13490, Z_g13492, Y_g13494, L2_g13496, -(W_g13504, V_g13506), J_g13508), Nb2_g26676, path(N1_g13532, X_g13534, Y_g13494, L3_g13536, -(U_g13544, V_g13506), I_g13546), path(N2_g27166, Z_g27168, Y_g27170, L2_g27172, -(W_g27180, V_g27182), J_g27184), path(N1_g27200, X_g27202, Y_g27170, L3_g27204, -(U_g27212, V_g27182), I_g27214), augmentation, ','(nonground(Y_g27170), ','(min(I_g27214, J_g27184, 1), ','(@<(X_g27202, Z_g27168), ','(transl(L1_g27274, L2_g27172, L3_g27204, -(-(U_g27212, W_g27180), V_g27182)), ','(length(L1_g27274, M_g27300), is(K_g27308, +(I_g27214, J_g27184))))))), path(M_g27300, X_g27202, Z_g27168, L1_g27274, -(U_g27212, W_g27180), K_g27308), propagate_xy_zy)),
	no_delayed_goals(','(nonground(Y_g13494), ','(min(I_g13546, J_g13508, 1), ','(@<(X_g13534, Z_g13492), ','(transl(L1_g13602, L2_g13496, L3_g13536, -(-(U_g13544, W_g13504), V_g13506)), ','(length(L1_g13602, M_g13628), is(K_g13636, +(I_g13546, J_g13508)))))))),
	!,
	coca(fired_rule(propagate_xy_zy)),
	'CHRpath_6__3__5__7__9__11__13__15__17__19__21__22'(Cs_g26684, KF2_g13510, path(N2_g13490, Z_g13492, Y_g13494, L2_g13496, -(W_g13504, V_g13506), J_g13508), PA1_g26682, Nb1_g26634),
	path(M_g13628, X_g13534, Z_g13492, L1_g13602, -(U_g13544, W_g13504), K_g13636).

'CHRpath_6__3__5__7__9__11__13__15__17__19__21__22'([_g27750|Cs_g27758], KF1_g27760, First_Head_g27766, PA_g27772, Nb1_g27778) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19__21__22'(Cs_g27758, KF1_g27760, First_Head_g27766, PA_g27772, Nb1_g27778).

'CHRpath_6__3__5__7__9__11__13__15__17__19__21__22'([], KF1_g27760, Constraint_g27842, PA_g27772, Nb1_g27778) :-
	'CHRpath_6__3__5__7__9__11__13__15__17__19__21__23'(Constraint_g27842, KF1_g27760, PA_g27772, Nb1_g27778).

:-
	set_flag(/('CHRpath_6__3__5__7__9__11__13__15__17__19__21__22', 5), leash, notrace).

'CHRpath_6__3__5__7__9__11__13__15__17__19__21__23'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF_g28002, PA_g28008, Nb_g28014) :-
	(;(->('CHRvar'(KF_g28002), 'CHRdelay'([KF_g28002, path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084)], 'CHRpath_6'(path(_g11074, _g11076, _g11078, _g11080, _g11082, _g11084), KF_g28002, PA_g28008, Nb_g28014))), true)).

:-
	set_flag(/('CHRpath_6__3__5__7__9__11__13__15__17__19__21__23', 4), leash, notrace).


:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
