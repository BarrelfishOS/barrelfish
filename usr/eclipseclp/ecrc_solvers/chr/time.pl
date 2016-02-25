
%%% The following code has been produced by the CHR compiler


:- ( current_module(chr) -> true ; use_module(library(chr)) ).

:- get_flag(variable_names, Val), setval(variable_names_flag, Val), set_flag(variable_names, off).
:- ['time-pc.pl'].
inf(3.40282e+38).
minf(-3.40282e+38).
sup(1e-45).
msup(-1e-45).
delay empty(_261, _264, _267) if var(_261), var(_264).
empty(0, [], _349).
delay universal(_364, _367, _370) if var(_364).
delay universal(_429, _432, _435) if var(_432).
delay universal(_494, _497, _500) if var(_500).
universal(_557, _560, _563) :- (is_quantl(_560) -> _560 = [_583 - _587], _583 =< minf, inf =< _587 ; _563 = p - p -> sort(_560, [eq, ge, le]) ; size(_563, _557)), !.
size(i - i, 13).
size(p - p, 3).
size(p - i, 5).
size(i - p, 5).
size(s - s, 5).
delay equality(_927, _930) if var(_930).
delay equality(_984, _987) if var(_984).
equality(_1039, i - i) :- !, member(equals, _1039).
equality(_1119, s - s) :- !, member(eq, _1119).
equality(_1199, p - p) :- (is_quall(_1199) -> member(_1220, _1199), (_1220 = eq ; number(_1220), _1220 =:= 0) ; member(_1250 - _1254, _1199), (_1250 = 0, _1254 = 0 ; _1250 =< 0, 0 =< _1254)), !.
delay unique(_1510) if nonground(_1510).
unique([_1559 - _1563]) :- !, _1559 =:= _1563.
unique([_1639]).
bind_value(_1650, _1653, [_1658]) :- (_1658 = _1669 - _1672 ; _1658 = _1669) -> _1653 =:= _1650 + _1669.
shift_interval(_1794, [], []).
shift_interval(_1811, [_1816 - _1820|_1821], [_1826 - _1830|_1831]) :- !, _1826 is _1816 - _1811, _1830 is _1820 - _1811, shift_interval(_1811, _1821, _1831).
shift_interval(_2015, [_2020|_2021], [_2026|_2027]) :- _2026 is _2020 - _2015, shift_interval(_2015, _2021, _2027).
delay intersection(_2147, _2150, _2153, _2156) if var(_2147).
delay intersection(_2220, _2223, _2226, _2229) if var(_2223).
intersection(_2291, _2294, _2297, _2300) :- qtype(_2291, _2313), qtype(_2294, _2324), (_2313 == quall, _2324 == quall -> intersection(_2291, _2294, _2297) ; qualquant(_2291, _2313, _2362), qualquant(_2294, _2324, _2376), interint(_2362, _2376, _2297)), !.
interint([], _2598, []).
interint(_2613, [], []) :- _2613 = [_2628|_2629].
interint([_2640|_2641], [_2646|_2647], _2650) :- isless(_2640, _2646) -> interint(_2641, [_2646|_2647], _2650) ; isless(_2646, _2640) -> interint([_2640|_2641], _2647, _2650) ; overlaps1(_2640, _2646, _2714) -> _2650 = [_2714|_2727], interint([_2640|_2641], _2647, _2727) ; overlaps2(_2640, _2646, _2714) -> _2650 = [_2714|_2727], interint(_2641, [_2646|_2647], _2727).
isless(_3073 - _3077, _3080 - _3084) :- _3077 < _3080.
overlaps1(_3152 - _3156, _3159 - _3163, _3166 - _3170) :- _3156 >= _3163, _3159 =< _3156, _3152 =< _3163, my_max(_3152, _3159, _3166), my_min(_3156, _3163, _3170).
overlaps2(_3370 - _3374, _3377 - _3381, _3384 - _3388) :- _3381 >= _3374, _3377 =< _3374, _3370 =< _3381, my_max(_3370, _3377, _3384), my_min(_3374, _3381, _3388).
my_max(_3588, _3591, _3594) :- _3588 >= _3591, !, _3588 = _3594.
my_max(_3685, _3688, _3688).
my_min(_3703, _3706, _3709) :- _3703 =< _3706, !, _3703 = _3709.
my_min(_3800, _3803, _3803).
delay transl(_3820, _3823, _3826, _3829) if var(_3820), var(_3823).
delay transl(_3914, _3917, _3920, _3923) if var(_3914), var(_3920).
delay transl(_4008, _4011, _4014, _4017) if var(_4011), var(_4014).
transl(_4100, _4103, _4106, _4109) :- qtype(_4100, _4122), qtype(_4103, _4133), qtype(_4106, _4144), (_4109 = p - p - p, (_4122 == quantl ; _4133 == quantl ; _4144 == quantl) -> qualquant(_4100, _4122, _4187), qualquant(_4103, _4133, _4201), qualquant(_4106, _4144, _4215), transl(_4187, _4201, _4215, _4109, quantl) ; quantqual(_4100, _4122, _4187), quantqual(_4103, _4133, _4201), quantqual(_4106, _4144, _4215), transl(_4187, _4201, _4215, _4109, quall)), !.
transl(_4675, _4678, _4681, _4684, _4687) :- var(_4681), !, setof(_4709, _4712 ^ _4716 ^ (member(_4712, _4675), member(_4716, _4678), trans(_4712, _4716, _4709, _4684, _4687)), _4758), mergerel(_4758, _4681, _4684, _4687).
transl(_4959, _4962, _4965, _4968, _4971) :- var(_4962), !, setof(_4993, _4996 ^ _5000 ^ (member(_4996, _4959), member(_5000, _4965), trans(_4996, _4993, _5000, _4968, _4971)), _5042), mergerel(_5042, _4962, _4968, _4971).
transl(_5243, _5246, _5249, _5252, _5255) :- var(_5243), !, setof(_5277, _5280 ^ _5284 ^ (member(_5280, _5246), member(_5284, _5249), trans(_5277, _5280, _5284, _5252, _5255)), _5326), mergerel(_5326, _5243, _5252, _5255).
mergerel(_5527, _5530, _5533, _5536) :- (_5536 == quantl -> mergerel(_5527, _5530) ; _5527 = _5530), !.
mergerel([], []).
mergerel([_5675 - _5679, _5682 - _5686|_5687], _5690) :- _5679 + sup >= _5682, !, my_min(_5675, _5682, _5715), my_max(_5679, _5686, _5729), mergerel([_5715 - _5729|_5687], _5690).
mergerel([_5922|_5923], [_5922|_5928]) :- mergerel(_5923, _5928).
trans(_5950, _5953, _5956, s - s - s, quall) :- !, strans(_5950, _5953, _5956).
trans(_6072, _6075, _6078, p - p - p, quall) :- !, prans(_6072, _6075, _6078).
trans(_6194, _6197, _6200, p - p - p, quantl) :- !, qtrans(_6194, _6197, _6200).
trans(_6316, _6319, _6322, _6325 - _6329 - _6333, quall) :- !, itrans(_6325 - _6329 - _6333, _6316, _6319, _6322).
delay qtype(_6474, _6477) if nonground(_6474).
qtype(_6529, quantl) :- is_quantl(_6529).
qtype(_6550, quall) :- is_quall(_6550).
is_quantl([_6573|_6572]) :- is_quant(_6573).
is_quall([_6593|_6592]) :- is_qual(_6593).
delay is_quant(_6613) if var(_6613).
is_quant(_6660 - _6664).
delay is_qual(_6677) if var(_6677).
is_qual(_6724) :- atomic(_6724).
delay qualquant(_6744, _6747, _6750) if var(_6744), var(_6750).
qualquant(_6828, _6831, _6834) :- _6831 == quall -> qualquant(_6828, _6852), mergerel(_6852, _6834) ; _6831 = quantl -> _6828 = _6834.
delay quantqual(_6996, _6999, _7002) if var(_6996), var(_7002).
quantqual(_7080, _7083, _7086) :- _7083 == quantl -> quantqual(_7080, _7086) ; _7083 = quall -> _7080 = _7086.
qualquant([], []).
qualquant([_7142|_7143], [_7148|_7149]) :- qualquant1(_7142, _7148), qualquant(_7143, _7149).
qualquant1(le, _7249 - _7253) :- !, _7249 is sup, _7253 is inf.
qualquant1(eq, 0 - 0) :- !.
qualquant1(ge, _7369 - _7373) :- !, _7369 is minf, _7373 is msup.
qualquant1(_7468, _7471 - _7471) :- _7471 is _7468.
quantqual(_7533, _7536) :- findall(_7545, quantqual1(_7533, _7545), _7536).
quantqual1(_7569, eq) :- once (member(_7582 - _7586, _7569), _7582 =< 0, 0 =< _7586).
quantqual1(_7696, le) :- once (member(_7709 - _7713, _7696), 0 < _7713).
quantqual1(_7799, ge) :- once (member(_7812 - _7816, _7799), _7812 < 0).
quantqual1(_7902, _7905) :- once (member(_7905 - _7919, _7902), _7905 =:= _7919).
:- ['allentable.pl'].
check_ii(_8015, _8018, _8021) :- interval_point(_8015, _8021, _8018).
interval_point([_8049, _8052], before, [_8059, _8062]) :- _8052 < _8059.
interval_point([_8151, _8154], after, [_8161, _8164]) :- _8164 < _8151.
interval_point([_8253, _8256], meets, [_8263, _8266]) :- _8256 =:= _8263.
interval_point([_8355, _8358], met_by, [_8365, _8368]) :- _8368 =:= _8355.
interval_point([_8457, _8460], starts, [_8467, _8470]) :- _8457 =:= _8467, _8460 < _8470.
interval_point([_8583, _8586], started_by, [_8593, _8596]) :- _8583 =:= _8593, _8596 < _8586.
interval_point([_8709, _8712], finishes, [_8719, _8722]) :- _8712 =:= _8722, _8719 < _8709.
interval_point([_8835, _8838], finished_by, [_8845, _8848]) :- _8838 =:= _8848, _8835 < _8848.
interval_point([_8961, _8964], during, [_8971, _8974]) :- _8971 < _8961, _8964 < _8974.
interval_point([_9087, _9090], contains, [_9097, _9100]) :- _9087 < _9097, _9100 < _9090.
interval_point([_9213, _9216], overlaps, [_9223, _9226]) :- _9213 < _9223, _9223 < _9216, _9216 < _9226.
interval_point([_9363, _9366], overlapped_by, [_9373, _9376]) :- _9373 < _9363, _9363 < _9376, _9376 < _9366.
interval_point([_9513, _9516], equals, [_9523, _9526]) :- _9513 =:= _9523, _9516 =:= _9526.
itrans(_9637 - _9641 - _9645, _9648, _9651, _9654) :- encode(_9637 - _9641, _9648, _9672), encode(_9641 - _9645, _9651, _9689), encode(_9637 - _9645, _9654, _9706), cons_tri(_9672, _9689, _9706).
delay encode(_9889 - _9893, _9896, _9899) if var(_9896), var(_9899).
encode(i - i, _9991, _9994) :- !, encode(_9991, _9994).
encode(p - i, _10079, _10082) :- !, pi_ii(_10079, _10097), encode(_10097, _10082).
encode(i - p, _10196, _10199) :- !, ip_ii(_10196, _10214), encode(_10214, _10199).
encode(p - p, _10313, _10316) :- !, pp_pi(_10313, _10331), pi_ii(_10331, _10342), encode(_10342, _10316).
delay encode(_10456, _10459) if var(_10456), var(_10459).
encode(before, 1).
encode(after, 2).
encode(during, 3).
encode(contains, 4).
encode(overlaps, 5).
encode(overlapped_by, 6).
encode(meets, 7).
encode(met_by, 8).
encode(starts, 9).
encode(started_by, 10).
encode(finishes, 11).
encode(finished_by, 12).
encode(equals, 13).
check_pp(_10701, _10704, _10707 - _10711) :- !, _10701 + _10707 < _10704, _10704 < _10701 + _10711.
check_pp(_10835, _10838, _10841) :- number(_10841), !, _10835 + _10841 =:= _10838.
check_pp(_10941, _10944, _10947) :- \+ member(_10947, [le, eq, ge]), !, _10944 = _10947.
check_pp(_11079, _11082, _11085) :- number(_11079), number(_11082) -> check_ppn(_11079, _11082, _11085) ; check_ppt(_11079, _11082, _11085).
check_ppn(_11228, _11231, le) :- _11228 < _11231.
check_ppn(_11288, _11291, eq) :- _11288 =:= _11291.
check_ppn(_11348, _11351, ge) :- _11348 > _11351.
check_ppt(_11408, _11411, le) :- _11408 @< _11411.
check_ppt(_11432, _11435, eq) :- _11432 = _11435.
check_ppt(_11456, _11459, ge) :- _11456 @> _11459.
prans(_11480, _11483, _11486) :- (number(_11480) ; number(_11483) ; number(_11486)), !, qtrans(_11480 - _11480, _11483 - _11483, _11486 - _11486).
prans(le, le, le).
prans(le, eq, le).
prans(le, ge, le).
prans(le, ge, eq).
prans(le, ge, ge).
prans(eq, le, le).
prans(eq, eq, eq).
prans(eq, ge, ge).
prans(ge, le, le).
prans(ge, le, eq).
prans(ge, le, ge).
prans(ge, eq, ge).
prans(ge, ge, ge).
qtrans(_11871 - _11875, _11878 - _11882, _11885 - _11889) :- var(_11871), var(_11875) -> safe_is(_11871, _11885 - _11882), safe_is(_11875, _11889 - _11878) ; var(_11878), var(_11882) -> safe_is(_11878, _11885 - _11875), safe_is(_11882, _11889 - _11871) ; var(_11885), var(_11889) -> safe_is(_11885, _11871 + _11878), safe_is(_11889, _11875 + _11882).
safe_is(_12313, _12316 - _12320) :- _12316 =:= minf, _12320 =:= inf -> _12313 is minf ; _12316 =:= inf, _12320 =:= minf -> _12313 is inf ; _12316 =:= msup, _12320 =:= sup -> _12313 is msup ; _12316 =:= sup, _12320 =:= msup -> _12313 is sup ; _12313 is _12316 - _12320.
safe_is(_12679, _12682 + _12686) :- _12682 =:= inf, _12686 =:= inf -> _12679 is inf ; _12682 =:= minf, _12686 =:= minf -> _12679 is minf ; _12682 =:= sup, _12686 =:= sup -> _12679 is sup ; _12682 =:= msup, _12686 =:= msup -> _12679 is msup ; _12679 is _12682 + _12686.
check_pi(_13045, [_13050, _13053], before) :- _13045 < _13050.
check_pi(_13126, [_13131, _13134], starts) :- _13126 =:= _13131.
check_pi(_13207, [_13212, _13215], during) :- _13212 < _13207, _13207 < _13215.
check_pi(_13312, [_13317, _13320], finishes) :- _13312 =:= _13320.
check_pi(_13393, [_13398, _13401], after) :- _13401 < _13393.
check_pi([_13476, _13479], _13482, after) :- _13482 < _13476.
check_pi([_13557, _13560], _13563, started_by) :- _13563 =:= _13557.
check_pi([_13638, _13641], _13644, contains) :- _13638 < _13644, _13644 < _13641.
check_pi([_13743, _13746], _13749, finished_by) :- _13749 =:= _13746.
check_pi([_13824, _13827], _13830, before) :- _13827 < _13830.
delay pi_ii(_13905, _13908) if var(_13905), var(_13908).
pi_ii(before, before).
pi_ii(before, meets).
pi_ii(before, finished_by).
pi_ii(before, contains).
pi_ii(before, overlaps).
pi_ii(starts, starts).
pi_ii(starts, equals).
pi_ii(starts, started_by).
pi_ii(during, during).
pi_ii(during, finishes).
pi_ii(during, overlaped_by).
pi_ii(finishes, met_by).
pi_ii(after, after).
delay ip_ii(_14152, _14155) if var(_14152), var(_14155).
ip_ii(before, before).
ip_ii(finished_by, meets).
ip_ii(contains, contains).
ip_ii(contains, overlaps).
ip_ii(contains, finished_by).
ip_ii(started_by, starts).
ip_ii(started_by, equals).
ip_ii(started_by, started_by).
ip_ii(after, during).
ip_ii(after, finishes).
ip_ii(after, overlaped_by).
ip_ii(after, met_by).
ip_ii(after, after).
delay pp_pi(_14399, _14402) if var(_14399), var(_14402).
pp_pi(le, before).
pp_pi(eq, starts).
pp_pi(ge, during).
pp_pi(ge, finishes).
pp_pi(ge, after).
delay pp_ii(_14542, _14545) if var(_14542), var(_14545).
pp_ii(_14618, _14621) :- pp_pi(_14618, _14632), pi_ii(_14632, _14621).

:- getval(variable_names_flag, Val), set_flag(variable_names, Val).
