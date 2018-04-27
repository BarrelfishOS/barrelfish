% load the decoding net
%:- use_module("/home/luki/ETH/barrelfish/usr/skb/programs/decoding_net4_support.pl").
:- use_module("decoding_net4.pl").

reset_static_state :- 
    (retract_translate(_,_) ; true),
    (retract_overlay(_,_) ; true).

test_translate_region1 :-
    reset_static_state,
    assert_translate(region(["IN"], block(0, 1000)), name(["OUT"], 400)),
    state_empty(S),
    translate_region(S, region(["IN"], block(0, 1000)), Out),
    writeln(Out).

test_translate_region2 :-
    reset_static_state,
    assert_translate(region(["IN"], block(0, 1000)), name(["OUT"], 400)),
    state_empty(S),
    translate_region(S, In, region(["OUT"], block(400, 1400))),
    writeln(In).

test_translate_region3 :-
    reset_static_state,
    assert_translate(region(["IN"], block(0, 1000)), name(["OUT"], 400)),
    state_empty(S),
    not(translate_region(S, In, region(["OUT"], block(400, 1500)))).

test_translate_region4 :-
    reset_static_state,
    assert_overlay(["IN"], ["OUT"]),
    state_empty(S),
    translate_region(S, In, region(["OUT"], block(400, 1500))),
    writeln(In).

run_test(Test) :-
    (
        call(Test),
        printf("Test %p succeeds!\n", Test)
    ) ; (
        printf("!!! Test %p failed !!!\n", Test)
    ).

:- export run_all_tests/0.
run_all_tests :-
    run_test(test_translate_region1),
    run_test(test_translate_region2),
    run_test(test_translate_region3),
    run_test(test_translate_region4).
