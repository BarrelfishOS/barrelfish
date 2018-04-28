% load the decoding net
%:- use_module("/home/luki/ETH/barrelfish/usr/skb/programs/decoding_net4_support.pl").
%:- use_module("decoding_net4.pl").
:- ["decoding_net4.pl"].

reset_static_state :- 
    (retract_translate(_,_) ; true),
    (retract_accept(_) ; true),
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

test_decodes_region1 :-
    reset_static_state,
    assert_overlay(["IN"], ["NEXT"]),
    assert_translate(region(["NEXT"], block(0, 1000)), name(["OUT"], 400)),
    state_empty(S),
    decodes_region(S, region(["IN"], block(400, 1000)), Out),
    writeln(Out).

test_decodes_region2 :-
    reset_static_state,
    assert_overlay(["IN"], ["NEXT"]),
    assert_translate(region(["NEXT"], block(0, 1000)), name(["OUT"], 400)),
    state_empty(S),
    decodes_region(S, In, region(["OUT"], block(400, 1500))),
    writeln(In).

test_resolves_region1 :-
    reset_static_state,
    assert_overlay(["IN"], ["NEXT"]),
    assert_translate(region(["NEXT"], block(0, 1000)), name(["OUT"], 400)),
    assert_accept(region(["OUT"], block(0, 10000))),
    state_empty(S),
    resolves_region(S, In, region(["OUT"], block(400, 1500))),
    writeln(In).

test_resolves_region2 :-
    reset_static_state,
    assert_overlay(["IN"], ["NEXT"]),
    assert_translate(region(["NEXT"], block(0, 1000)), name(["OUT"], 400)),
    assert_accept(region(["OUT"], block(0, 700))),
    state_empty(S),
    not(resolves_region(S, In, region(["OUT"], block(400, 1500)))).

:- export test_flat1/0.
test_flat1 :-
    reset_static_state,
    % This example resembles the xeon phi socket on
    assert_translate(region(["SOCKET"], block(1000, 2000)), name(["GDDR"], 0)),
    assert_translate(region(["SOCKET"], block(10000, 11000)), name(["SMPT_IN"], 0)),
    assert_configurable(["SMPT_IN"],34,["SMPT_OUT"]),
    assert_overlay(["SMPT_OUT"],["PCIBUS"]),
    assert_translate(region(["PCIBUS"], block(5000, 6000)), name(["DRAM"], 0)),
    assert_accept(region(["DRAM"], block(0, 1000))),
    assert_accept(region(["GDDR"], block(0, 1000))),
    findall((A,B,C), flat(A,B,C), Li),
    (foreach((A,B,C), Li) do 
        printf("flat(%p,%p,%p)\n", [A,B,C])
    ).


run_test(Test) :-
    (
        printf("Calling Test %p...\n", Test),
        call(Test),
        writeln(" Succeeds!")
    ) ; (
        writeln(" !!! Fails !!!")
    ).

:- export run_all_tests/0.
run_all_tests :-
    run_test(test_translate_region1),
    run_test(test_translate_region2),
    run_test(test_translate_region3),
    run_test(test_translate_region4),
    run_test(test_decodes_region1),
    run_test(test_decodes_region2),
    run_test(test_resolves_region1),
    run_test(test_resolves_region2),
    run_test(test_flat1).
