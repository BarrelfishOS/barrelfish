% load the decoding net
:- ["decoding_net4_support.pl"].

reset_static_state :-
    (retract_translate(_,_) ; true),
    (retract_accept(_) ; true),
    (retract_overlay(_,_) ; true),
    (retract_configurable(_,_,_) ; true).

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
    not(translate_region(S, _, region(["OUT"], block(400, 1500)))).

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
    not(resolves_region(S, _, region(["OUT"], block(400, 1500)))).

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

:- export test_alloc1/0.
test_alloc1 :-
    reset_static_state,
    % This example resembles the xeon phi socket on
    Size is 512 * 1024 * 1024,
    Size2M is 2 * 1024 * 1024,
    assert_accept(region(["DRAM"], block(0, Size))),
    state_empty(S0),
    state_add_free(S0, ["DRAM"], [block(0,Size)], S1),
    Reg1 = region(["DRAM"],_),
    Reg2 = region(["DRAM"],_),
    alloc(S1, Size2M, Reg1, S2),
    printf("Allocated (1): Reg=%p\nNewState=%p\n", [Reg1,S2]),
    alloc(S2, Size2M, Reg2, S3),
    printf("Allocated (2): Reg=%p\nNewState=%p\n", [Reg2,S3]).

:- export test_alloc2/0.
test_alloc2 :-
    reset_static_state,
    % This example resembles the xeon phi socket on
    Size is 512 * 1024 * 1024,
    Size2M is 2 * 1024 * 1024,
    assert_translate(region(["SOCKET"], block(0, Size)), name(["GDDR"], 0)),
    assert_translate(region(["SOCKET"], block(10000, 11000)), name(["SMPT_IN"], 0)),
    assert_configurable(["SMPT_IN"],34,["SMPT_OUT"]),
    assert_overlay(["SMPT_OUT"],["PCIBUS"]),
    assert_translate(region(["PCIBUS"], block(0, Size)), name(["DRAM"], 0)),
    assert_accept(region(["GDDR"], block(0, Size))),
    assert_accept(region(["DRAM"], block(0, Size))),
    state_empty(S0),
    state_add_free(S0, ["DRAM"], [block(0,Size)], S1),
    state_add_free(S1, ["GDDR"], [block(0,Size)], S2),
    state_add_avail(S2, ["SMPT_IN"], 32, S3),
    Reg1 = region(["GDDR"],_),
    alloc(S3, Size2M, Reg1, ["SOCKET"], S4),
    printf("Allocated (reachable from Socket): Reg=%p\nNewState=%p\n", [Reg1,S4]),
    Reg2 = region(["DRAM"],_),
    alloc(S4, Size2M, Reg2, ["SOCKET"], S5),
    printf("Allocated (reachable from Socket): Reg=%p\nNewState=%p\n", [Reg2,S5]),
    Reg3 = region(["DRAM"],_),
    alloc(S5, Size2M, Reg3, ["SOCKET"], ["PCIBUS"], S6),
    printf("Allocated (reachable from Socket and Pcibus): Reg=%p\nNewState=%p\n", [Reg3,S6]).

test_map1 :-
	% Case without
    reset_static_state,
    Size is 512 * 1024 * 1024,
    Size2M is 2 * 1024 * 1024,
    assert_translate(region(["SOCKET"], block(0, Size)), name(["GDDR"], 0)),
    assert_translate(region(["SOCKET"], block(10000, 11000)), name(["SMPT_IN"], 0)),
    assert_configurable(["SMPT_IN"],34,["SMPT_OUT"]),
    assert_overlay(["SMPT_OUT"],["PCIBUS"]),
    assert_translate(region(["PCIBUS"], block(0, Size)), name(["DRAM"], 0)),
    assert_accept(region(["GDDR"], block(0, Size))),
    assert_accept(region(["DRAM"], block(0, Size))),
    state_empty(S0),
    state_add_free(S0, ["DRAM"], [block(0,Size)], S1),
    state_add_free(S1, ["GDDR"], [block(0,Size)], S2),
    state_add_avail(S2, ["SMPT_IN"], 32, S3),

	Limit2M is Size2M - 1,
	SrcRegion = region(["SOCKET"], _),
	DstRegion = region(["GDDR"], block(0, Limit2M)),
	map(S, SrcRegion, DstRegion, S1),
	printf("Src=%p --> Dst=%p with S1=%p\n", [SrcRegion, DstRegion, S1]).

run_test(Test) :-
    (
        printf("Calling Test %p...\n", Test),
        call(Test),
        writeln(" Succeeds!")
    ) ; (
        writeln("#################################################"),
        writeln(" !!! Fails !!!"),
        writeln("#################################################")
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
    run_test(test_flat1),
    run_test(test_alloc1),
    run_test(test_alloc2).
