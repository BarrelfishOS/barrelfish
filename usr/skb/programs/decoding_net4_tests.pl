% load the decoding net
:- ["decoding_net4_support"].

reset_static_state :-
    retractall(translate(_,_)),
    retractall(accept(_)),
    retractall(overlay(_,_)),
    retractall(node_id_next(_)),
    retractall(configurable(_,_,_)),
    retractall(node_id_node_enum(_,_)),
    retractall(current_state(_)),
    retract_translate(_,_),
    retract_accept(_),
    retract_overlay(_,_),
    retract_configurable(_,_,_),
    init_state.

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

:- export dump_flat/0.
dump_flat :-
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
	% Case without a node reconfiguration necessary
    reset_static_state,
    Size is 512 * 1024 * 1024,
    Size2M is 2 * 1024 * 1024,
    Offset is 16 * 1024 * 1024,
    OffsetLimit is Offset + Size,
    assert_translate(region(["SOCKET"], block(Offset, OffsetLimit)), name(["GDDR"], 0)),
    assert_translate(region(["SOCKET"], block(10000, 11000)), name(["SMPT_IN"], 0)),
    assert_overlay(["SMPT_OUT"],["PCIBUS"]),
    assert_translate(region(["PCIBUS"], block(0, Size)), name(["DRAM"], 0)),
    assert_accept(region(["GDDR"], block(0, Size))),
    assert_accept(region(["DRAM"], block(0, Size))),
    state_empty(S0),
    assert_conf_node(S0, ["SMPT_IN"],["SMPT_OUT"], 34, 32, S1),
    state_add_free(S1, ["DRAM"], [block(0,Size)], S2),
    state_add_free(S2, ["GDDR"], [block(0,Size)], S3),
	Limit2M is Size2M - 1,
	SrcRegion = region(["SOCKET"], _),
	DstRegion = region(["GDDR"], block(0, Limit2M)),
	map(S3, SrcRegion, DstRegion, S4),
	printf("Src=%p --> Dst=%p with S1=%p\n", [SrcRegion, DstRegion, S4]).



test_map2 :-
	% Case with a node configuration necessary, the translated node fits in
    % the size of the remapped nodes.
    reset_static_state,
    Size is 512 * 1024 * 1024,
    Size2M is 2 * 1024 * 1024,
    Offset is 16 * 1024 * 1024,
    OffsetLimit is Offset + Size,
    %assert_translate(region(["SOCKET"], block(Offset, OffsetLimit)), name(["GDDR"], 0)),
    assert_translate(region(["SOCKET"], block(Offset, OffsetLimit)), name(["SMPT_IN"], 0)),
    assert_overlay(["SMPT_OUT"],["PCIBUS"]),
    assert_translate(region(["PCIBUS"], block(0, Size)), name(["DRAM"], 0)),
    assert_accept(region(["GDDR"], block(0, Size))),
    assert_accept(region(["DRAM"], block(0, Size))),
    state_empty(S0),
    assert_conf_node(S0, ["SMPT_IN"],["SMPT_OUT"], 34, 32, S1),
    state_add_free(S1, ["DRAM"], [block(0,Size)], S2),
    state_add_free(S2, ["GDDR"], [block(0,Size)], S3),

	Limit2M is Size2M - 1,
	SrcRegion = region(["SOCKET"], _),
	DstRegion = region(["DRAM"], block(0, Limit2M)),
	map(S3, SrcRegion, DstRegion, S4),
	printf("Src=%p --> Dst=%p with NewS=%p\n", [SrcRegion, DstRegion, S4]).

test_map3 :-
	% Case with a node configuration necessary, the translated node spans
    % multiple remapped nodes.
    reset_static_state,
    Size is 512 * 1024 * 1024,
    Size2M is 2 * 1024 * 1024,
    Offset is 16 * 1024 * 1024,
    OffsetLimit is Offset + Size,
    assert_accept(region(["DRAM"], block(0, Size))),
    state_empty(S0),
    assert_conf_node(S0, ["MMU"],["DRAM"], 21, 1024, S1),
    state_add_free(S1, ["DRAM"], [block(0,Size)], S2),

	Limit8M is 8 * 1024 * 1024,
	SrcRegion = region(["MMU"], _),
	DstRegion = region(["DRAM"], block(0, Limit8M)),
    findall((A,B,C), flat(A,B,C), Li),
    (foreach((A,B,C), Li) do
        printf("flat(%p,%p,%p)\n", [A,B,C])
    ),
	map(S2, SrcRegion, DstRegion, S3),
	printf("Src=%p --> Dst=%p with NewS=%p\n", [SrcRegion, DstRegion, S3]).

test_map4 :-
    reset_static_state,
	% Case with a node configuration necessary that passes two configurable
    % nodes.
    Size is 512 * 1024 * 1024,
    Size2M is 2 * 1024 * 1024,
    Offset is 16 * 1024 * 1024,
    OffsetLimit is Offset + Size,
    assert_accept(region(["DRAM"], block(0, Size))),
    state_empty(S0),
    assert_conf_node(S0, ["SMPT_IN"],["IOMMU_IN"], 34, 32, S1),
    assert_conf_node(S1, ["IOMMU_IN"],["DRAM"], 21, 1024, S2),

    Limit8M is 8 * 1024 * 1024 - 1,
	SrcRegion = region(["SMPT_IN"], _),
	DstRegion = region(["DRAM"], block(0, Limit8M)),
    findall((A,B,C), flat(A,B,C), Li),
	map(S2, SrcRegion, DstRegion, S3),
	printf("Src=%p --> Dst=%p with NewS=%p\n", [SrcRegion, DstRegion, S3]),
    write_conf_update(S2,S3).

test_map_wrap :-
    reset_static_state,
    assert_accept(region(["DRAM"], block(0, Size))),
    state_empty(S0),
    assert_conf_node(S0, ["SMPT_IN"],["IOMMU_IN"], 34, 32, S1),
    assert_conf_node(S1, ["IOMMU_IN"],["DRAM"], 21, 1024, S2),

    Size2M is 2 * 1024 * 1024,
    node_enum(["DRAM"], DramEnum),
    node_enum(["SMPT_IN"], SrcEnum),
    map_wrap(S2, Size2M, 21, DramEnum, 0, [SrcEnum], S3).

test_alloc_wrap :-
    reset_static_state,
    assert_accept(region(["DRAM"], block(0, Size))),
    state_empty(S0),
    state_add_free(S0, ["DRAM"], [block(0,Size)], S1),
    Size2M is 2 * 1024 * 1024,
    node_enum(["DRAM"], DramEnum),
    alloc_wrap(S1, Size2M, 21, DramEnum, [], S2).

test_add_process :-
    reset_static_state,
    init(S0),
    add_process(S0, E1, S1),
    printf("ProcEnum=%p, State=%p\n", [E1, S1]),
    node_enum(["DRAM"], DramEnum),
    Size2M is 2 * 1024 * 1024,
    alloc_wrap(S1, Size2M, 21, DramEnum, [E1], S2).

test_add_xeon_phi :-
    reset_static_state,
    state_empty(S0),
    add_xeon_phi(S0, addr(10,0,0), E1, S1),
    printf("XPhiEnum=%p, State=%p\n", [E1]),
    add_process(S1, E1, S2),
    printf("ProcEnum=%p, State=%p\n", [E1]),
    xeon_phi_meta(S2, E1, KNC_SOCKET_E, _, _, _, GDDR_E),
    Size = 2 * 1024 * 1024,
    printf("before allocation"),
    alloc_wrap(S2, Size, _, GDDR_E, [KNC_SOCKET_E]).


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
    run_test(test_alloc2),
    run_test(test_map1),
    run_test(test_map2),
    run_test(test_map3),
    run_test(test_map4),
    run_test(test_map_wrap),
    run_test(test_alloc_wrap),
    run_test(test_add_process).

/*
 *-----------------
 * Benchmarks
 * ----------------
 */

bench_init(E1, E2, NewS) :-
    reset_static_state,
    init_state,
    init(S0),
    add_pci(S0, addr(0,0,0), E1, S1),
    add_pci(S1, addr(0,0,1), E2, NewS).

% Benchmark allocation time with increasing number of nodes.
bench_nodes_one(NumPci) :-
    bench_init(E1,E2, S0),
    (fromto(S0, SIn, SOut, S1), for(I,2,NumPci) do 
        add_pci(SIn, addr(0,0,I),_, SOut)
    ),

    % setup for alloc
    Size2M is 2097152,
    DestReg = region(["DRAM"],_),
    node_enum(N1, E1),
    not(N1 = addr(_,_,_)),
    node_enum(N2, E2),
    not(N2 = addr(_,_,_)),
    statistics(hr_time, Start),
    % and go
    alloc(S1, Size2M, DestReg, N1, N2, NewS),
    statistics(hr_time, Stop),
    Diff is Stop - Start,
    printf("%p %p\n", [NumPci, Diff]).

bench_nodes(MaxNumPci) :-
    writeln("===== NODES ALLOC BENCH START ====="),
    (for(I,2,MaxNumPci) do 
        bench_nodes_one(I), !
    ).

% Increasing number of allocations on a real system
bench_real_ram_alloc(NumAllocs) :-
    node_enum(addr(_,_,_), E1),
    node_enum(N1, E1),
    not(N1 = addr(_,_,_)),

    node_enum(addr(_,_,_), E1),
    node_enum(N2, E2),
    not(N2 = addr(_,_,_)),
    not(N2 = N1),

    Dest = ["DRAM"],

    printf("Determined Dest=%p Src=%p,%p for ram alloc bench\n",
        [Dest, N1, N2]),
    writeln("===== REAL RAM ALLOC BENCH START ====="),
    (for(I,0,NumAllocs), param(Dest), param(N1), param(N2) do 
        Size2M is 2097152,
        DestReg = region(Dest, _),
        N1Reg = region(N1, _),
        N2Reg = region(N2, _),

        state_get(S),
        statistics(hr_time, Start),
        alloc(S, Size2M, DestReg, N1, N2, NewS),
        statistics(hr_time, Stop),
        state_set(NewS),
        Diff is Stop - Start,
        %printf("Allocated %p\n", [DestReg]),
        printf("%p %p\n", [I, Diff])
    ).


% RUN ALL SYNTHETIC BENCHMARKS. Resets state, breaks BF if run on real system.
bench_synth :-
    bench_nodes(100).

% RUN ALL REAL BENCHMARS: Expects KNC system has been instantiated
% does not reset state.
bench_real :- 
    bench_real_ram_alloc(1000).

