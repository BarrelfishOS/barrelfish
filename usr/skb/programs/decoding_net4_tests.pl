% load the decoding net
:- ["decoding_net4_support"].
:- lib(listut).

state_equal(state(M0,F0,A0),state(M1,F1,A1)) :-
    perm(M0,M1),
    perm(F0,F1),
    perm(A0,A1).

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
    Size512M is 512 * 1024 * 1024,
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
    state_empty(S0),
    state_add_free(S0, ["SOCKET"], [block(Offset,OffsetLimit)], S1),

    assert_translate(region(["SOCKET"], block(10000, 11000)), name(["SMPT_IN"], 0)),
    assert_overlay(["SMPT_OUT"],["PCIBUS"]),
    assert_translate(region(["PCIBUS"], block(0, Size)), name(["DRAM"], 0)),
    assert_accept(region(["GDDR"], block(0, Size))),
    assert_accept(region(["DRAM"], block(0, Size))),
    assert_conf_node(S1, ["SMPT_IN"],["SMPT_OUT"], 34, 32, S2),
    state_add_free(S2, ["DRAM"], [block(0,Size)], S3),
    state_add_free(S3, ["GDDR"], [block(0,Size)], S4),
    Limit2M is Size2M - 1,
    SrcRegion = region(["SOCKET"], _),
    DstRegion = region(["GDDR"], block(0, Limit2M)),
    map(S4, SrcRegion, DstRegion, S5),
    printf("Src=%p --> Dst=%p with S1=%p\n", [SrcRegion, DstRegion, S5]).

test_map1_nochoice :-
    findall(dummy, test_map1, Li),
    length(Li, 1).



test_map2 :-
    % Case with a node configuration necessary, the translated node fits in
    % the size of the remapped nodes.
    reset_static_state,
    Size is 512 * 1024 * 1024,
    Size2M is 2 * 1024 * 1024,
    Offset is 16 * 1024 * 1024,
    OffsetLimit is Offset + Size,
    %assert_translate(region(["SOCKET"], block(Offset, OffsetLimit)), name(["GDDR"], 0)),
    state_empty(S0),
    state_add_free(S0, ["SOCKET"], [block(Offset,OffsetLimit)], S1),
    assert_translate(region(["SOCKET"], block(Offset, OffsetLimit)), name(["SMPT_IN"], 0)),
    assert_overlay(["SMPT_OUT"],["PCIBUS"]),
    assert_translate(region(["PCIBUS"], block(0, Size)), name(["DRAM"], 0)),
    assert_accept(region(["GDDR"], block(0, Size))),
    assert_accept(region(["DRAM"], block(0, Size))),
    assert_conf_node(S1, ["SMPT_IN"],["SMPT_OUT"], 34, 32, S2),
    state_add_free(S2, ["DRAM"], [block(0,Size)], S3),
    state_add_free(S3, ["GDDR"], [block(0,Size)], S4),

    Limit2M is Size2M - 1,
    SrcRegion = region(["SOCKET"], _),
    DstRegion = region(["DRAM"], block(0, Limit2M)),
    map(S4, SrcRegion, DstRegion, S5),
    printf("Src=%p --> Dst=%p with NewS=%p\n", [SrcRegion, DstRegion, S5]).

test_map3 :-
    % Case with a node configuration necessary, the translated node spans
    % multiple remapped nodes.
    reset_static_state,
    Size is 512 * 1024 * 1024,
    Size2M is 2 * 1024 * 1024,
    Offset is 16 * 1024 * 1024,
    OffsetLimit is Offset + Size,
    state_empty(S0),
    assert_accept_node(S0, region(["DRAM"], block(0, Size)), S1),
    assert_vspace_node(S1, region(["IN"], block(0,Size)), name(["MMU"], 0), S2),
    assert_conf_node(S2, ["MMU"],["DRAM"], 21, 1024, S3),

    Limit8M is 8 * 1024 * 1024,
    SrcRegion = region(["IN"], _),
    DstRegion = region(["DRAM"], block(0, Limit8M)),
    findall((A,B,C), flat(A,B,C), Li),
    (foreach((A,B,C), Li) do
        printf("flat(%p,%p,%p)\n", [A,B,C])
    ),
    map(S3, SrcRegion, DstRegion, S4),
    printf("Src=%p --> Dst=%p with NewS=%p\n", [SrcRegion, DstRegion, S4]).

test_map4 :-
    reset_static_state,
    % Case with a node configuration necessary that passes two configurable
    % nodes.
    Size is 512 * 1024 * 1024,
    Size2M is 2 * 1024 * 1024,
    Offset is 16 * 1024 * 1024,
    OffsetLimit is Offset + Size,
    state_empty(S0),
    assert_accept_node(S0, region(["DRAM"], block(0, Size)),S1),
    assert_conf_node(S1, ["SMPT_IN"],["IOMMU_IN"], 34, 32, S2),
    assert_conf_node(S2, ["IOMMU_IN"],["DRAM"], 21, 1024, S3),
    assert_vspace_node(S3, region(["IN"], block(0,Size)), name(["SMPT_IN"], 0), S4),

    Limit8M is 8 * 1024 * 1024 - 1,
    SrcRegion = region(["IN"], _),
    DstRegion = region(["DRAM"], block(0, Limit8M)),
    findall((A,B,C), flat(A,B,C), Li),
    map(S4, SrcRegion, DstRegion, S5),
    printf("Src=%p --> Dst=%p with NewS=%p\n", [SrcRegion, DstRegion, S4]),
    write_conf_update(S4,S5).

test_map_wrap :-
    reset_static_state,
    assert_accept(region(["DRAM"], block(0, Size))),
    state_empty(S0),
    Limit is 512 * 1024 *1024 *1024,
    assert_vspace_node(S0, region(["IN"],block(0,Limit)), name(["SMPT_IN"], 0), S1),
    assert_conf_node(S1, ["SMPT_IN"],["IOMMU_IN"], 34, 32, S2),
    assert_conf_node(S2, ["IOMMU_IN"],["DRAM"], 21, 1024, S3),

    Size2M is 2 * 1024 * 1024,
    node_enum(["DRAM"], DramEnum),
    node_enum(["IN"], SrcEnum),
    map_wrap(S3, Size2M, 21, DramEnum, 0, [SrcEnum], S4).

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

test_add_vm :-
    reset_static_state,
    state_get(S),
    node_enum(["DRAM"], DramEnum),
    Limit2G is 2 * 1024 * 1024 * 1024 - 1,
    Blk = block(0, Limit2G),
    assert_accept(region(["DRAM"], Blk)),
    state_add_free(S, ["DRAM"], [Blk], S1),
    Limit1G is 1024 * 1024 * 1024 - 1,
    DramBase = 1024 * 1024 * 1024,
    DramLimit = 1024 * 1024 * 1024 - 1,
    add_vm_overlay(S1, VmEnum, 0, Limt1G, DramEnum, DramBase, NewS),
    printf("VmEnum %p\n", VmEnum).


% Make sure this does not create backtracking behaviour.
test_freelist_nochoice :-
    findall(X,free_list_insert([block(0,19),block(50,100)], block(20,49), X),Li),
    length(Li, 1).

% Functional correctnes
test_freelist2 :-
    free_list_insert([block(0,10)], block(11,20), [block(0,20)]),
    free_list_insert([block(0,10),block(50,100)], block(20,30), [block(0,10),block(20,30), block(50,100)]),
    free_list_insert([block(0,19),block(50,100)], block(20,49), [block(0,100)]),
    free_list_insert([block(50,100)], block(200,210), [block(50,100), block(200,210)]),
    free_list_insert([block(50,100)], block(0,10), [block(0,10), block(50,100)]),
    free_list_insert([block(50,100)], block(40,49), [block(40,100)]),
    findall(R, free_list_allocated([block(0,10),block(20,30), block(50,100)], 0, 100, R), RLi),
    printf("Free list allocated %p\n", [RLi]).


test_alloc_free :-
    reset_static_state,
    state_get(S),
    node_enum(["DRAM"], DramEnum),
    Limit2G is 2 * 1024 * 1024 * 1024 - 1,
    Blk = block(0, Limit2G),
    assert_accept(region(["DRAM"], Blk)),
    state_add_free(S, ["DRAM"], [Blk], S1),
    Reg1 = region(["DRAM"],  _),
    Reg2 = region(["DRAM"],  _),
    Reg3 = region(["DRAM"],  _),
    Reg4 = region(["DRAM"],  _),
    Reg5 = region(["DRAM"],  _),
    Reg6 = region(["DRAM"],  _),
    Size1M is 1024 * 1024,
    Size2M is 2 * 1024 * 1024,
    alloc(S1, Size1M, Reg1, S2),
    printf("Reg1 = %p\n", [Reg1]),
    alloc(S2, Size1M, Reg2, S3),
    printf("Reg2 = %p\n", [Reg2]),
    alloc(S3, Size1M, Reg3, S4),
    printf("Reg3 = %p\n", [Reg3]),
    alloc(S4, Size1M, Reg4, S5),
    printf("Reg4 = %p\n", [Reg4]),
    alloc(S5, Size1M, Reg5, S6),
    printf("Reg5 = %p\n", [Reg5]),
    free(S6, Reg2, S7),
    free(S7, Reg4, S8),
    alloc(S8, Size2M, Reg6, S9),
    printf("Reg6 = %p\n", [Reg6]),
    state_has_free(S9, ["DRAM"], FreeBlks),
    findall(R, free_list_allocated(FreeBlks, 0, Limit2G, R), RLi),
    printf("Free list allocated %p\n", [RLi]).

test_alloc_specific :-
    reset_static_state,
    state_get(S),
    node_enum(["DRAM"], DramEnum),
    Limit2G is 2 * 1024 * 1024 * 1024 - 1,
    Blk = block(0, Limit2G),
    assert_accept(region(["DRAM"], Blk)),
    state_add_free(S, ["DRAM"], [Blk], S1),
    
    Base is 10 * 1024 * 1024,
    Limit is 12 * 1024 * 1024 - 1,
    Reg1 = region(["DRAM"],  block(Base, Limit)),
    region_size(Reg1, Reg1Size),
    alloc(S1, Reg1Size, Reg1, S2),
    printf("alloc specific Reg1=%p, S2=%p\n", [Reg1, S2]).

test_unmap_setup(NewS) :-
    reset_static_state,
    Size is 512 * 1024 * 1024,
    Size2M is 2 * 1024 * 1024,
    Offset is 16 * 1024 * 1024,
    OffsetLimit is Offset + Size,
    assert_accept(region(["DRAM"], block(0, Size))),
    state_empty(S0),
    assert_conf_node(S0, ["MMU0"],["DRAM"], 21, 1024, S1),
    assert_conf_node(S1, ["MMU1"],["DRAM"], 21, 1024, S2),
    state_add_free(S2, ["DRAM"], [block(0,Size)], S3),

    state_add_free(S3, ["PROC0"], [block(0,Size)], S4),
    state_add_free(S4, ["PROC1"], [block(0,Size)], S5),
    assert_translate(S5, region(["PROC0"], block(0, Size)), name(["MMU0"], 0), S6),
    assert_translate(S6, region(["PROC1"], block(0, Size)), name(["MMU1"], 0), NewS).

% one map and followed by one unmap keeps the state intact
test_unmap1 :-
    test_unmap_setup(S4),
    Size8M is 8 * 1024 * 1024,
    SrcRegion = region(["PROC0"], _),
    DstRegion = region(["DRAM"], _),
    alloc(S4, Size8M, DstRegion, S5), 
    %printf("Allocated Src=%p\n", [DstRegion]),
    map(S5, SrcRegion, DstRegion, S6),
    %printf("Mapped Src=%p --> Dst=%p with NewS=%p\n", [SrcRegion, DstRegion, S6]),
    installed_vspace_map(S6, SrcRegion),
    unmap(S6, [SrcRegion], DstRegion, S7),
    %printf("Unmapped Src, NewS=%p\n", [S7]),
    state_equal(S5, S7).

test_unmap1_nochoice :-
    test_unmap_setup(S4),
    Size8M is 8 * 1024 * 1024,
    SrcRegion = region(["PROC0"], _),
    DstRegion = region(["DRAM"], _),
    alloc(S4, Size8M, DstRegion, S5), 
    %printf("Allocated Src=%p\n", [DstRegion]),
    map(S5, SrcRegion, DstRegion, S6),
    %printf("Mapped Src=%p --> Dst=%p with NewS=%p\n", [SrcRegion, DstRegion, S6]),
    installed_vspace_map(S6, SrcRegion),
    findall(S7, unmap(S6, [SrcRegion], DstRegion, S7), Li),
    length(L1, 1).

%  3 maps into same space followed by one unmap keeps the state intact
test_unmap2 :-
    test_unmap_setup(S4),
    Size8M is 8 * 1024 * 1024,
    Size6M is 6 * 1024 * 1024,
    SrcRegion1 = region(["PROC0"], _),
    SrcRegion2 = region(["PROC0"], _),
    DstRegion1 = region(["DRAM"], _),
    DstRegion2 = region(["DRAM"], _),
    DstRegion3 = region(["DRAM"], _),
    alloc(S4, Size8M, DstRegion1, S5), 
    alloc(S5, Size8M, DstRegion2, S6), 
    alloc(S6, Size6M, DstRegion3, S7), 
    printf("Dest1=%p,  Dest2=%p, Dest3=%p\n", [DstRegion1, DstRegion2, DstRegion3]),
    map(S7, SrcRegion1, DstRegion1, S8),
    map(S8, SrcRegion2, DstRegion2, S9),
    map(S9, SrcRegion3, DstRegion3, S10),
    unmap(S10, [SrcRegion1], DstRegion1, S11),
    unmap(S11, [SrcRegion2], DstRegion2, S12),
    unmap(S12, [SrcRegion3], DstRegion3, S13),
    state_equal(S7, S13).

test_unmap3 :-
    test_unmap_setup(S4),
    Size8M is 8 * 1024 * 1024,
    Size6M is 6 * 1024 * 1024,
    SrcRegion1 = region(["PROC0"], _),
    SrcRegion2 = region(["PROC1"], _),
    DstRegion = region(["DRAM"], _),
    alloc(S4, Size8M, DstRegion, S5), 
    map(S5, SrcRegion1, DstRegion, S6),
    map(S6, SrcRegion2, DstRegion, S7),
    unmap(S7, [SrcRegion1,SrcRegion2], DstRegion, S8),
    printf("S5=%p\nS7=%p\n", [S5,S8]),
    state_equal(S5, S8).


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
    run_test(test_add_process),
    run_test(test_add_vm),
    run_test(test_freelist_nochoice),
    run_test(test_freelist2),
    run_test(test_alloc_free),
    run_test(test_unmap1),
    run_test(test_unmap1_nochoice),
    run_test(test_unmap2),
    run_test(test_unmap3).

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

