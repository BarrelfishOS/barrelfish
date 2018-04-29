
:- lib(ic).
:- ["decoding_net4"].



:- dynamic translate/2.
:- dynamic accept/1.
:- dynamic overlay/2.
:- dynamic node_id_next/1.
:- dynamic configurable/3.
:- dynamic node_id_node_enum/2.
:- dynamic current_state/1.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Sockeye support
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- export load_net/1.
load_net(File) :-
    ensure_loaded(File).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% State management
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
 * ===========================================================================
 * Static State
 * ===========================================================================
 */



  /*
   * ---------------------------------------------------------------------------
   * Node IDs
   * ---------------------------------------------------------------------------
   */



unused_node_enum(Enum) :-
    node_id_next(E),
    retractall(node_id_next(_)),
    Enum is E + 1,
    assert(node_id_next(Enum)).

node_enum(NodeId, Enum) :-
    node_id_node_enum(NodeId, Enum).
node_enum(NodeId, Enum) :-
    unused_node_enum(Enum),
    assert(node_id_node_enum(NodeId, Enum)).

node_enum_alias(Alias, Enum) :- assert(node_id_node_enum(Alias, Enum)).
node_enum_exists(NodeId, Enum) :-
    node_id_node_enum(NodeId, Enum).

node_enum_retract(NodeId, Enum) :-
    node_enum_exists(NodeId, Enum),
    retractall(node_id_node_enum(_, Enum)),
    retractall(node_id_node_enum(NodeId, _)).


 /*
  * ---------------------------------------------------------------------------
  * Add to state
  * ---------------------------------------------------------------------------
  */



% initializes the state to be empty
init_state :-
    assert(current_state(null)),
    assert(node_id_next(0)),
    state_empty(S),
    state_set(S).

% sets the new state
:- export state_set/1.
state_set(S) :-
    retract(current_state(_)), assert(current_state(S)).

:- export state_get/1.
state_get(S) :- current_state(S).



state_remove_suffix(State, NodeID, NewS) :-
    (findall(t(A,B), (
        translate(region(NID, D), B),
        A = region(NID, D),
        append(_, NodeID, NID)
    ), List) ; List = []),

    (foreach(t(A,B), List) do
        retract_translate(A,B)
    ),

    (findall(t(R), (
        accept(region(NID, D)),
        R = region(NID, D),
        append(_, NodeID, NID)
        ),List2); List2 = []),

    (foreach(t(R), List2) do
        retract_accept(R)
    ),
    (findall(o(A,B), (
        overlay(A, B),
        append(_, NodeID, A)
        ),List3 ); List3 = []),

    (foreach(o(A,B), List3) do
        retract_overlay(A,B)
    ),

    (findall(m(S,D), (
        state_has_mapping(State, region(SrcId, B), D),
        append(_, NodeID, SrcId),
        S = region(SrcId, B)
        ),LMappings) ; LMappings = []
    ),


    (findall(f(SrcId, Blks), (
        state_has_free(State, SrcId, Blks),
        append(_, NodeID, SrcId)
        ),LFree) ; LFree = []
    ),

    (findall(a(SrcId, C), (
        state_has_avail(State, SrcId, C),
        append(_, NodeID, SrcId)
        ),LAvail) ; LAvail = []
    ),

    (foreach(m(S,D), LMappings), fromto(State, SIn, SOut, State2) do
        state_remove_mapping(SIn, S, D, SOut)
    ),

    (foreach(f(SrcId, Blks), LFree), fromto(State2, SIn, SOut, State3) do
        state_remove_free(SIn, SrcId, Blks, SOut)
    ),

    (foreach(a(SrcId, C), LAvail), fromto(State3, SIn, SOut, NewS) do
        state_remove_avail(SIn, SrcId, C, SOut)
    ).




%%%% STATIC STATE

:- export assert_translate/4.
assert_translate(S,A,B,S) :- assert(translate(A,B)).
:- export assert_translate/2.
assert_translate(A,B) :- assert(translate(A,B)).
:- export retract_translate/2.
retract_translate(A,B) :- retractall(translate(A,B)).


:- export assert_overlay/4.
assert_overlay(S,A,B,S) :- assert(overlay(A,B)).
:- export assert_overlay/2.
assert_overlay(A,B) :- assert(overlay(A,B)).
:- export retract_overlay/2.
retract_overlay(A,B) :- retractall(overlay(A,B)).


:- export assert_accept/3.
assert_accept(S,R,SNew) :-
    assert(accept(R)),
    R = region(NodeId, B),
    state_add_free(S, NodeId, [B], SNew).

:- export assert_accept/1.
assert_accept(R) :- assert(accept(R)).
:- export retract_accept/1.
retract_accept(R) :- retractall(accept(R)).


:- export assert_configurable/5.
assert_configurable(S,SrcId,Bits,DstId,SNew) :-
    (Bits = 21 -> Slots is (512 * 512 * 512); Slots is 32),
    assert_conf_node(S,SrcId, DstId, Bits, Slots, SNew).

:- export assert_configurable/3.
assert_configurable(SrcId,Bits,DstId) :- assert(configurable(SrcId, Bits, DstId)).
:- export retract_configurable/3.
retract_configurable(SrcId,Bits,DstId) :- retractall(configurable(SrcId,Bits,DstId)).


assert_conf_node(S, InNodeId, OutNodeId, Bits, Slots, NewS) :-
    assert_configurable(InNodeId,Bits,OutNodeId),
    state_add_avail(S, InNodeId, Slots, S1),
    Limit is 2^Bits * Slots,
    state_add_free(S1, InNodeId, [block(0, Limit)], NewS).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% call the init

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% X86 Support
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This uses the memory_region facts (defined in the main module) to
% find a region above 4G that we will manage.
initial_dram_block(Block) :- %a
    % Find the usable DRAM using the existing SKB facts
    call(mem_region_type, RamType, ram)@eclipse,
    findall((Base, Size), call(memory_region,Base,_,Size,RamType,_)@eclipse, MemCandidates),
    (foreach((Base,Size), MemCandidates), fromto([], In, Out, FiltCandidates) do
        (((MinBase = 4294967296, % 4G
        MinSize = 1073741824, % 1G
        Base >= MinBase,
        Size >= MinSize) ->  Out = [(Base,Size) | In]
        ) ; (
        Out = In
        ))
    ),
    FiltCandidates = [(Base,Size) | _],
    Limit is Base + Size,
    % HACK MAKE SURE DRAM STARTS AT 16GB
    AlignBase is 16 * 1024 * 1024 * 1024,
    % ENDHACK
    Block = block(AlignBase,Limit).

% Called by Kaluga
:- export init/1.
init(NewS) :-
    init_state,
    state_empty(S1),
    add_SYSTEM(S1, [], S2),

    DRAM_ID = ["DRAM"],
    initial_dram_block(Block),
    assert_accept(region(["DRAM"], Block)),
    state_add_free(S2, ["DRAM"], [Block], NewS),
    node_enum(DRAM_ID, DRAM_ENUM),
    printf("Decoding net initialized using %p as DRAM. DRAM nodeid: %p\n",
        [Block, DRAM_ENUM]).



:- export add_process/3.
add_process(S, Enum, NewS) :-
    unused_node_enum(Enum),
    Id = [Enum],
    % The node where the process virtual addresses are issued.
    OutId = ["OUT", "PROC0" | Id],
    node_enum_alias(OutId, Enum),

    PCIBUS_ID = ["PCIBUS"],
    add_PROC_MMU(S, Id, S1),

    % Mark MMU block remappable
    MMU_IN_ID = ["IN", "MMU0" | Id],
    MMU_OUT_ID = ["OUT", "MMU0" | Id],

    NumSlots is (512 * 512 * 512),
    assert_conf_node(S1, MMU_IN_ID, MMU_OUT_ID, 21, NumSlots, S2),

    OUT_ID = ["OUT" | Id],
    assert_overlay(OUT_ID, PCIBUS_ID),

    % Reserve memory for the process, the OUT/PROC0 node is the one where
    % initially the process (virtual) addresses are issued.
    NumBlocks is (20 * 512),
    Limit is (NumBlocks * (512 * 4096) - 1),
    Size is Limit + 1,
    %state_add_in_use(S3, region(OutId, block(0,Limit)), NewS),
    alloc(S2, Size, region(MMU_IN_ID, block(0, Limit)), S3),
    state_decrement_avail(S3, MMU_IN_ID, NumBlocks, NewS).

iommu_enabled :-
    call(iommu_enabled,0,_)@eclipse.

% Addr - PCI address, should be provided by caller.
:- export remove_pci/3.
remove_pci(S, Addr, NewS) :-
    node_enum_exists(Addr, Enum),
    % remove pci_address_node lookup
    node_enum_retract(Addr, Enum),
    state_remove_suffix(S, [Enum], NewS).



%test_remove_pci :-
%    state_empty(S),
%    state_add(S, blu(3,2), S1),
%    add_pci(S1, addr(0,0,0), _, S2),
%    remove_pci(S2, addr(0,0,0), S3),
%    writeln(S3).

:- export add_xeon_phi/4.
add_xeon_phi(S, Addr, Enum, NewS) :-
    add_pci_internal(S, Addr, Enum, add_XEONPHI, add_XEONPHI_IOMMU, S1),
    %Ok, now we need to fixup the accepting bars installed by PCI.
    BAR0_ID = [0, "BAR", Enum],
    retract_accept(region(BAR0_ID, _)),
    GDDR_ID = ["GDDR", "PCI0", Enum],
    assert_overlay(BAR0_ID, GDDR_ID),
    % Make sure we have Node Enums for GDDR and Socket
    node_enum(GDDR_ID, _),
    node_enum(["KNC_SOCKET", "PCI0", Enum], _),
    node_enum(["SMPT_IN", "PCI0", Enum], _),
    node_enum(["IN", "IOMMU0", Enum], _),
    node_enum(["K1OM_CORE", "PCI0", Enum], _),
    node_enum(["DMA", "PCI0", Enum], _),


    % Reserve memory for the process, the OUT/PROC0 node is the one where
    % initially the process (virtual) addresses are issued.
    Limit is (4 * 1024 * 1024 * 1024) - 1,
    Size is Limit + 1,
    %state_add_in_use(S3, region(OutId, block(0,Limit)), NewS),
    alloc(S1, Size, region(GDDR_ID, block(0, Limit)), NewS).


:- export add_pci/4.
add_pci(S, Addr, Enum, NewS) :-
    add_pci_internal(S, Addr, Enum, add_PCI, add_PCI_IOMMU, NewS).

% Enum will be the new enum for the Phi
:- export replace_with_xeon_phi/4.
replace_with_xeon_phi(S, OldEnum, NewEnum, NewS) :-
    node_enum_exists(Addr, OldEnum),
    remove_pci(S, Addr, S1),
    add_xeon_phi(S1, Addr, NewEnum, NewS).

% Given the Pci enum of the xeon phi (as returned by the driverkit lib),
% return some other node enums.
:- export xeon_phi_meta/8.
xeon_phi_meta(_, PCI_E, KNC_SOCKET_E, SMPT_IN_E, IOMMU_IN_E, DMA_E, K1OM_CORE_E, GDDR_E) :-
    node_enum_exists([_ | Rm], PCI_E),
    node_enum_exists(["KNC_SOCKET" | Rm], KNC_SOCKET_E),
    node_enum_exists(["SMPT_IN" | Rm], SMPT_IN_E),
    node_enum_exists(["IN", "IOMMU0", PCI_E], IOMMU_IN_E),
    node_enum_exists(["DMA" | Rm], DMA_E),
    node_enum_exists(["K1OM_CORE" | Rm], K1OM_CORE_E),
    node_enum_exists(["GDDR" | Rm], GDDR_E).


% Helper to instantiate a PCI card. If no IOMMU is present it will
% instantiate Module, else ModuleIommu
add_pci_internal(S, Addr, Enum, Module, ModuleIOMMU, NewS) :-

    unused_node_enum(Enum),
    node_enum_alias(Addr, Enum),
    OutNodeId = ["OUT", "PCI0", Enum],
    node_enum_alias(OutNodeId, Enum),

    Id = [Enum],


    Addr = addr(_,_,_),
    PCIBUS_ID = ["PCIBUS"],
    PCIOUT_ID = ["OUT" | Id],
    (iommu_enabled -> (
        %add_PCI_IOMMU(S0, Id, S1),
        call(ModuleIOMMU, S, Id, S3),
        % Mark IOMMU block remappable
        IOMMU_IN_ID = ["IN", "IOMMU0" | Id],
        Slots is (512 *512 *512),
        assert_conf_node(S3, IOMMU_IN_ID, ["OUT", "IOMMU0" | Id], 21, Slots, S5)
    ) ; (
        % IOMMU disabled.
        %add_PCI(S0, Id, S2)
        call(Module, S, Id, S5)
    )),

    % connect the output to the systems pci bus
    assert_overlay(PCIOUT_ID, PCIBUS_ID),

    % Now insert the BAR into the PCI bus address space
    findall((Addr, BarNum, BarStart, BarEnd),
                %currentbar(addr(7, 18, 5), 0, 15033349584, 15033349588, 4).
            (call(currentbar(Addr, BarNum, BarStartPage, BarEndPage, _))@eclipse,
            BarStart is BarStartPage * 4096,
            BarEnd is BarEndPage * 4096),
            Bars),
    (foreach((_, BarNum, BarStart, BarEnd), Bars),
     param(Id), param(PCIBUS_ID),
     fromto(S5, SIn, SOut, NewS) do
        BarId = [BarNum, "BAR" | Id],
        InStart is BarStart,
        InEnd is BarEnd - 1,
        AcceptStart is 0,
        AcceptEnd is (BarEnd - BarStart)  - 1,
        assert_accept(region(BarId, block(AcceptStart, AcceptEnd))),
        state_add_free(SIn, BarId, block(AcceptStart, AcceptEnd), SOut),
        assert_translate(region(PCIBUS_ID, block(InStart,InEnd)),
                                                 name(BarId,AcceptStart))
    ).
    % Set it to the node id where addresses are issued from the PCI device.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Query Wrappers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% outputs a list of regions
write_regions(Regs) :-
    (foreach(Reg, Regs), foreach(Term, Terms) do
       Reg = region(NId, block(B, _)),
       node_enum_exists(NId, Enum), 
       Term = name(B, Enum)
    ),
    writeln(Terms).


mapping_confs(SrcReg, DstName, Confs) :-
    SrcReg = region(SrcId, block(SrcBase, _)),
    DstName = name(DstId, DstBase),
    configurable(SrcId, _, DstId),
    node_enum_exists(SrcId, SrcEnum),
    Confs = [c(SrcEnum, SrcBase, DstBase)].
    /*
    BlockSize is 2^Bits,
    SrcSize is SrcLimit - SrcBase + 1,
    NumBlocksEnd is SrcSize // BlockSize - 1,
    BaseVPN is SrcBase // BlockSize,
    BasePPN is DstBase // BlockSize,
    (for(I, 0, NumBlocksEnd), param(BaseVPN), param(BasePPN), param(BlockSize),
     param(SrcId), foreach(c(SrcEnum, In, Out), Confs) do
        VPN is BaseVPN + I,
        PPN is BasePPN + I,
        In is BlockSize * VPN,
        Out is BlockSize * PPN,
        node_enum(SrcId, SrcEnum)
    ). */

write_conf_update(state(M0,_,_), state(M1, _, _)) :-
    subtract(M1, M0, MDelta),
    (foreach(mapping(SrcReg,DstName), MDelta), fromto([],InT,OutT,Terms) do
        mapping_confs(SrcReg, DstName, Confs),
        append(InT, Confs, OutT)
    ),
    writeln(Terms).


:- export alloc_wrap/6.
alloc_wrap(S, Size, _, DestEnum, SrcEnums, NewS) :-
    (foreach(Enum, SrcEnums), foreach(SrcId, SrcIds) do
        node_enum_exists(SrcId, Enum)
    ),
    node_enum_exists(DestId, DestEnum),
    DestReg = region(DestId, _),
    (
        SrcIds = [R1,R2], alloc(S, Size, DestReg, R1, R2, NewS) ;
        SrcIds = [R1], alloc(S, Size, DestReg, R1, NewS) ;
        SrcIds = [], alloc(S, Size, DestReg, NewS)
    ),
    write_regions([DestReg]).

:- export map_wrap/7.
map_wrap(S0, Size, _, DestEnum, DestAddr, SrcEnums, NewS)  :-
    % Set up DestReg
    node_enum_exists(DestId, DestEnum),
    Limit is DestAddr + Size - 1,
    DestReg = region(DestId, block(DestAddr, Limit)),
    (foreach(Enum, SrcEnums), fromto(S0, SIn, SOut, NewS),foreach(SrcReg,SrcRegs),
     param(DestReg) do
        node_enum_exists(SrcId, Enum),
        SrcReg = region(SrcId, _),
        map(SIn, SrcReg, DestReg, SOut)
    ),
    append([DestReg], SrcRegs, OutputRegs),
    write_regions(OutputRegs),
    write_conf_update(S0, NewS).


:- export reverse_resolve_wrap/5.
%reverse_resolve_wrap(S0, DstEnum, DstAddr, DstSize, SrcEnum)  :-
reverse_resolve_wrap(S0, DstEnum, DstAddr, _, SrcEnum)  :-
    node_enum_exists(DstNodeId, DstEnum),
    node_enum_exists(SrcNodeId, SrcEnum),
    %DstLimit is DstSize + DstAddr - 1,
    %DstRegion = region(DstNodeId, block(DstAddr, DstLimit)),
    %SrcRegion = region(SrcNodeId, _),

    DstName = name(DstNodeId, DstAddr),
    resolve_name(S0, DstName, ResolvedDstName),
    SrcName = name(SrcNodeId, SrcAddr),
    resolve_name(S0, SrcName, ResolvedDstName),

    writeln([name(SrcAddr, SrcEnum)]).


:- export alias_conf_wrap/6.
alias_conf_wrap(S0, SrcEnum, SrcAddr, Size, DstEnum, NewS)  :-
    % Build src region
    node_enum_exists(SrcId, SrcEnum),
    SrcLimit is SrcAddr + Size - 1,
    SrcRegion = region(SrcId, block(SrcAddr, SrcLimit)),

    node_enum_exists(DstId, DstEnum),
    DstRegion = region(DstId, _),
    alias_conf(S0, SrcRegion, DstRegion, Confs),
    state_add_confs(S0, Confs, S1),

    state_add_in_use(S1, DstRegion, NewS),

    write_regions([DstRegion]),
    write_confs(NewS, Confs).

:- export xeon_phi_meta_wrap/2.
xeon_phi_meta_wrap(S, PCI_E) :-
    xeon_phi_meta(S, PCI_E, E1, E2, E3, E4, E5, E6),
    printf("%p %p %p %p %p %p\n", [E1, E2, E3, E4, E5, E6]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Debug
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listing_term(S) :- write("    "), write(S), writeln(",").

:- export decoding_net_listing/0.
decoding_net_listing :-
    state_get(S),
    S = state(A, M, O, BM, BC, U, E, P, V),
    writeln("state(S) :- S = state("),
    writeln("  ["),
    checklist(listing_term, A),
    writeln("  ], "),
    writeln("  ["),
    checklist(listing_term, M),
    writeln("  ], "),
    writeln("  ["),
    checklist(listing_term, O),
    writeln("  ], "),
    writeln("  ["),
    checklist(listing_term, BM),
    writeln("  ], "),
    writeln("  ["),
    checklist(listing_term, BC),
    writeln("  ], "),
    writeln("  ["),
    checklist(listing_term, U),
    writeln("  ], "),
    writeln("  ["),
    checklist(listing_term, E),
    writeln("  ], "),
    writeln("  ["),
    checklist(listing_term, P),
    writeln("   ], "),
    writeln("   ["),
    checklist(listing_term, V),
    writeln("   ]"),
    writeln(")").
