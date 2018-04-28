
:- lib(ic).
:- ["decoding_net4"].


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

:- dynamic node_id_next/1.
node_id_next(0).

unused_node_enum(Enum) :-
    node_id_next(E),
    retractall(node_id_next(_)),
    Enum is E + 1,
    assert(node_id_next(Enum)).

:- dynamic node_id_node_enum/2.
node_enum(Enum, NodeId) :-
    node_id_node_enum(NodeId, Enum).
node_enum(Enum, NodeId) :-
    unused_node_enum(Enum),
    assert(node_id_node_enum(NodeId, Enum)).

node_enum_exists(Enum, NodeId) :-
    node_id_node_enum(NodeId, Enum).

 /*
  * ---------------------------------------------------------------------------
  * Add to state
  * ---------------------------------------------------------------------------
  */

:- dynamic current_state/1.

% initializes the state to be empty
init_state :-
    assert(current_state(null)),
    state_empty(S),
    state_set(S).

% sets the new state
:- export state_set/1.
state_set(S) :-
    retract(current_state(_)), assert(current_state(S)).

:- export state_get/1.
state_get(S) :- current_state(S).




%%%% STATIC STATE
:- dynamic translate/2.
:- export assert_translate/4.
assert_translate(S,A,B,S) :- assert(translate(A,B)).
:- export assert_translate/2.
assert_translate(A,B) :- assert(translate(A,B)).
:- export retract_translate/2.
retract_translate(A,B) :- retractall(translate(A,B)).

:- dynamic overlay/2.
:- export assert_overlay/4.
assert_overlay(S,A,B,S) :- assert(overlay(A,B)).
:- export assert_overlay/2.
assert_overlay(A,B) :- assert(overlay(A,B)).
:- export retract_overlay/2.
retract_overlay(A,B) :- retractall(overlay(A,B)).

:- dynamic accept/1.
:- export assert_accept/3.
assert_accept(S,R,SNew) :-
    assert(accept(R)),
    R = region(NodeId, B),
    state_add_free(S, NodeId, [B], SNew).

:- export assert_accept/1.
assert_accept(R) :- assert(accept(R)).
:- export retract_accept/1.
retract_accept(R) :- retractall(accept(R)).

:- dynamic configurable/3.
:- export assert_configurable/5.
assert_configurable(S,SrcId,Bits,DstId,S) :- assert(configurable(SrcId, Bits, DstId)).
:- export assert_configurable/3.
assert_configurable(SrcId,Bits,DstId) :- assert(configurable(SrcId, Bits, DstId)).
:- export retract_configurable/3.
retract_configurable(SrcId,Bits,DstId) :- retractall(configurable(SrcId,Bits,DstId)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% call the init
:- init_state.

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
    state_empty(S1),
    add_SYSTEM(S1, [], S2),
    assert_accept(region(["DRAM"], Block)),

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
    node_enum(OutId, Enum),

    DRAM_ID = ["DRAM"],
    add_PROC_MMU(S, Id, S1),

    % Mark MMU block remappable
    MMU_IN_ID = ["IN", "MMU0" | Id],
    MMU_OUT_ID = ["OUT", "MMU0" | Id],
    state_add_block_meta(S1, MMU_IN_ID, 21, MMU_OUT_ID, S2), % Make MMU configurable
    OUT_ID = ["OUT" | Id],
    state_add_overlay(S2, OUT_ID, DRAM_ID, S3),
    % Reserve memory for the process, the OUT/PROC0 node is the one where
    % initially the process (virtual) addresses are issued.
    Limit is 10995116277760 - 1, % (512 << 31) - 1
    state_add_in_use(S3, region(OutId, block(0,Limit)), NewS).

iommu_enabled :-
    call(iommu_enabled,0,_)@eclipse.

% Addr - PCI address, should be provided by caller.
:- export remove_pci/3.
remove_pci(S, Addr, NewS) :-
    state_has_pci_id(S, Addr, Enum),
    % remove pci_address_node lookup
    state_remove_pci_id(S, Addr, Enum, S1),
    state_remove_suffix(S1, [Enum], NewS).

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
    state_remove_accept(S1, region(BAR0_ID, _), S2),
    GDDR_ID = ["GDDR", "PCI0", Enum],
    state_add_overlay(S2, BAR0_ID, GDDR_ID, NewS),
    % Make sure we have Node Enums for GDDR and Socket
    node_enum(GDDR_ID, _),
    node_enum(["KNC_SOCKET", "PCI0", Enum], _),
    node_enum(["SMPT_IN", "PCI0", Enum], _),
    node_enum(["IN", "IOMMU0", Enum], _),
    node_enum(["K1OM_CORE", "PCI0", Enum], _),
    node_enum(["DMA", "PCI0", Enum], _).

%    state_remove_pci_id(S3, Addr, _, S4),
%    K1OMCoreId = ["K1OM_CORE", "PCI0", Enum],
    %state_add_pci_id(S4, Addr, K1OMCoreId, NewS).

:- export add_pci/4.
add_pci(S, Addr, Enum, NewS) :-
    add_pci_internal(S, Addr, Enum, add_PCI, add_PCI_IOMMU, NewS).

% Enum will be the new enum for the Phi
:- export replace_with_xeon_phi/4.
replace_with_xeon_phi(S, OldEnum, NewEnum, NewS) :-
    state_has_pci_id(S, Addr, OldEnum),
    remove_pci(S, Addr, S1),
    add_xeon_phi(S1, Addr, NewEnum, NewS).

% Given the Pci enum of the xeon phi (as returned by the driverkit lib),
% return some other node enums.
:- export xeon_phi_meta/7.
xeon_phi_meta(S, PCI_E, KNC_SOCKET_E, SMPT_IN_E, IOMMU_IN_E, DMA_E, K1OM_CORE_E) :-
    node_enum(S, [_ | Rm], PCI_E, S),
    node_enum(S, ["KNC_SOCKET" | Rm], KNC_SOCKET_E, S),
    node_enum(S, ["SMPT_IN" | Rm], SMPT_IN_E, S),
    node_enum(S, ["IN", "IOMMU0", PCI_E], IOMMU_IN_E, S),
    node_enum(S, ["DMA" | Rm], DMA_E, S),
    node_enum(S, ["K1OM_CORE" | Rm], K1OM_CORE_E, S).

% Helper to instantiate a PCI card. If no IOMMU is present it will
% instantiate Module, else ModuleIommu
add_pci_internal(S, Addr, Enum, Module, ModuleIOMMU, NewS) :-
    unused_node_enum(Enum),
    Id = [Enum],
    OutNodeId = ["OUT", "PCI0", Enum],
    node_enum(S, OutNodeId, Enum, S0),
    Limit512G is 512 * 1024 * 1024 * 1024 - 1,

    Addr = addr(_,_,_),
    PCIBUS_ID = ["PCIBUS"],
    PCIOUT_ID = ["OUT" | Id],
    (iommu_enabled -> (
        %add_PCI_IOMMU(S0, Id, S1),
        call(ModuleIOMMU, S0, Id, S1),
        % Mark IOMMU block remappable
        IOMMU_IN_ID = ["IN", "IOMMU0" | Id],
        state_add_block_meta(S1, IOMMU_IN_ID, 21, ["OUT", "IOMMU0" | Id], S2),
        state_add_in_use(S2, region(IOMMU_IN_ID, block(0, Limit512G)), S3),
        % This is not strictly necessary, but it will speedup the allocations
        state_add_in_use(S3, region(PCIOUT_ID, block(0, Limit512G)), S4)
    ) ; (
        % IOMMU disabled.
        %add_PCI(S0, Id, S2)
        call(Module, S0, Id, S4)
    )),

    % connect the output to the systems pci bus
    state_add_overlay(S4, PCIOUT_ID, PCIBUS_ID, S5),

    % Now insert the BAR into the PCI bus address space
    findall((Addr, BarNum, BarStart, BarSize),
            call(bar(Addr, BarNum, BarStart, BarSize, mem, _, _))@eclipse,
            Bars),
    (foreach((_, BarNum, BarStart, BarSize), Bars),
     param(Id), param(PCIBUS_ID),
     fromto(S5, SIn, SOut, S6) do
        BarId = [BarNum, "BAR" | Id],
        InStart is BarStart,
        InEnd is BarStart + BarSize - 1,
        AcceptStart is 0,
        AcceptEnd is BarSize - 1,
        state_add_accept(SIn, region(BarId, block(AcceptStart, AcceptEnd)), SIn1),
        state_add_mapping(SIn1,region(PCIBUS_ID, block(InStart,InEnd)),
                                                 name(BarId,AcceptStart), SOut)
    ),
    % Set it to the node id where addresses are issued from the PCI device
    state_add_pci_id(S6, Addr, Enum, NewS).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Query Wrappers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% outputs a list of regions
write_regions(Regs) :-
    (foreach(Reg, Regs), foreach(Term, Terms) do
       Reg = region(NId, block(B, _)),
       node_enum(NId, Enum),
       Term = name(B, Enum)
    ),
    writeln(Terms).


% outputs a list of confs
write_confs(S, Confs) :-
    (foreach(Conf, Confs), param(S), foreach(Term, Terms)  do
        Conf = block_conf(NodeId, VPN, PPN),
        state_has_block_meta(S, NodeId, Bits, _),
        BlockSize is 2 ^ Bits,
        In is BlockSize * VPN,
        Out is BlockSize * PPN,
        node_enum_exists(NodeEnum, NodeId),
    %    printf("c(%d, 0x%lx, 0x%lx)", [NodeEnum, In, Out]),
        Term = c(NodeEnum, In, Out)
    ),
    %writeln(""),
    writeln(Terms).



:- export alloc_wrap/6.
alloc_wrap(S, Size, Bits, DestEnum, SrcEnums, NewS) :-
    (foreach(Enum, SrcEnums), foreach(Reg, SrcRegs) do
        Reg = region(RId, _),
        node_enum_exists(RId, Enum)
    ),
    node_enum_exists(DestId, DestEnum), % The double S is deliberate, no new allocation permitted.
    DestReg = region(DestId, _),
    alloc(S, Size, Bits, DestReg, SrcRegs, _),
    state_add_in_use(S, DestReg, NewS),

    append([DestReg], SrcRegs, OutputRegs),
    write_regions(OutputRegs).

:- export map_wrap/7.
map_wrap(S0, Size, Bits, DestEnum, DestAddr, SrcEnums, NewS)  :-

    % Set up DestReg
    node_enum_exists(DestId, DestEnum), % The double S is deliberate, no new allocation permitted.
    Limit is DestAddr + Size - 1,
    DestReg = region(DestId, block(DestAddr, Limit)),

    (foreach(Enum, SrcEnums), foreach(Reg, SrcRegs) do
        Reg = region(RId, _),
        node_enum_exists(RId, Enum)
    ),
    map(S0, Size, Bits, DestReg, SrcRegs, Confs),
    state_add_confs(S0, Confs, S2),
    (foreach(Reg, SrcRegs), fromto(S2, SIn, SOut, NewS) do
       state_add_in_use(SIn, Reg, SOut)
    ),

    append([DestReg], SrcRegs, OutputRegs),
    write_regions(OutputRegs).


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
    xeon_phi_meta(S, PCI_E, E1, E2, E3, E4, E5),
    printf("%p %p %p %p %p\n", [E1, E2, E3, E4, E5]).

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
