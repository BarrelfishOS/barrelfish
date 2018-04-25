
:- module(decoding_net4_support).
:- use_module(decoding_net4).
:- lib(ic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Node enumeration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unused_node_enum(S, Enum) :-
    Enum #> 0,
    labeling([Enum]),
    not(state_has_node_enum(S, Enum, _)).

:- export node_enum/4.
node_enum(S, NodeId, Enum, NewS) :-
    (
        state_has_node_enum(S, Enum, NodeId),
        NewS = S
    ) ; (
        not(state_has_node_enum(S, Enum, NodeId)),
        unused_node_enum(S, Enum),
        state_add_node_enum(S, Enum, NodeId, NewS)
    ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Sockeye support
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- export load_net/1.
load_net(File) :-
    ensure_loaded(File).

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

:- export init/1.
init(NewS) :-
    state_add_accept(S2, region(["DRAM"], Block), S3),
    state_empty(S1),
    add_SYSTEM(S1, [], S2),
    DRAM_ID = ["DRAM"],
    initial_dram_block(Block),
    state_add_accept(S2, region(["DRAM"], Block), S3),
    node_enum(S3, DRAM_ID, DRAM_ENUM, S4),
    printf("Decoding net initialized using %p as DRAM. DRAM nodeid: %p\n",
        [Block, DRAM_ENUM]),
    NewS = S4.

    % Manage space for vnodes
    %vnode_meta(PageSize, PoolSize),
    %VnodePoolSize is PageSize * PoolSize,
    %Size = [VnodePoolSize],
    %alloc_range(S2, DRAM_ID, [memory, Size], BaseOut, S3),
    %mark_range_in_use(S3, DRAM_ID, BaseOut, Size, S4),
    %in_use(DRAM_ID, Region),
    %assert(vnode_region(Region)),
    %writeln("Using for PageTables:"), writeln(Region).

:- export add_process/3.
add_process(S, Enum, NewS) :-
    unused_node_enum(S, Enum),
    Id = [Enum],
    % The node where the process virtual addresses are issued.
    OutId = ["OUT", "PROC0" | Id],
    node_enum(S, OutId, Enum, S0),

    DRAM_ID = ["DRAM"],
    add_PROC_MMU(S0, Id, S1),

    % Mark MMU block remappable
    MMU_IN_ID = ["IN", "MMU0" | Id],
    MMU_OUT_ID = ["OUT", "MMU0" | Id],
    state_add_block_meta(S1, MMU_IN_ID, 21, MMU_OUT_ID, S2), % Make MMU configurable
    OUT_ID = ["OUT" | Id],
    state_add_overlay(S2, OUT_ID, DRAM_ID, S3),
    % Reserve memory for the process, the OUT/PROC0 node is the one where
    % initially the process (virtual) addresses are issued.
    Limit = 10995116277760, % (512 << 31) - 1
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
    state_add_overlay(S2, BAR0_ID, GDDR_ID, NewS).

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

% Given the node enum E of the xeon phi (as returned by xeon phi)
% get some other nodeid's
:- export xeon_phi_meta/6.
xeon_phi_meta(S, E, N, K1OMCoreId, SMPTId, IOMMUId) :-
    node_enum(S, N, E, S),
    N = [_ | Rm],
    K1OMCoreId = ["K1OM_CORE" | Rm],
    SMPTId = ["SMPT_IN" | Rm],
    IOMMUId = ["IN", "IOMMU0", E].

% Helper to instantiate a PCI card. If no IOMMU is present it will
% instantiate Module, else ModuleIommu
add_pci_internal(S, Addr, Enum, Module, ModuleIOMMU, NewS) :-
    unused_node_enum(S, Enum),
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
        state_add_in_use(S2, region(IOMMU_IN_ID, block(0, Limit512G)), S3)
    ) ; (
        % IOMMU disabled.
        %add_PCI(S0, Id, S2)
        call(Module, S0, Id, S3)
    )),

    % connect the output to the systems pci bus
    state_add_overlay(S3, PCIOUT_ID, PCIBUS_ID, S4),

    % Now insert the BAR into the PCI bus address space
    findall((Addr, BarNum, BarStart, BarSize),
            call(bar(Addr, BarNum, BarStart, BarSize, mem, _, _))@eclipse,
            Bars),
    (foreach((_, BarNum, BarStart, BarSize), Bars),
     param(Id), param(PCIBUS_ID),
     fromto(S4, SIn, SOut, S5) do
        BarId = [BarNum, "BAR" | Id],
        BarEnd is BarSize - 1,
        state_add_accept(SIn, region(BarId, block(0, BarEnd)), SIn1),
        state_add_mapping(SIn1,region(PCIBUS_ID, block(BarStart,BarEnd)),
                                                 name(BarId,0), SOut)
    ),
    % Set it to the node id where addresses are issued from the PCI device
    state_add_pci_id(S5, Addr, Enum, S6),
    % We keep the lowest root vnode slot unmapped, hence the 512G hole.
    state_add_in_use(S6, region(OutNodeId, block(0, Limit512G)), NewS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Root Vnode Management
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
next_free_root_vnodeslot(S, NodeId, SlotTry, Slot) :-
    not(state_has_free_vnodeslot(S, NodeId, SlotTry)),
    Slot = SlotTry ;
    NextSlotTry is SlotTry + 1,
    next_free_root_vnodeslot(S, NodeId, NextSlotTry, Slot).

alloc_root_vnodeslot(S, NodeId, Slot, NewS) :-
    next_free_root_vnodeslot(S, NodeId, 1, Slot),
    state_add_vnodeslot(S, NodeId, Slot, NewS).

:- export alloc_root_vnodeslot_wrap/4.
alloc_root_vnodeslot_wrap(S, NEnum, Slot, NewS) :-
    node_enum(S, NId, NEnum, S),
    alloc_root_vnodeslot(S, NId, Slot, NewS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Query Wrappers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_regions(S, Regs, NewS) :-
    (foreach(Reg, Regs), param(S), foreach(Term, Terms), fromto(S, InS, OutS, NewS) do
       Reg = region(NId, block(B, _)),
       node_enum(InS, NId, Enum, OutS),
       Term = name(B, Enum)
    ),
    writeln(Terms).

write_confs(S, Confs) :-
    (foreach(Conf, Confs), param(S), foreach(Term, Terms)  do
        Conf = block_conf(NodeId, VPN, PPN),
        state_has_block_meta(S, NodeId, Bits, _),
        BlockSize is 2 ^ Bits,
        In is BlockSize * VPN,
        Out is BlockSize * PPN,
        state_has_node_enum(S, NodeEnum, NodeId),
    %    printf("c(%d, 0x%lx, 0x%lx)", [NodeEnum, In, Out]),
        Term = c(NodeEnum, In, Out)
    ),
    %writeln(""),
    writeln(Terms).

:- export alloc_wrap/6.
alloc_wrap(S, Size, Bits, DestEnum, SrcEnums, NewS) :-
    (foreach(Enum, SrcEnums), foreach(Reg, SrcRegs), param(S) do
        Reg = region(RId, _),
        node_enum(S, RId, Enum, S)
    ),
    node_enum(S, DestId, DestEnum, S), % The double S is deliberate, no new allocation permitted.
    DestReg = region(DestId, _),
    alloc(S, Size, Bits, DestReg, SrcRegs, _),
    state_add_in_use(S, DestReg, S1),

    append([DestReg], SrcRegs, OutputRegs),
    write_regions(S1, OutputRegs, NewS).

:- export map_wrap/7.
map_wrap(S0, Size, Bits, DestEnum, DestAddr, SrcEnums, NewS)  :-

    % Set up DestReg
    node_enum(S0, DestId, DestEnum, S0), % The double S is deliberate, no new allocation permitted.
    Limit is DestAddr + Size - 1,
    DestReg = region(DestId, block(DestAddr, Limit)),

    (foreach(Enum, SrcEnums), foreach(Reg, SrcRegs), param(S0) do
        Reg = region(RId, _),
        node_enum(S0, RId, Enum, S0)
    ),
    map(S0, Size, Bits, DestReg, SrcRegs, Confs),
    state_add_confs(S0, Confs, S2),
    (foreach(Reg, SrcRegs), fromto(S2, SIn, SOut, S3) do
       state_add_in_use(SIn, Reg, SOut)
    ),

    append([DestReg], SrcRegs, OutputRegs),
    write_regions(S3, OutputRegs, NewS).

% Translate from RAM nodeid to the next Bus Id
ram_bus_nodeid(["DRAM"], ["PCIBUS"]).
% ram_bus_nodeid(["GDDR"], ["PCIBUS"]). % ADD XEON PHI RULE

:- export alias_conf_wrap/6.
alias_conf_wrap(S0, SrcEnum, SrcAddr, Size, DstEnum, NewS)  :-
    xeon_phi_meta(S0, DstEnum, _, XeonSrc, SmptId, IommuId), % TODO: Make me work with arbitrary destinations.

    node_enum(S0, SrcNodeId, SrcEnum, S1),
    node_enum(S1, SmptId, _, S2),
    node_enum(S2, IommuId, _, S3),
    % HACK: Replace dram nodeid with bus. Because our capabilities actually
    % protect an area in the BUS not in the DRAM.
    ram_bus_nodeid(SrcNodeId, SrcBusId),
    % ENDHACK
    SrcLimit is SrcAddr + Size - 1,
    R1 = region(SrcBusId, block(SrcAddr, SrcLimit)),

    XPhiRegion = region(XeonSrc, _),
    alias_conf(S3, R1, XPhiRegion, Confs),
    state_add_confs(S3, Confs, S4),

    state_add_in_use(S4, XPhiRegion, S5),

    write_regions(S5, [XPhiRegion], NewS),
    write_confs(NewS, Confs).
    %write_confs_for_nodeid(NewS, smpt, SMPTId, Confs),
    %write_iommu_confs_for_nodeid(NewS, iommu, SMPTId, Confs).
    % TODO Think about what should be in NewS


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
