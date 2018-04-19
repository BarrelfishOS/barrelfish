
:- module(decoding_net4_support).
:- use_module(decoding_net4).
:- lib(ic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Node enumeration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unused_node_enum(S, Enum) :-
    Enum #> 0,
    labeling([Enum]),
    not(state_query(S, enum_node_id(Enum, _))).

:- export node_enum/4.
node_enum(S, NodeId, Enum, NewS) :-
    (
        state_query(S, enum_node_id(Enum, NodeId)),
        NewS = S
    ) ; (
        unused_node_enum(S, Enum),
        state_add(S, enum_node_id(Enum, NodeId), NewS)
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
    Block = block(Base,Limit).

:- export init/1.
init(NewS) :-
    state_empty(S1),
    add_SYSTEM(S1, [], S2),
    DRAM_ID = ["DRAM"],
    initial_dram_block(Block),
    state_add(S2, accept(region(["DRAM"], Block)), S3), 
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
    state_add(S1, block_meta(MMU_IN_ID, 21, MMU_OUT_ID), S2), % Make MMU configurable
    OUT_ID = ["OUT" | Id],
    state_add(S2, overlay(OUT_ID, DRAM_ID), S3),
    % Reserve memory for the process, the OUT/PROC0 node is the one where
    % initially the process (virtual) addresses are issued.
    Limit = 1099511627775, % (512 << 31) - 1
    state_add(S3, in_use(region(OutId, block(0,Limit))), NewS).

iommu_enabled :-
    call(iommu_enabled,0,_)@eclipse.

% Addr - PCI address, should be provided by caller.
:- export add_pci/4.
add_pci(S, Addr, Enum, NewS) :-
    unused_node_enum(S, Enum),
    Id = [Enum],
    OutNodeId = ["OUT", "PCI0", Enum],
    node_enum(S, OutNodeId, Enum, S0),

    Addr = addr(_,_,_),
    PCIBUS_ID = ["PCIBUS"],
    PCIOUT_ID = ["OUT" | Id],
    (iommu_enabled -> (
        add_PCI_IOMMU(S0, Id, S1), 
        % Mark IOMMU block remappable
        state_add(S1, block_meta(["IN", "IOMMU0" | Id], 21, ["OUT", "IOMMU0" | Id]), S2)
    ) ; (
        % IOMMU disabled.
        add_PCI(S0, Id, S2)
    )),

    % connect the output to the systems pci bus
    state_add(S2, overlay(PCIOUT_ID, PCIBUS_ID), S3),

    % Now insert the BAR into the PCI bus address space
    findall((Addr, BarNum, BarStart, BarSize), 
            call(bar(Addr, BarNum, BarStart, BarSize, mem, _, _))@eclipse, 
            Bars),
    (foreach((_, BarNum, BarStart, BarSize), Bars),
     param(Id), param(PCIBUS_ID),
     fromto(S3, SIn, SOut, S4) do
        BarId = [BarNum, "BAR" | Id],
        BarEnd is BarStart + BarSize,
        state_add(SIn, accept(region(
            BarId,
            block(BarStart,BarEnd))), SIn1),
        state_add(SIn1,
            mapping(region(
                PCIBUS_ID,
                block(BarStart,BarEnd)
            ),
            name(
                BarId,
                BarStart
            )),
            SOut)
    ),
    % Set it to the node id where addresses are issued from the PCI device
    state_add(S4, pci_address_node_id(Addr, Enum), NewS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Root Vnode Management 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
next_free_root_vnodeslot(S, NodeId, SlotTry, Slot) :-
    not(state_query(S, root_vnode(NodeId, SlotTry))),
    Slot = SlotTry ;
    NextSlotTry is SlotTry + 1,
    next_free_root_vnodeslot(S, NodeId, NextSlotTry, Slot).

alloc_root_vnodeslot(S, NodeId, Slot, NewS) :-
    next_free_root_vnodeslot(S, NodeId, 2, Slot),
    state_add(S, root_vnode(NodeId, Slot), NewS).

:- export alloc_root_vnodeslot_wrap/4.
alloc_root_vnodeslot_wrap(S, NEnum, Slot, NewS) :-
    node_enum(S, NId, NEnum, S),
    alloc_root_vnodeslot(S, NId, Slot, NewS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Query Wrappers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_regions(S, Regs) :-
    (foreach(Reg, Regs), param(S), foreach(Term, Terms) do
       Reg = region(NId, block(B, _)),
       node_enum(S, NId, Enum, S),
       Term = name(B, Enum)
    ),
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
    state_add(S, in_use(DestReg), NewS),
    
    append([DestReg], SrcRegs, OutputRegs),
    write_regions(NewS, OutputRegs).

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
    map(S0, Size, Bits, DestReg, SrcRegs, S1),
    state_union(S0, S1, S2),
    (foreach(Reg, SrcRegs), fromto(S2, SIn, SOut, NewS) do
       state_add(SIn, in_use(Reg), SOut)
    ),
    
    append([DestReg], SrcRegs, OutputRegs),
    write_regions(NewS, OutputRegs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Persisted state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic state/1.

:- export state_set/1.
state_set(S) :-
    (state(_) -> retract(state(_)) ; true), assert(state(S)).

:- export state_get/1.
state_get(S) :- state(S) -> true ; state_empty(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Debug 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listing_term(S) :- write(S), writeln(",").

:- export decoding_net_listing/0.
decoding_net_listing :-
    state_get(S),
    writeln("state(S) :- S = ["),
    checklist(listing_term, S),
    writeln("]").

