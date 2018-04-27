%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2018, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is the one-dimensional implementation of the decoding net.
%% decoding_net3_multid contains the arbitrary dimensional implementation.

% Some Conventions:
% NodeId = identifier. list of strings
% Addr = 1
% Block block{..}

:- module(decoding_net4).
:- lib(ic).


%%% Bottom layer is storing the following facts in the State
% accept(Region)
% mapping(SrcRegion, DstName)
% overlay(SrcNodeId, OutNodeId)
% block_meta(NodeId, Bits, OutNodeId)  -- Metadata for block reconfigurable nodes
% block_conf(NodeId, VPN, PPN)         -- For block reconfigurable nodes
% in_use(Region)                       -- Subset of accepted ranges that has been allocated



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Persisted state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

state_remove([], _, []).
state_remove([Head|Tail], Fact, Out) :-
    Head = Fact,
    state_remove(Tail, Fact, Out)
    ;
    not(Head = Fact),
    state_remove(Tail, Fact, SubOut),
    Out = [Head | SubOut].

% define the empty state to have only emtpy lists
:- export state_empty/1.
state_empty(S) :-
    S = state([],[],[]).

% START STAYS HERE
:- export state_add_mapping/4.
state_add_mapping(S0, SrcReg, DstName, S1) :-
    S0 = state(M, F, A),
    S1 = state([mapping(SrcReg, DstName) | M], F, A).

:- export state_add_free/3.
% TODO: This should probably add to the list?
state_add_free(S0, NodeId, Blks, S1) :-
    S0 = state(M, F, A),
    S1 = state(M, [free(NodeId, Blks) | F], A).

:- export state_add_avail/4.
state_add_avail(S0, NodeId, C, S1) :-
    S0 = state(M, F, A),
    S1 = state(M, F, [avail(NodeId, VPN, PPN) | A]).
%%% STAYS HERE

:- export state_remove_mapping/4.
state_remove_mapping(S0, SrcReg, DstName, S1) :-
    S0 = state(M, F, A),
    state_remove(M, mapping(SrcReg, DstName), M1),
    S1 = state(M1, F, A).

:- export state_remove/4.
state_remove_free(S0, NodeId, Blks, S1) :-
    S0 = state(M, F, A),
    state_remove(F, free(NodeId, Blks), F1),
    S1 = state(M, F1, A).

:- export state_remove_avail/4.
state_remove_avail(S0, NodeId, C, S1) :-
    S0 = state(M, F, A),
    state_remove(A, avail(NodeId, C), A1),
    S1 = state(M, F, A1).


state_has_fact([Fact|_], Fact).
state_has_fact([_|Tail], Fact) :-
    state_has_fact(Tail, Fact).


:- export state_has_mapping/3.
state_has_mapping(S0, SrcReg, DstName) :-
    S0 = state(M, _, _),
    state_has_fact(M, mapping(SrcReg, DstName)).

state_has_free(S0, NodeId, Blks, S1) :-
    S0 = state(M, F, A),
    state_has_fact(F,free(NodeId, Blks)).

state_has_avail(S0, NodeId, C, S1) :-
    S0 = state(_, _, A),
    state_has_fact(A,avail(NodeId, C), A1).
    

%%%% STATIC STATE
:- dynamic translate/2.
:- export assert_translate/2.
assert_translate(A,B) :- assert(translate(A,B)).
:- export retract_translate/2.
retract_translate(A,B) :- retract(translate(A,B)).

:- dynamic overlay/2.
:- export assert_overlay/2.
assert_overlay(A,B) :- assert(overlay(A,B)).
:- export retract_overlay/2.
retract_overlay(A,B) :- retract(overlay(A,B)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Model layer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% region_region_contains(A, B).. A \in B
region_region_contains(region(N, block(ABase, ALimit)), region(N, block(BBase, BLimit))) :-
    ABase #>= BBase,
    BLimit #>= ABase,
    ALimit #>= BBase,
    BLimit #>= ALimit.

% region_name_translate(A, ASrc, DstBaseName, DstRegion
region_name_translate(
        region(SrcId, block(ABase, ALimit)),
        region(SrcId, block(ASrcBase, _)),
        name(DstId, DstBase), 
        region(DstId, block(BBase, BLimit))) :-
            Offset #= ABase - ASrcBase,
            BBase #= Offset + DstBase,
            BLimit #= ALimit - ABase + BBase.


translate(_, SrcReg, DstName) :- translate(SrcReg, DstName).
translate(S, SrcReg, DstName) :- state_has_mapping(S, SrcReg, DstName).

:- export translate_region/3.
translate_region(S, SrcReg, region(DstId,block(DstBase, DstLimit))) :-
    translate(S, SrcCand, name(DstId, AbsDstBase)),
    region_region_contains(SrcReg, SrcCand),
    region_name_translate(SrcReg, SrcCand, name(DstId, AbsDstBase), region(DstId,block(DstBase, DstLimit))).

translate_region(S, region(SrcId,B), region(DstId,B)) :-
    overlay(SrcId, DstId).


%% translate with mapping
%translate_region(S, SrcRegion, DstRegion) :-
%    region_mapping(S, SrcRegion, mapping(InCandidate, OutCandidate)),
%    region_base_name(SrcRegion, name(_, SrcAddr)),
%    InCandidate = region(_, InBlock),
%    OutCandidate = name(OutNodeId, DstBaseAddr),
%    block_translate(SrcAddr, InBlock, DstAddr, DstBaseAddr),
%    region_base_name(DstRegion, name(OutNodeId, DstAddr)),
%    region_size(SrcRegion, Size),
%    region_size(DstRegion, Size).
%
%
%
%
%
%% translate with overlay
%translate_region(S, SrcRegion, DstRegion) :-
%    not(region_mapping(S, SrcRegion, _)),
%    SrcRegion = region(SrcId, B),
%    state_has_overlay(S, SrcId, DstId),
%    DstRegion = region(DstId, B).
%
%
%% translate with configurable nodes
%translate_region(S, SrcRegion, DstRegion) :-
%    SrcRegion = region(SrcId, block(SrcBase, SrcLimit)),
%    DstRegion = region(DstId, block(DstBase, DstLimit)),
%
%    state_has_block_meta(S, SrcId, Bits, DstId),
%    bits_aligned_superregion(SrcRegion, Bits, SuperSrcRegion),
%    translate_region_aligned(S, SuperSrcRegion, SuperDstRegion),
%
%    % Calculate DstRegion
%    SuperSrcRegion = region(_, block(SuperSrcBase, SuperSrcLimit)),
%    SuperDstRegion = region(_, block(SuperDstBase, SuperDstLimit)),
%    BaseOffset is SrcBase - SuperSrcBase,
%    DstBase #= BaseOffset + SuperDstBase,
%    LimitOffset is SrcLimit - SuperSrcLimit, %LimitOffset is negative.
%    DstLimit #= LimitOffset + SuperDstLimit.
%
%% translate with configurable nodes, assumes aligned blocks
%translate_region_aligned(S, SrcRegion, DstRegion) :-
%    not(region_mapping(S, SrcRegion, _)),
%    SrcRegion = region(SrcId, SrcBlock),
%    SrcBlock = block(SrcBase, _),
%    state_has_block_meta(S, SrcId, Bits, DstId),
%
%    % Base of block must be Bits aligned
%    aligned(SrcBase, Bits, _),
%
%    % Size must be multiple of BlockSize
%    block_size(SrcBlock, SrcSize),
%    aligned(SrcSize, Bits, NumBlocks),
%    ItEnd is NumBlocks,
%
%    % Check NumBlocks consecutive VPN/PPN pairs
%    split_vaddr(SrcBase, Bits, BaseVPN, Offset),
%    split_vaddr(DstBase, Bits, BasePPN, Offset),
%    state_has_block_conf(S, SrcId, BaseVPN, BasePPN),
%    translate_region_loop(ItEnd, S, SrcId, BaseVPN, BasePPN),
%
%    % And calculate destination accordingly.
%    DstBlock = block(DstBase, _),
%    block_size(DstBlock, SrcSize),
%    DstRegion = region(DstId, DstBlock).
%
%
%
%
%% translate with mapping
%translate_name(S, SrcName, name(DstId, DstAddr)) :-
%    name_mapping(S, SrcName, mapping(InCandidate, OutCandidate)),
%    SrcName = name(_, SrcAddr),
%    InCandidate = region(_, InBlock),
%    OutCandidate = name(DstId, DstBaseAddr),
%    block_translate(SrcAddr, InBlock, DstAddr, DstBaseAddr).
%
%% translate with overlay
%translate_name(S, SrcName, name(DstId, DstAddr)) :-
%    not(name_mapping(S, SrcName, _)),
%    SrcName = name(SrcId,DstAddr),
%    state_has_overlay(S, SrcId, DstId).
%
%% translate with configurable nodes
%translate_name(S, SrcName, name(DstId, DstAddr)) :-
%    not(name_mapping(S, SrcName, _)),
%    SrcName = name(SrcId, SrcAddr),
%    state_has_block_meta(S, SrcId, Bits, DstId),
%    split_vaddr(SrcAddr, Bits, VPN, Offset),
%    state_has_block_conf(S, SrcId, VPN, PPN),
%    split_vaddr(DstAddr, Bits, PPN, Offset).
%
%accept_name(S, Name) :-
%    state_has_accept(S, Candidate),
%    name_region_match(Name, Candidate).
%
%accept_region(S, Region) :-
%    state_has_accept(S, Candidate),
%    region_region_contains(Region, Candidate).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Queries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%decodes_name(_, N,N).
%decodes_name(S, SrcName, DstName) :-
%    translate_name(S, SrcName, NextName),
%    decodes_name(S, NextName, DstName).
%
%decodes_region(_, N,N).
%decodes_region(S, SrcName, DstName) :-
%    translate_region(S, SrcName, NextName),
%    decodes_region(S, NextName, DstName).
%
%
%:- export resolve_name/3.
%resolve_name(S, SrcName, DstName) :-
%    SrcName = name(_, _),
%    DstName = name(_, _),
%    decodes_name(S, SrcName,DstName),
%    accept_name(S, DstName).
%
%resolve_region(S, SrcRegion, DstRegion) :-
%    decodes_region(S, SrcRegion, DstRegion),
%    accept_region(S, DstRegion).
%
%region_in_use(S, Region) :-
%    state_has_in_use(S, Candidate),
%    region_region_intersect(Region, Candidate).
%
%region_free(S, Region) :-
%    not(region_in_use(S, Region)).
%
%% Ensure Base address is aligned to Bits and
%region_aligned(Region, Bits) :-
%    Region = region(_, block(Base, _)),
%    aligned(Base, Bits, NumBlock),
%    labeling([NumBlock]).
%
%% This is a search optimization to find meaningful free regions.
%% The order of these facts matter (for performance).
%% Case 1: Try if just after an existing region works.
%region_free_bound(S, Reg) :-
%    Reg = region(Id, block(RBase, _)),
%    CReg = region(Id, block(_, CLimit)),
%    state_has_in_use(S, CReg),
%    RBase is CLimit + 1.
%
%% Case 2: Try if just the beginning of a mapping block works
%region_free_bound(S, Reg) :-
%    Reg = region(Id, block(RBase, _)),
%    CReg = region(Id, block(CBase, _)),
%    state_has_mapping(S, CReg, _),
%    RBase is CBase.
%
%% Case 3: Try if just the beginning of an accept block
%region_free_bound(S, Reg) :-
%    Reg = region(Id, block(RBase, _)),
%    CReg = region(Id, block(CBase, _)),
%    state_has_accept(S, CReg),
%    RBase is CBase.
%
%% Case 4: Give up. Try any (will lead to a sequential search).
%region_free_bound(_, Reg) :-
%    writeln("ENTERING SLOW PATH FOR ALLOCATION"),
%    Reg = region(_, block(RBase, _)),
%    RBase #>= 0.
%
%
%
%% This is a search optimization: Accepting regions must be in an existing region.
%region_accepting_bound(S, Reg) :-
%    Reg = region(Id, block(RBase, _)),
%    CReg = region(Id, block(AccBase, AccLimit)),
%    state_has_accept(S, CReg),
%    RBase #>= AccBase,
%    AccLimit #>= RBase.
%
%
%region_alloc(S, Reg, Size, Bits) :-
%    region_free_bound(S, Reg),
%    region_aligned(Reg, Bits),
%    region_size(Reg, Size),
%    region_free(S, Reg).
%
%region_alloc_slowpath(S, Reg, Size, Bits) :-
%    writeln("FORCED ENTERING SLOW PATH FOR ALLOCATION"),
%    region_aligned(Reg, Bits),
%    region_size(Reg, Size),
%    region_free(S, Reg).
%
%translate_region_alloc(0, Confs, _, _, _, Confs).
%
%translate_region_alloc(I, ConfsIn, SrcId, VPN, PFN, ConfsOut) :-
%    I >= 1,
%    INext is I - 1,
%    VPNNext is VPN + 1,
%    PFNNext is PFN + 1,
%    ConfsNext = [block_conf(SrcId, VPN, PFN) | ConfsIn],
%    translate_region_alloc(INext, ConfsNext, SrcId, VPNNext, PFNNext, ConfsOut).
%
%
%
%optimize_search_order(_, PPN) :-
%    PPNFirst is 2^30 * 4 / (2^21), % Start the search at 4G
%    integer(PPNFirst, PPNFirstI),
%    PPNLast is 2^30 * 16 / (2^21), % End the search at 16G
%    integer(PPNLast, PPNLastI),
%    PPN #>= PPNFirstI,
%    PPNLastI #>= PPN ;
%    true.
%
%% Assumes SrcRegion has no mapping in S, SrcRegion is a multiple of
%% the BlockSize and BlockSize aligned.
%translate_region_conf_aligned(S, SrcRegion, DstRegion, COut) :-
%    SrcRegion = region(SrcId, SrcBlock),
%    DstRegion = region(DstId, block(DstBase, DstLimit)),
%    SrcBlock = block(SrcBase, _),
%    state_has_block_meta(S, SrcId, Bits, DstId),
%
%    % Base of block must be Bits aligned
%    aligned(SrcBase, Bits, _),
%
%    % Size must be multiple of BlockSize
%    block_size(SrcBlock, SrcSize),
%    aligned(SrcSize, Bits, NumBlocks),
%    ItEnd is NumBlocks,
%
%    % Check NumBlocks consecutive VPN/PPN pairs
%    split_vaddr(SrcBase, Bits, BaseVPN, Offset),
%    %labeling([BaseVPN, Offset]),
%    split_vaddr(DstBase, Bits, BasePPN, Offset),
%    DstLimit #= DstBase + SrcSize - 1,
%    %optimize_search_order(BaseVPN, BasePPN),
%    labeling([BaseVPN, BasePPN, DstLimit, DstBase, SrcBase, Offset]),
%    translate_region_alloc(ItEnd, [], SrcId, BaseVPN, BasePPN, COut).
%
%:- export bits_aligned_superregion/3.
%bits_aligned_superregion(SrcRegion, Bits, SuperRegion) :-
%    SrcRegion = region(Id, block(Base, Limit)),
%    BlockSize is 2^Bits,
%    SuperBase is (Base // BlockSize) * BlockSize,
%    SuperLimit is (Limit // BlockSize + 1) * BlockSize - 1,
%    SuperRegion = region(Id, block(SuperBase, SuperLimit)).
%
%
%% Assumes SrcRegion has no mapping in S, support any size/position of SrcRegion
%translate_region_conf(S, SrcRegion, DstRegion, COut) :-
%    SrcRegion = region(SrcId, block(SrcBase, SrcLimit)),
%
%    region_free(S, SrcRegion),
%
%    DstRegion = region(DstId, block(DstBase, DstLimit)),
%    state_has_block_meta(S, SrcId, Bits, DstId),
%
%    % Get'n translate super region
%    bits_aligned_superregion(SrcRegion, Bits, SuperSrcRegion),
%    translate_region_conf_aligned(S, SuperSrcRegion, SuperDstRegion, COut),
%
%    % Calculate DstRegion
%    SuperSrcRegion = region(_, block(SuperSrcBase, SuperSrcLimit)),
%    SuperDstRegion = region(_, block(SuperDstBase, SuperDstLimit)),
%    BaseOffset is SrcBase - SuperSrcBase,
%    DstBase #= BaseOffset + SuperDstBase,
%    LimitOffset is SrcLimit - SuperSrcLimit, %LimitOffset is negative.
%    DstLimit #= LimitOffset + SuperDstLimit.
%
%
%
%route_step(S, SrcRegion, NextRegion, Conf) :-
%    translate_region(S, SrcRegion, NextRegion),
%    Conf = [] ;
%    not(translate_region(S, SrcRegion, NextRegion)),
%    translate_region_conf(S, SrcRegion, NextRegion, Conf).
%
%route_step_cont(S, NextRegion, DstRegion, C1, Conf) :-
%    accept_region(S, NextRegion),
%    DstRegion = NextRegion,
%    Conf = C1 ;
%    not(accept_region(S, NextRegion)),
%    route(S, NextRegion, DstRegion, C2),
%    append(C1, C2, Conf).
%
%% Requires SrcRegion ID and DstRegion ID to be set!
%route(S, SrcRegion, DstRegion, Conf) :-
%    % Pre-Check if this DstRegion can ever be reached from SrcRegion
%    SrcRegion = region(SrcId, _),
%    DstRegion = region(DstId, _),
%    reachable_id(S, SrcId, DstId),
%
%    %writeln(["Routing", SrcRegion, DstRegion]),
%    route_step(S, SrcRegion, NextRegion, C1),
%    route_step_cont(S, NextRegion, DstRegion, C1, Conf).
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Interface queries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:- export alias/3.
%alias(S, N1, N2) :-
%    resolve_name(S, N1, D),
%    resolve_name(S, N2, D).
%
%:- export alias_region/3.
%alias_region(S, R1, R2) :-
%    resolve_region(S, R1, D),
%    resolve_region(S, R2, D).
%
%%%%% HACK
%xeon_phi_extra_cons(SrcRegion, DstRegion) :-
%    DstRegion = region(["DRAM"],_),
%    SrcRegion = region(_, block(Base,_ )),
%    Base #>= 34359738368. %
%
%xeon_phi_extra_cons(SrcRegion, DstRegion) :-
%    not(DstRegion = region(["DRAM"],_)),
%    SrcRegion = region(_, block(Base,_ )),
%    Base #>= 0.
%
%%%%% ENDHACK
%
%% Make R2 an alias of R1, permitting configuration changes
%:- export alias_conf/4.
%% Try without reconfiguration
%alias_conf(S, R1, R2, []) :-
%    resolve_region(S, R1, DstRegion),
%    DstRegion = region(DstId, block(DstBase, _)),
%    R2 = region(R2Id, block(R2Base, R2Limit)),
%    resolve_name(S, name(R2Id, R2Base), name(DstId,DstBase)),
%    region_size(R1, R1Size),
%    R2Limit is R2Base + R1Size - 1,
%    resolve_region(S, R2, DstRegion).
%
%% Ok. Need reconfiguration.
%alias_conf(S, R1, R2, Conf) :-
%    resolve_region(S, R1, D),
%    xeon_phi_extra_cons(R2, D),
%    region_size(R1, R1Size),
%    region_alloc(S, R2, R1Size, 21),
%    route(S, R2, D, Conf).
%
%
%region_alloc_multiple(_, [], _, _).
%region_alloc_multiple(S, [Reg | Regs], Size, Bits) :-
%    region_alloc_multiple(S, Regs, Size, Bits),
%    region_alloc(S, Reg, Size, Bits).
%
%route_multiple(_, [], _, C, C).
%route_multiple(S, [Reg | Regs], DestReg, CIn, COut) :-
%    route_multiple(S, Regs, DestReg, CIn, CIn2),
%    route(S, Reg, DestReg, C),
%    append(CIn2, C, COut).
%
%region_alloc_and_route_multiple(_, [], _, _, _, []).
%region_alloc_and_route_multiple(S, [Reg | Regs], Size, Bits, DestReg, Conf) :-
%    region_alloc_and_route_multiple(S, Regs, Size, Bits, DestReg, C1),
%    region_alloc(S, Reg, Size, Bits),
%    route(S, Reg, DestReg, C2),
%    append(C1, C2, Conf).
%
%% NEW ALLOC:
%% Find intersection of all sources
%% keep a list of free blocks, pop front
%% Reachability of configurable nodes: - everything. if its
%% free, everything is reachable.
%
%%%
%% S - State
%% Size - Size of the allocation (must be multiple of Bits)
%% Bits - Alignment requirement for Base address
%% SrcRegs - Observer Regions
%% DestReg - Aligned region that will be resolved from Reg1/2
%% Conf - State Delta which must be added to S
%:- export alloc/6.
%alloc(S, Size, Bits, DestReg, SrcRegs, Conf) :-
%    % dest region not used and accepting
%    region_accepting_bound(S, DestReg),
%    region_alloc(S, DestReg, Size, Bits),
%    map(S, Size, Bits, DestReg, SrcRegs, Conf).
%
%:- export map/6.
%map(S, Size, Bits, DestReg, SrcRegs, Conf) :-
%    accept_region(S, DestReg), %Move this to region_accepting_bound
%
%    % Allocate and route src simultaneously
%    % make only two
%    region_alloc_and_route_multiple(S, SrcRegs, Size, Bits, DestReg, Conf).
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%% Existence of BlockNum as integer will tell that A is multiple of 2^Bits
%aligned(A, Bits, BlockNum) :-
%    BlockSize is 2^Bits,
%    BlockNum #>= 0,
%    A #= BlockNum * BlockSize.
%
%split_vaddr(VA, Bits, VPN, Offset) :-
%    BlockSize is 2^Bits,
%    BlockSizeMin1 is BlockSize - 1,
%    VPN #>= 0,
%    Offset #>= 0,
%    BlockSizeMin1 #>= Offset,
%    VA #>= 0,
%    VA #= VPN * BlockSize + Offset.
%
%
%
%% block_translate(A,BaseA,B,BaseB) ==> A-BaseA = B-BaseB
%block_translate(SrcAddr, SrcBlock, DstAddr, DstBase) :-
%    SrcBlock = block(SrcBase, _),
%    DstAddr #= SrcAddr - SrcBase + DstBase.
%
%region_base_name(Region, Name) :-
%    Region = region(NodeId, block(Base, _)),
%    Name = name(NodeId, Base).
%
%region_size(region(_, Block), Size) :-
%    block_size(Block, Size).
%
%block_size(block(B, L), Size) :-
%    Size #= L - B + 1.
%
%address_block_match(A, block(B, L)) :-
%    A #>= B,
%    L #>= A.
%
%
%address_region_match(Addr, region(_, Block)) :-
%    address_block_match(Addr, Block).
%
%name_region_match(name(Id, A), region(Id, Block)) :-
%    address_block_match(A, Block).
%
%
%
%region_region_intersect(region(N, AB), region(N, BB)) :-
%    block_block_intersect(AB, BB).
%
%addresses_intersect(Ab, Al, Bb, Bl) :-
%    Ab >= Bb, Bl >= Ab;
%    Bb >= Ab, Al >= Bb.
%
%block_block_intersect(A, B) :-
%    A = block(ABase,ALimit),
%    B = block(BBase,BLimit),
%    addresses_intersect(ABase, ALimit, BBase, BLimit).
%
%
%% Being optimistic that a translation can be found (ie it treats configurable
%% nodes like overlays), is DstId reachable from SrcId.
%:- export reachable_id/3.
%reachable_id_step(S, SrcId,NxtId) :-
%    state_has_overlay(S, SrcId, NxtId).
%reachable_id_step(S, SrcId, NxtId) :-
%    state_has_block_meta(S, SrcId, _, NxtId).
%reachable_id_step(S, SrcId, NxtId) :-
%    state_has_mapping(S, region(SrcId,_), name(NxtId, _)).
%reachable_id(_, Id, Id).
%reachable_id(S, SrcId, DstId) :-
%    reachable_id_step(S, SrcId, NxtId),
%    reachable_id(S, NxtId, DstId).
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Node enumeration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%:- export unused_node_enum/2.
%unused_node_enum(S, Enum) :-
%    Enum #>= 1,
%    labeling([Enum]),
%    not(state_has_node_enum(S, Enum, _)).
%
%:- export node_enum/4.
%node_enum(S, NodeId, Enum, NewS) :-
%        state_has_node_enum(S, Enum, NodeId),
%        NewS = S
%     ;
%        not(state_has_node_enum(S, Enum, NodeId)),
%        unused_node_enum(S, Enum),
%        state_add_node_enum(S, Enum, NodeId, NewS).
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Root Vnode Management
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%next_free_root_vnodeslot(S, NodeId, SlotTry, Slot) :-
%    not(state_has_free_vnodeslot(S, NodeId, SlotTry)),
%    Slot = SlotTry ;
%    NextSlotTry is SlotTry + 1,
%    next_free_root_vnodeslot(S, NodeId, NextSlotTry, Slot).
%
%:- export alloc_root_vnodeslot/4.
%alloc_root_vnodeslot(S, NodeId, Slot, NewS) :-
%    next_free_root_vnodeslot(S, NodeId, 1, Slot),
%    state_add_vnodeslot(S, NodeId, Slot, NewS).
%
%
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%/*
%
%test_accept_name :-
%    state_empty(S0),
%    state_add_accept(S0, region(["In"], block(50, 100)), S1),
%    accept_name(S1, name(["In"], 75)).
%
%test_accept_region :-
%    state_empty(S0),
%    state_add_accept(S0, region(["In"], block( 50, 100)), S1),
%    accept_region(S1, region(["In"], block(75, 80))).
%
%test_translate_region :-
%    state_empty(S0),
%    state_add_mapping(S0,
%            region(["In"], block(1000,2000)),
%            name( ["Out1"],  0), S1),
%    state_add_overlay(S1, ["In"], ["Out2"], S2),
%    translate_region(S2,
%        region(["In"], block(1000,2000)),
%        region( ["Out1"], block(0,  1000))),
%    translate_region(S2,
%        region(["In"], block(0,999)),
%        region( ["Out2"], block(0, 999))),
%    translate_region(S2,
%        region(["In"], block(2001,3000)),
%        region( ["Out2"], block(2001, 3000))).
%
%test_translate_region2 :-
%    state_empty(S1),
%    state_add_block_meta(S1, ["In"], 21, ["Out"],S2),
%    state_add_block_conf(S2, ["In"], 0, 1, S3),
%    state_add_block_conf(S3, ["In"], 1, 1, S4),
%    Base2M is 2^21,
%    Limit2M is 2^21 - 1,
%    Limit4M is 2*(2^21) - 1,
%    Limit6M is 3*(2^21) - 1,
%    translate_region(S4,
%        region(["In"], block(0,Limit2M)),
%        region(["Out"], block( Base2M,  Limit4M))),
%    not(translate_region(S4,
%        region(["In"], block(0,Limit4M)),
%        _)),
%
%    state_empty(Q0),
%    state_add_block_meta(Q0, ["In"], 21, ["Out"], Q1),
%    state_add_block_conf(Q1, ["In"], 0, 1, Q2),
%    state_add_block_conf(Q2, ["In"], 1, 2, Q3),
%    translate_region(Q3,
%        region(["In"], block(0,Limit4M)),
%        region(["Out"], block(Base2M,  Limit6M))).
%
%test_translate_region_conf :-
%    state_empty(S0),
%    state_add_block_meta(S0, ["In"], 21, ["Out"], S1),
%    Base6M is 3*2^21,
%    Limit4M is 2*(2^21) - 1,
%    translate_region_conf(S1,
%        region(["In"], block(0,Limit4M)),
%        Out, Confs),
%    Out = region(_, block(Base6M, _)),
%    state_add_confs(S1, Confs, S2),
%    state_has_block_conf(S2, _, 0,3),
%    state_has_block_conf(S2, _, 1,4).
%
%test_translate_region_conf2 :-
%    state_empty(S0),
%    state_add_block_meta(S0,["SMPT"], 34, ["RAM"],S1),
%
%    translate_region_conf(S1, region(["SMPT"], block(0,100)), DstRegion, Conf),
%    printf("DstRegion = %p, Conf = %p\n", [DstRegion, Conf]).
%
%test_translate_name :-
%    %Setup
%    state_empty(S0),
%    state_add_mapping(S0, region(["In"], block(1000,2000)), name(["Out1"],  0), S1),
%    state_add_overlay(S1, ["In"], ["Out2"], S2),
%    translate_name(S2,
%        name(["In"], 1000),
%        name(["Out1"], 0)),
%    translate_name(S2,
%        name(["In"], 0),
%        name(["Out2"], 0)),
%    translate_name(S2,
%        name(["In"], 2001),
%        name(["Out2"], 2001)),
%    not(translate_name(S2,
%        name(["In"], 1000),
%        name(["Out2"], 1000))).
%
%test_resolve_name1 :-
%    %Setup
%    state_empty(S0),
%    state_add_mapping(S0,
%            region(["In"], block(1000,2000)),
%            name(["Out1"],  0), S1),
%    state_add_overlay(S1, ["In"], ["Out2"], S2),
%    state_add_accept(S2, region(["Out1"], block(0, 2000)), S3),
%    state_add_accept(S3, region(["Out2"], block(0, 2000)), S4),
%    % Hit the translate block
%    resolve_name(S4,
%        name(["In"],  1000),
%        name(["Out1"],  0)),
%    % Hit the overlay
%    resolve_name(S4,
%        name(["In"],  500),
%        name(["Out2"],  500)).
%
%test_resolve_name2 :-
%    %Setup
%    state_empty(S0),
%    state_add_mapping(S0,
%            region( ["In1"], block(1000,2000)),
%            name( ["Out1"],  0), S1),
%    state_add_mapping(S1,
%            region(["In2"], block(6000,7000)),
%            name(["Out1"],  0), S2),
%    state_add_accept(S2, region(["Out1"], block(0, 2000)), S3),
%    % Reverse lookup
%    resolve_name(S3,
%        name(["In1"], 1000),
%        R),
%    resolve_name(S3,
%        name(["In2"], Out),
%        R),
%    Out = 6000.
%
%:- export test_route/0.
%test_route :-
%    Upper is 512 * 1024 * 1024,
%    Base2M is 2^21,
%    Limit4M is 2*(2^21) - 1,
%    Limit6M is 3*(2^21) - 1,
%
%    state_empty(S0),
%    state_add_overlay(S0,["IN"], ["MMU"],S2),
%    state_add_block_meta(S2,["MMU"], 21, ["RAM"],S3),
%    state_add_accept(S3,region(["RAM"], block(0, Upper)),S4),
%
%
%    OutRegion = region(["RAM"],block(Base2M, Limit6M)),
%    route(S4, region(["IN"],block(0,Limit4M)), OutRegion, Confs),
%    state_add_confs(S4, Confs, S5),
%    state_has_block_conf(S5, ["MMU"], 0, 1),
%    state_has_block_conf(S5, ["MMU"], 1, 2).
%
%% Test the case where we translate a block that fits completly (but does not fill)
%% into a configurable block.
%test_route2 :-
%    Upper is 512 * 1024 * 1024,
%    Base2M is 2^21,
%    Limit4M is 2*(2^21) - 1,
%
%    state_empty(S0),
%    state_add_overlay(S0,["IN"], ["SMPT"],S1),
%    state_add_block_meta(S1,["SMPT"], 34, ["RAM"],S2),
%    state_add_accept(S2,region(["RAM"], block(0, Upper)),S3),
%
%    OutRegion = region(["RAM"],block(Base2M, Limit4M)),
%    route(S3, region(["IN"],block(Base2M,Limit4M)), OutRegion, Confs),
%    printf("OutRegion = %p, Confs = %p\n", [OutRegion, Confs]).
%
%
%
%test_region_alloc :-
%    state_empty(S0),
%    state_add_in_use(S0, region(["IN"], block(0,2097151)), S1),
%
%    Size is 2^21,
%    Reg = region(["IN"], _),
%
%    region_alloc(S1, Reg, Size, 21),
%    %printf("Reg=%p\n", Reg),
%
%    Size2 is 2^24,
%    Reg2 = region(["IN"], _),
%
%    region_alloc(S1, Reg2, Size2, 21).
%    %printf("Reg2=%p\n", Reg2).
%
%test_alloc :-
%    Size is 2*(2^21),
%    Base128M is 128*(2^21),
%    Limit512M is 512*(2^21) - 1,
%
%    state_empty(S0),
%    state_add_overlay(S0, ["IN1"], ["MMU1"], S1),
%    state_add_block_meta(S1, ["MMU1"], 21, ["RAM"], S2),
%    state_add_overlay(S2, ["IN2"], ["MMU2"], S3),
%    state_add_block_meta(S3, ["MMU2"], 21, ["RAM"], S4),
%    state_add_accept(S4, region(["RAM"],block(Base128M, Limit512M)), S5),
%
%    Reg1 = region(["IN1"], _),
%    Reg2 = region(["IN2"], _),
%    DestReg = region(["RAM"], _),
%    alloc(S5, Size, 21, DestReg, [Reg1, Reg2], Confs),
%    printf("Reg1=%p, Reg2=%p\n", [Reg1, Reg2]),
%    printf("DestReg=%p\n", [DestReg]),
%    printf("Conf=%p\n", [Confs]).
%
%test_alias :-
%    state_empty(S0),
%    state_add_overlay(S0, ["IN1"], ["OUT"], S1),
%    state_add_mapping(S1, region(["IN2"], block(5000,10000)),name(["OUT"], 0), S2),
%    state_add_accept(S2, region(["OUT"], block(0, 10000)), S3),
%
%    N1 = name(["IN1"], 0),
%    N2 = name(["IN2"], _),
%    alias(S3, N1, N2),
%    N2 = name(_, 5000).
%
%run_test(Test) :-
%    (
%        call(Test),
%        printf("Test %p succeeds!\n", Test)
%    ) ; (
%        printf("!!! Test %p failed !!!\n", Test)
%    ).
%
%:- export run_all_tests/0.
%run_all_tests :-
%    run_test(test_translate_region),
%    run_test(test_translate_region2),
%    run_test(test_translate_region_conf),
%    run_test(test_translate_name),
%    run_test(test_accept_name),
%    run_test(test_resolve_name1),
%    run_test(test_resolve_name2),
%    run_test(test_accept_region),
%    run_test(test_region_alloc),
%    run_test(test_route),
%    run_test(test_route2),
%    run_test(test_alias),
%    run_test(test_alloc).
%*/
