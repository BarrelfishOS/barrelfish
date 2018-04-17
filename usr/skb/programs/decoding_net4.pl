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
:- use_module(decoding_net4_state).


%%% Bottom layer is storing the following facts in the State
% accept(Region)
% mapping(SrcRegion, DstName)
% overlay(SrcNodeId, OutNodeId)
% block_meta(NodeId, Bits, OutNodeId)  -- Metadata for block reconfigurable nodes
% block_conf(NodeId, VPN, PPN)         -- For block reconfigurable nodes
% in_use(Region)                       -- Subset of accepted ranges that has been allocated

state_valid([]).
state_valid([accept(_) | As]) :- state_valid(As).
state_valid([mapping(_,_) | As]) :- state_valid(As).
state_valid([overlay(_,_) | As]) :- state_valid(As).
state_valid([block_meta(_,_,_) | As]) :- state_valid(As).
state_valid([block_conf(_,_,_) | As]) :- state_valid(As).
state_valid([in_use(_) | As]) :- state_valid(As).

:- export struct(block(base,limit)).
:- export struct(region(node_id,block)).
:- export struct(name(node_id,address)).

:- lib(ic).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Model layer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

region_mapping(S, SrcRegion, mapping(In, Out))  :-
    state_query(S, mapping(In, Out)),
    region_region_contains(SrcRegion, In).

name_mapping(S, Name, mapping(In,Out))  :-
    state_query(S, mapping(In, Out)),
    name_region_match(Name, In).

% translate with mapping
translate_region(S, SrcRegion, DstRegion) :-
    region_mapping(S, SrcRegion, mapping(InCandidate, OutCandidate)),
    region_base_name(SrcRegion, name{address:SrcAddr}),
    InCandidate = region{block:InBlock},
    OutCandidate = name{node_id: OutNodeId, address: DstBaseAddr},
    block_translate(SrcAddr, InBlock, DstAddr, DstBaseAddr),
    region_base_name(DstRegion, name{node_id: OutNodeId, address: DstAddr}),
    region_size(SrcRegion, Size),
    region_size(DstRegion, Size).

% translate with overlay
translate_region(S, SrcRegion, DstRegion) :-
    not(region_mapping(S, SrcRegion, _)),
    SrcRegion = region{node_id: SrcId, block:B},
    state_query(S, overlay(SrcId, DstId)),
    DstRegion = region{node_id: DstId, block:B}.


translate_region_loop(0, S, SrcID, BaseVPN, BasePPN).
translate_region_loop(I, S, SrcID, VPN, PFN):-
    state_query(S, block_conf(SrcId, VPN, PFN)),
    INext is I-1,
    VPNNext is VPN + 1,
    PFNNext is PFN + 1,
    translate_region_loop(INext, S, SrcID, VPNNext, PFNNext).

% translate with configurable nodes
translate_region(S, SrcRegion, DstRegion) :-
    not(region_mapping(S, SrcRegion, _)),
    SrcRegion = region{node_id: SrcId, block:SrcBlock},
    SrcBlock = block{base:SrcBase},
    state_query(S, block_meta(SrcId, Bits, DstId)),

    % Base of block must be Bits aligned
    aligned(SrcBase, Bits, _),

    % Size must be multiple of BlockSize
    block_size(SrcBlock, SrcSize),
    aligned(SrcSize, Bits, NumBlocks),
    ItEnd is NumBlocks - 1,

    % Check NumBlocks consecutive VPN/PPN pairs
    split_vaddr(SrcBase, Bits, [BaseVPN, Offset]),
    split_vaddr(DstBase, Bits, [BasePPN, Offset]),
    state_query(S, block_conf(SrcId, BaseVPN, BasePPN)),
    translate_region_loop(ItEnd, S, BaseVPN, BasePPN),

    % And calculate destination accordingly.
    DstBlock = block{base: DstBase},
    block_size(DstBlock, SrcSize),
    DstRegion = region{node_id:DstId, block: DstBlock}.



% translate with mapping
translate_name(S, SrcName, name{node_id: DstId, address: DstAddr}) :-
    name_mapping(S, SrcName, mapping(InCandidate, OutCandidate)),
    SrcName = name{address:SrcAddr},
    InCandidate = region{block:InBlock},
    OutCandidate = name{node_id: DstId, address: DstBaseAddr},
    block_translate(SrcAddr, InBlock, DstAddr, DstBaseAddr).

% translate with overlay
translate_name(S, SrcName, name{node_id: DstId, address: DstAddr}) :-
    not(name_mapping(S, SrcName, _)),
    SrcName = name{node_id: SrcId, address:DstAddr},
    state_query(S, overlay(SrcId, DstId)).

% translate with configurable nodes
translate_name(S, SrcName, name{node_id: DstId, address: DstAddr}) :-
    not(name_mapping(S, SrcName, _)),
    SrcName = name{node_id: SrcId, address:SrcAddr},
    state_query(S, block_meta(SrcId, Bits, DstId)),
    split_vaddr(SrcAddr, Bits, [VPN, Offset]),
    state_query(S, block_conf(SrcId, VPN, PPN)),
    split_vaddr(DstAddr, Bits, [PPN, Offset]).

accept_name(S, Name) :-
    state_query(S, accept(Candidate)),
    name_region_match(Name, Candidate).

accept_region(S, Region) :-
    state_query(S, accept(Candidate)),
    region_region_contains(Region, Candidate).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Queries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decodes_name(_, N,N).
decodes_name(S, SrcName, DstName) :-
    translate_name(S, SrcName, NextName),
    decodes_name(S, NextName, DstName).

decodes_region(_, N,N).
decodes_region(S, SrcName, DstName) :-
    translate_region(S, SrcName, NextName),
    decodes_region(S, NextName, DstName).

resolve_name(S, SrcName, DstName) :-
    SrcName = name{},
    DstName = name{},
    decodes_name(S, SrcName,DstName),
    accept_name(S, DstName).

resolve_region(S, SrcRegion, DstRegion) :-
    decodes_region(S, SrcRegion, DstRegion),
    accept_region(S, DstRegion).

region_in_use(S, Region) :-
    state_query(S, in_use(Candidate)),
    region_region_intersect(Region, Candidate).

region_free(S, Region) :-
    not(region_in_use(S, Region)).

% Ensure Base address is aligned to Bits and
region_aligned(Region, Bits) :-
    Region = region{block:block{base: Base}},
    aligned(Base, Bits, NumBlock),
    labeling([NumBlock]).

region_alloc(S, Reg, Size, Bits) :-
    region_aligned(Reg, Bits),
    region_size(Reg, Size),
    region_free(S, Reg).


translate_region_alloc(0, SIn, SrcId, VPN, PFN, SOut):-
    SOut = SIn.

translate_region_alloc(I, SIn, SrcId, VPN, PFN, SOut) :-
    INext is I - 1,
    VPNNext is VPN + 1,
    PFNNext is PFN + 1,
    translate_region_alloc(INext, SIn, VPNNext, PFNNext, SIn2),
    state_add(SIn2, block_conf(SrcId, VPN, PFN), SOut).

% Assumes SrcRegion has no mapping in S.
translate_region_conf(S, SrcRegion, DstRegion, Conf) :-
    state_empty(C1),
    SrcRegion = region{node_id: SrcId, block:SrcBlock},
    DstRegion = region{node_id: DstId, block:block{base: DstBase, limit: DstLimit}},
    SrcBlock = block{base:SrcBase},
    state_query(S, block_meta(SrcId, Bits, DstId)),

    % Base of block must be Bits aligned
    aligned(SrcBase, Bits, _),

    % Size must be multiple of BlockSize
    block_size(SrcBlock, SrcSize),
    aligned(SrcSize, Bits, NumBlocks),
    ItEnd is NumBlocks - 1,

    % Check NumBlocks consecutive VPN/PPN pairs
    split_vaddr(SrcBase, Bits, [BaseVPN, Offset]),
    split_vaddr(DstBase, Bits, [BasePPN, Offset]),
    DstLimit #= DstBase + SrcSize - 1,
    labeling([BaseVPN, BasePPN]),
    translate_region_alloc(ItEnd, CIn, SrcID, BaseVPN, BasePPN, COut).

route_step(S, SrcRegion, NextRegion, Conf) :-
    translate_region(S, SrcRegion, NextRegion),
    state_empty(Conf) ;
    translate_region_conf(S, SrcRegion, NextRegion, Conf).

route(S, SrcRegion, DstRegion, Conf) :-
    route_step(S, SrcRegion, NextRegion, C1),
    accept_region(S, NextRegion),
    DstRegion = NextRegion,
    Conf = C1 ;
    not(accept_region(S, NextRegion)),
    route(S, NextRegion, DstRegion, C2),
    state_union(C1, C2, Conf).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Interface queries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- export alias/3.
alias(S, N1, N2) :-
    resolve_name(S, N1, D),
    resolve_name(S, N2, D).


alloc_is_free(S, [], Size, Bits).
alloc_is_free(S, [Reg | Regs], Size, Bits) :-
    alloc_is_free(S, Regs, Size, Bits),
    region_alloc(S, Reg, Size, Bits).

alloc_is_in_use(SIn, [], CIn, COut):-
    COut = CIn.
alloc_is_in_use(S, [Reg | Regs], DestReg, CIn, COut) :-
    alloc_is_in_use(S, Regs, DestReg, CIn, CIn2),
    route(S, Reg, DestReg, C),
    state_union(CIn2, C, COut).

%%
% S - State
% Size - Size of the allocation (must be multiple of Bits)
% Bits - Alignment requirement for Base address
% SrcRegs - Observer Regions
% DestReg - Aligned region that will be resolved from Reg1/2
% Conf - State Delta which must be added to S
:- export alloc/6.
alloc(S, Size, Bits, DestReg, SrcRegs, Conf) :-
    alloc_is_free(S, SrcRegs, Size, Bits),
    region_alloc(S, DestReg, Size, Bits),
    accept_region(S, DestReg),
    alloc_is_in_use(S, SrcRegs, DestReg, [], Conf).



:- export map/6.
map(S, Size, Bits, DestReg, SrcRegs, Conf) :-
    alloc_is_free(S, SrcRegs, Size, Bits),
    accept_region(S, DestReg),
    alloc_is_in_use(S, SrcRegs, DestReg, [], Conf).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Existence of BlockNum as integer will tell that A is multiple of 2^Bits
aligned(A, Bits, BlockNum) :-
    BlockSize is 2^Bits,
    BlockNum #>= 0,
    A #= BlockNum * BlockSize.

split_vaddr(VA, BlockSizeBits, [VPN, Offset]) :-
    BlockSize is 2^Bits,
    VPN is VA / BlockSize,
    Offset is VA - (VPN * BlockSize).

% block_translate(A,BaseA,B,BaseB) ==> A-BaseA = B-BaseB
block_translate(SrcAddr, SrcBlock, DstAddr, DstBase) :-
    SrcBlock = block{base:SrcBase},
    DstAddr #= SrcAddr - SrcBase + DstBase.

region_base_name(Region, Name) :-
    Region = region{node_id: NodeId, block: block{base:Base}},
    Name = name{node_id:NodeId, address: Base}.

region_size(region{block:Block}, Size) :-
    block_size(Block, Size).

block_size(block{base:B, limit: L}, Size) :-
    Size #= L - B + 1.

address_block_match(A, block{base: B, limit: L}) :-
    A #>= B,
    L #>= A.

address_region_match(Addr, region{block:Block}) :-
    address_block_match(Addr, Block).

name_region_match(name{node_id:Id, address: A}, region{node_id: Id, block:Block}) :-
    address_block_match(A, Block).

% region_region_contains(A,B) --> A is part of B
region_region_contains(region{node_id:N, block:AB}, region{node_id:N, block:BB}) :-
    block_block_contains(AB, BB).

block_block_contains(A, B) :-
    A = block{base:ABase, limit: ALimit},
    B = block{base:BBase, limit: BLimit},
    ABase >= BBase,
    BLimit >= ABase,
    ALimit >= BBase,
    BLimit >= ALimit.

region_region_intersect(region{node_id:N, block:AB}, region{node_id:N, block:BB}) :-
    block_block_intersect(AB, BB).

addresses_intersect(Ab, Al, Bb, Bl) :-
    Ab >= Bb, Bl >= Ab;
    Bb >= Ab, Al >= Bb.

block_block_intersect(A, B) :-
    A = block{base:ABase, limit: ALimit},
    B = block{base:BBase, limit: BLimit},
    addresses_intersect(ABase, ALimit, BBase, BLimit).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


test_accept_name :-
    S = [accept(region{node_id:["In"], block: block{base: 50, limit:100}})],
    accept_name(S, name{node_id:["In"], address: 75}).

test_accept_region :-
    S = [accept(region{node_id:["In"], block: block{base: 50, limit:100}})],
    accept_region(S, region{node_id:["In"], block: block{base:75, limit:80}}).

test_translate_region :-
    S = [
        mapping(
            region{node_id:["In"], block:block{base:1000,limit:2000}},
            name{node_id: ["Out1"], address: 0}),
        overlay(["In"], ["Out2"])
      ],
    translate_region(S,
        region{node_id:["In"], block:block{base:1000,limit:2000}},
        region{node_id: ["Out1"], block:block{base:0, limit: 1000}}),
    translate_region(S,
        region{node_id:["In"], block:block{base:0,limit:999}},
        region{node_id: ["Out2"], block:block{base:0, limit:999}}),
    translate_region(S,
        region{node_id:["In"], block:block{base:2001,limit:3000}},
        region{node_id: ["Out2"], block:block{base:2001, limit:3000}}).

test_translate_region2 :-
    S = [
        block_meta(["In"], 21, ["Out"]),
        block_conf(["In"], 0, 1),
        block_conf(["In"], 1, 1)
      ],
    Base2M is 2^21,
    Limit2M is 2^21 - 1,
    Limit4M is 2*(2^21) - 1,
    Limit6M is 3*(2^21) - 1,
    translate_region(S,
        region{node_id:["In"], block:block{base:0,limit:Limit2M}},
        region{node_id:["Out"], block:block{base: Base2M, limit: Limit4M}}),
    not(translate_region(S,
        region{node_id:["In"], block:block{base:0,limit:Limit4M}},
        _)),
    S2 = [
        block_meta(["In"], 21, ["Out"]),
        block_conf(["In"], 0, 1),
        block_conf(["In"], 1, 2)
      ],
    translate_region(S2,
        region{node_id:["In"], block:block{base:0,limit:Limit4M}},
        region{node_id:["Out"], block:block{base: Base2M, limit: Limit6M}}).

test_translate_region_conf :-
    S = [
        block_meta(["In"], 21, ["Out"])
      ],
    Base6M is 3*2^21,
    Limit4M is 2*(2^21) - 1,
    translate_region_conf(S,
        region{node_id:["In"], block:block{base:0,limit:Limit4M}},
        Out, C),
    Out = region{block:block{base:Base6M}},
    state_query(C, block_conf(_, 0,3)),
    state_query(C, block_conf(_, 1,4)).

test_translate_name :-
    %Setup
    S = [
        mapping(
            region{node_id:["In"], block:block{base:1000,limit:2000}},
            name{node_id: ["Out1"], address: 0}),
        overlay(["In"], ["Out2"])
      ],
    translate_name(S,
        name{node_id:["In"], address:1000},
        name{node_id: ["Out1"], address:0}),
    translate_name(S,
        name{node_id:["In"], address:0},
        name{node_id: ["Out2"], address:0}),
    translate_name(S,
        name{node_id:["In"], address:2001},
        name{node_id: ["Out2"], address:2001}),
    not(translate_name(S,
        name{node_id:["In"], address:1000},
        name{node_id: ["Out2"], address:1000})).

test_resolve_name1 :-
    %Setup
    S = [
        mapping(
            region{node_id:["In"], block: block{base:1000,limit:2000}},
            name{node_id: ["Out1"], address: 0}),
        overlay(["In"], ["Out2"]),
        accept(region{node_id:["Out1"], block: block{base:0, limit:2000}}),
        accept(region{node_id:["Out2"], block: block{base:0, limit:2000}})
        ],
    % Hit the translate block
    resolve_name(S,
        name{node_id:["In"], address: 1000},
        name{node_id:["Out1"], address: 0}),
    % Hit the overlay
    resolve_name(S,
        name{node_id:["In"], address: 500},
        name{node_id:["Out2"], address: 500}).

test_resolve_name2 :-
    %Setup
    S = [mapping(
            region{node_id: ["In1"], block: block{base:1000,limit:2000}},
            name{node_id: ["Out1"], address: 0}),
        mapping(
            region{node_id:["In2"], block: block{base:6000,limit:7000}},
            name{node_id:["Out1"], address: 0}),
        accept(region{node_id:["Out1"], block: block{base:0, limit:2000}})
        ],
    % Reverse lookup
    resolve_name(S,
        name{node_id:["In1"], address:1000},
        R),
    resolve_name(S,
        name{node_id:["In2"], address:Out},
        R),
    Out = 6000.

test_route :-
    Upper is 512 * 1024 * 1024,
    Base2M is 2^21,
    Limit4M is 2*(2^21) - 1,
    Limit6M is 3*(2^21) - 1,
    S = [
        overlay(["IN"], ["MMU"]),
        block_meta(["MMU"], 21, ["RAM"]),
        accept(region{node_id: ["RAM"], block:block{base:0, limit: Upper}})
        ],
    route(S, region{node_id:["IN"], block:block{base:0, limit: Limit4M}}, OutRegion, Conf),
    OutRegion = region{node_id:["RAM"], block:block{base:Base2M, limit: Limit6M}},
    state_query(Conf, block_conf(["MMU"], 0, 1)),
    state_query(Conf, block_conf(["MMU"], 1, 2)).

test_region_alloc :-
    S = [in_use(region{node_id:["IN"], block:block{base:0,limit:2097151}})],
    Size is 2^21,
    Reg = region{node_id: ["IN"]},

    region_alloc(S, Reg, Size, 21),
    %printf("Reg=%p\n", Reg),

    Size2 is 2^24,
    Reg2 = region{node_id: ["IN"]},

    region_alloc(S, Reg2, Size2, 21).
    %printf("Reg2=%p\n", Reg2).

test_alloc :-
    Size is 2*(2^21),
    Base128M is 128*(2^21),
    Limit512M is 512*(2^21) - 1,
    S = [
        overlay(["IN1"], ["MMU1"]),
        block_meta(["MMU1"], 21, ["RAM"]),
        overlay(["IN2"], ["MMU2"]),
        block_meta(["MMU2"], 21, ["RAM"]),
        accept(region{node_id: ["RAM"], block:block{base:Base128M, limit: Limit512M}})
        ],
   Reg1 = region{node_id:["IN1"]},
   Reg2 = region{node_id:["IN2"]},
   DestReg = region{node_id:["RAM"]},
   alloc(S, Size, 21, DestReg, [Reg1, Reg2], _).
   %printf("Reg1=%p, Reg2=%p\n", [Reg1, Reg2]),
   %printf("DestReg=%p\n", [DestReg]),
   %printf("Conf=%p\n", [Conf]).

test_alias :-
    S = [
        overlay(["IN1"], ["OUT"]),
        mapping(
            region{node_id:["IN2"], block: block{base:5000,limit:10000}},
            name{node_id:["OUT"], address: 0}),
        accept(region{node_id: ["OUT"], block:block{base:0, limit: 10000}})
        ],
    N1 = name{node_id: ["IN1"], address: 0},
    N2 = name{node_id: ["IN2"]},
    alias(S, N1, N2),
    N2 = name{address:5000}.

run_test(Test) :-
    (
        call(Test),
        printf("Test %p succeeds!\n", Test)
    ) ; (
        printf("!!! Test %p failed !!!\n", Test)
    ).

:- export run_all_tests/0.
run_all_tests :-
    run_test(test_translate_region),
    run_test(test_translate_region2),
    run_test(test_translate_region_conf),
    run_test(test_translate_name),
    run_test(test_accept_name),
    run_test(test_resolve_name1),
    run_test(test_resolve_name2),
    run_test(test_accept_region),
    run_test(test_region_alloc),
    run_test(test_route),
    run_test(test_alias),
    run_test(test_alloc).
