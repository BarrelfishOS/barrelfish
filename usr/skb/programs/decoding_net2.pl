%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2018, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Some Conventions: 
% NodeId = identifier list 
% IAddr = [1,2,3] 
% Addr = [kind, [1,2,3]]
% IBlock [block{..}, block{...}]
% Block = [kind, [block{..}, block{..}]]


:- module(decoding_net2).


%% node_accept(InNodeId, InAddr :: block).
:- dynamic node_accept/2.

%% node_translate(InNodeId, InAddr :: block, OutNodeId, OutAddr :: block).
:- dynamic node_translate/4.

%% node_overlay(InNodeId, OutNodeId).
:- dynamic node_overlay/2.

:- export node_accept/2.
:- export node_translate/4.
:- export node_overlay/2.
:- export struct(block(base,limit,props)).
:- export struct(region(node_id,blocks)).
:- export struct(name(node_id,address)).

:- lib(ic).

iblock_match(A, block{base: B, limit: L}) :-
    B #=< A,
    A #=< L.

iblock_nomatch(A, block{base: B, limit: L}) :-
    A #< B ;
    A #> L.

iblocks_match_any(A, [B | Bs]) :-
    iblock_match(A, B) ; iblocks_match_any(A, Bs).

iblocks_match_any_ic(A, B) :-
    iblocks_match_any(A,B),
    labeling([A]).

:- export iblock_values/2.
% Union of blocks. [block{base:0,limit:5},block{base:33,limit:35}] -> 0,1,..,5,33,..,35
iblock_values(Blocks, Values) :-
    findall(X, iblocks_match_any_ic(X, Blocks), Values).
    

iblocks_match([], []).
iblocks_match([A|As], [B|Bs]) :-
    iblock_match(A,B),
    iblocks_match(As, Bs).

iblocks_nomatch([], []).
iblocks_nomatch([A|As], [B|Bs]) :-
    iblock_nomatch(A,B),
    iblocks_nomatch(As, Bs).


% For a ic constrained variable
iblocks_match_ic(X,Bs) :-
    length(Bs,LiLe),
    length(X,LiLe),
    iblocks_match(X, Bs),
    labeling(X).

:- export iblock_crossp/2.
% Cross product of blocks
iblock_crossp(Blocks, Values) :-
    findall(X, iblocks_match_ic(X, Blocks), Values).


address_match([K, IAddr], [K, IBlocks]) :-
    iblocks_match(IAddr, IBlocks).

:- export accept/2.
accept(NodeId, Addr) :-
    node_accept(NodeId, Block),
    address_match(Addr, Block).

:- export iaddr_iblock_map/4.
iaddr_iblock_map([], [], [], []).
iaddr_iblock_map([SrcAddr | A], [SrcBlock | B], [DstAddr | C], [DstBlock | D]) :-
    SrcBlock = block{base:SrcBase},
    DstBlock = block{base:DstBase},
    DstAddr #= SrcAddr - SrcBase + DstBase,
    iaddr_iblock_map(A,B,C,D).

:- export test_iaddr_iblock_map/0.
test_iaddr_iblock_map :-
    iaddr_iblock_map([1],[block{base:0, limit:1024}], Dst, [block{base:100,limit:2000}]),
    Dst = [101].

%% Convert from region (encoded as block) to names
to_name(Region,Name) :-
    region{
        node_id:Id,
        blocks: Blocks % Blocks = [Kind, [block{...},block{...}]]
    } = Region,
    address_match(Addr, Blocks),
    Name = name{
        node_id:Id,
        address:Addr
    }.


default_iaddr_constraint(Addr) :-
    Addr #>= 0,
    Addr #< 2147483648.

%% Thes functions turn an IC constrained Addr to base/limit blocks
iaddr_to_iblock_one(Addr, Block) :-
    default_iaddr_constraint(Addr),
    get_bounds(Addr,Min,Max),
    Size is Max - Min + 1,
    ( get_domain_size(Addr,Size) ->
            Block = block{
            base:Min,
            limit:Max
        }
    ;
        writeln(stderr,"Name conversion to region failed: Non continuous domain for address"),
        fail
    ).

iaddr_to_iblocks([], []).
iaddr_to_iblocks([A | As], [B | Bs]) :-
    iaddr_to_iblock_one(A,B),
    iaddr_to_iblocks(IAddr, Blocks).

addr_to_blocks([K, IAddr], [K, IBlocks]) :-
    iaddr_to_iblocks(IAddr, IBlocks).

%% Convert from names to regions

to_region(Name,Region) :-
    name{
        node_id:Id,
        address:Addr
    } = Name,
    region{
        node_id: Id,
        blocks: Blocks
    } = Region,
    addr_to_blocks(Addr, Blocks).



:- export block_translate/4.
block_translate([SrcK, ISrcAddr], [SrcK, SrcBlock], [DstK, IDstAddr], [DstK, DstBlock]) :-
    iaddr_iblock_map(ISrcAddr, SrcBlock, IDstAddr, DstBlock).

:- export test_block_translate/0.
test_block_translate :-
    block_translate(
        [memory,[1]],
        [memory, [block{base:0, limit:1024}]],
        Dst,
        [memory, [block{base:100,limit:2000}]]),
    Dst = [memory, [101]].
    

does_not_translate(NodeId, [AKind,IAddr]) :-
    findall(B, node_translate(NodeId, B, _, _), Blocks),
    (foreach([AKind, IBlock], Blocks),param(IAddr) do
        iblocks_nomatch(IAddr, IBlock)
    ).

:- export test_does_not_translate/0.
test_does_not_translate :-
    assert(node_translate(
        ["In"], [memory, [block{base:1000,limit:2000}]],
        ["Out1"], [memory, [block{base:0,limit:1000}]])),
    does_not_translate(["In"], [memory, [500]]).
    

% Takes translate and overlays predicates into account.
:- export translate/4.
translate(SrcNodeId, SrcAddr, DstNodeId, DstAddr) :-
    % The conditions check if, independently of Dst, a matching translate block
    % exists. Only if it doesnt, we consider the overlay
    (
        node_translate(SrcNodeId, SrcBlock, CandidId, CandidBlock),
        address_match(SrcAddr, SrcBlock),
        DstNodeId = CandidId,
        block_translate(SrcAddr, SrcBlock, DstAddr, CandidBlock)
    ;
        does_not_translate(SrcNodeId, SrcAddr),
        SrcAddr = DstAddr,
        node_overlay(SrcNodeId, DstNodeId)
    ).

:- export test_translate/0.
test_translate :-
    add_pci,
    translate(["IN", "IOMMU0", "PCI0"], [memory, [1]], OutNodeId, OutAddr),
    OutNodeId = ["OUT", "IOMMU0", "PCI0"],
    OutAddr = [memory, [1]].

:- export test_translate2/0.
test_translate2 :-
    %Setup
    assert(node_translate(
        ["In"], [memory, [block{base:1000,limit:2000}]],
        ["Out1"], [memory, [block{base:0,limit:1000}]])),
    assert(node_overlay(["In"], ["Out2"])),
    % Test the translate block
    translate(["In"], [memory, [1000]], ["Out1"], [memory, [0]]),
    % Test the overlay
    translate(["In"], [memory, [0]], ["Out2"], [memory, [0]]),
    % make sure the upper limit is respected.
    translate(["In"], [memory, [2500]], ["Out2"], [memory, [2500]]), 
    % make sure no within block translation to overlay exists
    not(translate(["In"], [memory, [1000]], ["Out2"], [memory, [1000]])).


:- export accept_dummy/2.
accept_dummy(NodeId, AcS) :-
    node_accept(NodeId, AcS).

:- export accept_name/1.
accept_name(Name) :-
    name{
        node_id:NodeId,
        address:Addr
    } = Name,
    accept(NodeId, Addr).


:- export decode_step/2.
decode_step(SrcName,DstName) :-
    name{
        node_id:SrcNodeId,
        address:SrcAddr
    } = SrcName,
    name{
        node_id:DstNodeId,
        address:DstAddr
    } = DstName,
    translate(SrcNodeId,SrcAddr,DstNodeId, DstAddr). % CHECKME

:- export test_decode_step/0.
test_decode_step :-
    add_pci,
    decode_step(
        name{node_id:["IN", "IOMMU0", "PCI0"], address: [memory, [1]]},
        Out),
    Out = name{node_id: ["OUT", "IOMMU0", "PCI0"], address: [memory, [1]]}.

% Reflexive, transitive closure of decode_step
:- export decodes_to/2.
decodes_to(S,S).

decodes_to(SrcName,DstName) :-
    decode_step(SrcName,Name),
    decodes_to(Name,DstName).

:- export resolve/2.
resolve(SrcName,DstName) :-
    name{} = SrcName,
    name{} = DstName,
    decodes_to(SrcName,DstName),
    accept_name(DstName).

resolve(SrcRegion,DstRegion) :-
    to_name(SrcRegion,SrcName),
    to_name(DstRegion,DstName),
    resolve(SrcName,DstName),
    to_region(SrcName,SrcRegion),
    to_region(DstName,DstRegion).

:- export test_resolve/0.
test_resolve :-
    %Setup
    assert(node_translate(
        ["In"], [memory, [block{base:1000,limit:2000}]],
        ["Out1"], [memory, [block{base:0,limit:1000}]])),
    assert(node_overlay(["In"], ["Out2"])),
    assert(node_accept(["Out1"], [memory,[block{base:0, limit:2000}]])),
    assert(node_accept(["Out2"], [memory,[block{base:0, limit:2000}]])),
    % Hit the translate block
    resolve(
        name{node_id:["In"], address:[memory, [1000]]},
        name{node_id:["Out1"], address:[memory, [0]]}),
    % Hit the overlay 
    resolve(
        name{node_id:["In"], address:[memory, [500]]},
        name{node_id:["Out2"], address:[memory, [500]]}).

:- export test_resolve2/1.
test_resolve2(Out) :-
    %Setup
    assert(node_translate(
        ["In1"], [memory, [block{base:1000,limit:2000}]],
        ["Out1"], [memory, [block{base:0,limit:1000}]])),
    assert(node_translate(
        ["In2"], [memory, [block{base:6000,limit:7000}]],
        ["Out1"], [memory, [block{base:0,limit:1000}]])),
    assert(node_accept(["Out1"], [memory,[block{base:0, limit:2000}]])),
    % Reverse lookup
    resolve(
        name{node_id:["In1"], address:[memory, [1000]]},
        R),
    resolve(
        name{node_id:["In2"], address:Out},
        R).

:- export test_resolve3/1.
test_resolve3(Out) :-
    %Setup
    assert(node_translate(
        ["In1"], [memory, [block{base:1000,limit:2000}]],
        ["Out1"], [memory, [block{base:0,limit:1000}]])),
    assert(node_translate(
        ["In2"], [memory, [block{base:6000,limit:7000}]],
        ["Out1"], [memory, [block{base:0,limit:1000}]])),
    assert(node_accept(["Out1"], [memory,[block{base:0, limit:2000}]])),
    InRegion = region{node_id:["In1"], blocks:[memory, [block{base:1000, limit:1500}]]},
    resolve(InRegion,Out).

%% Load a precompiled decoding net
:- export load_net/1.
load_net(File) :-
    ensure_loaded(File).

:- export load_net_module/2.
load_net_module(File, Mod) :-
    ensure_loaded(File),
    call(Mod, []).


%% FOR INTERACTIVE DEBUGGING ONLY 
:- export assert_node_accept/2.
assert_node_accept(A,B) :-
    assert(node_accept(A,B)).

:- export assert_node_translate/4.
assert_node_translate(A,B,C,D) :-
    assert(node_translate(A,B,C,D)).



%% X86 Support, should really be moved into its own file

:- export init/0.
:- export add_pci/0.
:- export add_process/0.
:- export remove_process_mapping/0.
:- export add_process_mapping/0.

init :-
    SYS_ID = ["SYS"],
    add_SYSTEM(SYS_ID).


% Make ID argument if we want to add multiple.
add_pci :-
    ID = ["PCI0"],
    PCIBUS_ID = ["PCIBUS", "SYS"],
    PCIIN_ID = ["IN" | ID],
    PCIOUT_ID = ["OUT" | ID],
    add_PCI_IOMMU(ID),
    % connect the output to the systems pci bus
    assert(node_overlay(PCIOUT_ID, PCIBUS_ID)),
    % Now insert the BAR into the PCI bus address space
    assert(node_translate(PCIBUS_ID, [memory,[block{base:1024,limit:2048}]], PCIIN_ID, [memory, [block{base:1024,limit:2048}]])).

% Make ID argument if we want to add multiple.
add_process :-
    ID = ["PROC0"],
    DRAM_ID = ["DRAM", "SYS"],
    add_PROC_MMU(ID),
    assert(node_overlay(["OUT" | ID], DRAM_ID)).

remove_process_mapping :-
    MMU_ID = ["MMU","PROC0"],
    IN_ID = ["IN" | MMU_ID],
    OUT_ID = ["OUT" | MMU_ID],
    retract(node_translate(IN_ID, _, _, _)).

add_process_mapping :-
    MMU_ID = ["MMU","PROC0"],
    IN_ID = ["IN" | MMU_ID],
    OUT_ID = ["OUT" | MMU_ID],
    assert(node_translate(IN_ID, [memory, [block{base:1024,limit:2048}]], OUT_ID, [memory, [block{base:1024,limit:2048}]])).

:- export process_to_pci/2.
process_to_pci(ProcAddr, PCIAddr) :- 
    PROC_ID = ["OUT", "PROC0", "PROC0"],
    PCI_ID = ["OUT", "PCI0"],
    resolve(name{node_id:PROC_ID, address: ProcAddr}, D),
    resolve(name{node_id:PCI_ID, address: PCIAddr}, D).

:- export test_process_to_pci/1.
test_process_to_pci(Out) :- 
    init, add_pci, add_process,
    process_to_pci([memory,[0]], Out).


% Return a region accessible by both, PROC and PCI
:- export common_dram/3.
common_dram(DRAM, PROC_RANGE, PCI_RANGE) :- 
    PROC_ID = ["OUT", "PROC0", "PROC0"],
    PCI_ID = ["OUT", "PCI0"],
    PROC_RANGE = region{node_id: PROC_ID, blocks: [memory, [block{base:0,limit:1000}]]},
    %PCI_RANGE = region{node_id: PCI_ID, blocks: [memory, [block{base:0,limit:1000}]]},
    resolve(PROC_RANGE, DRAM),
    resolve(PCI_RANGE, DRAM).

:- export test_common_dram/3.
test_common_dram(A,B,C) :-
    init, add_pci, add_process,
    common_dram(A,B,C).
