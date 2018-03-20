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

%% This fact keeps track of blocks that are in use.
%% node_in_use(NodeId, Block).
:- dynamic node_in_use/2.

:- export node_accept/2.
:- export node_translate/4.
:- export node_overlay/2.
:- export node_in_use/2.
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

iaddress_gt([], []).
iaddress_gt([S | Ss], [B | Bs]) :-
    S #< B,
    iaddress_gt(Ss,Bs).

% Will only compare addresses of the same kind
address_gt([K, ISmaller], [K, IBigger]) :-
    iaddress_gt(ISmaller, IBigger).

iblock_iaddress_gt([], []).
iblock_iaddress_gt([Block | Bs], [Addr | As]) :-
    block{
        limit: Limit
    } = Block,
    Addr #>  Limit,
    iblock_iaddress_gt(Bs, As).

block_address_gt([K, IBlocks], [K, IAddress]) :-
    iblock_iaddress_gt(IBlocks, IAddress).

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
    iaddr_to_iblocks(As, Bs).

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
    ensure_loaded(File).
    %call(Mod, []).


%% FOR INTERACTIVE DEBUGGING ONLY 
:- export assert_node_accept/2.
assert_node_accept(A,B) :-
    assert(node_accept(A,B)).

:- export assert_node_translate/4.
assert_node_translate(A,B,C,D) :-
    assert(node_translate(A,B,C,D)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% X86 Support. Complements the sockeye file, should really be moved into its own file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- export init/0.
:- export add_pci/0.
:- export add_process/0.
:- export remove_process_mapping/0.
:- export add_process_mapping/0.

init :-
    SYS_ID = ["SYS"],
    add_SYSTEM(SYS_ID),
    % Reserver some memory
    assert(node_in_use(["SYS","DRAM"], [memory, [block{base:0, limit:8192}]])).

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
    OUT_ID = ["OUT" | ID],
    assert(node_overlay(OUT_ID, DRAM_ID)),
    % Reserve memory for the process
    assert(node_in_use(OUT_ID, [memory, [block{base:0, limit: 65535}]])).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Mark ranges used and Query them 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- export free_range/3.
free_range(NodeId, _, Out) :-
   % Not a very smart allocator, finds the highest addr in use and append
   % Therefore can ignore Size
   findall(X, node_in_use(NodeId, X), UsedBlockLi),
   (foreach(UsedBlock, UsedBlockLi), param(Out) do
       block_address_gt(UsedBlock, Out)
   ).

:- export free_range/2.
free_range(Name, Size) :-
    name{
        node_id: NodeId,
        address: Out
    } = Name,
    free_range(NodeId, Size, Out).

%% NodeId:: Addr, Size :: Addr, Out :: Addr
:- export alloc_range/3.
alloc_range(NodeId, Size, Out) :-
   free_range(NodeId, Size, Out),
   term_variables(Out, OutVars),
   labeling(OutVars).


% After finding a range with alloc range, you actually want to mark it used
% with this function.
mark_range_in_use(NodeId, Addr, ISize) :-
    Addr = [Kind, IAddr],
    (foreach(UsedBlock, UsedBlockLi), foreach(A, IAddr), foreach(S,ISize) do
        Limit is A + S,
        UsedBlock = block{
            base: A,
            limit: Limit
        }    
    ),
    assert(node_in_use(NodeId, [Kind, UsedBlockLi])).

mark_range_in_use(Name, ISize) :-
    name{
        node_id: NodeId,
        address: Addr
    } = Name,
    mark_range_in_use(NodeId, Addr, ISize).

:- export test_alloc_range/0.
test_alloc_range :-
    Id = [],
    % Test setup
    mark_range_in_use(Id, [memory, [0]], [1000]),
    
    % First allocation
    Size = [1000],
    alloc_range(Id, [memory, Size], Out),
    mark_range_in_use(Id, Out, Size),
    writeln("First allocation: "),
    writeln(Out),

    % Second allocation
    Size2 = [5000],
    alloc_range(Id, [memory, Size2], Out2),
    mark_range_in_use(Id, Out2, Size2),
    writeln("Second allocation: "),
    writeln(Out2).

% Find a unused buffer. 
% Node1 :: Addr, Node2 :: Name, Resolved :: Name
:- export common_free_buffer/4.
common_free_buffer(BufferSize, Node1, Node2, Resolved)  :-
    free_range(Node1, [memory, [BufferSize]]),
    free_range(Node2, [memory, [BufferSize]]),
    resolve(Node1, Resolved),
    resolve(Node2, Resolved),
    free_range(Resolved, BufferSize),
    term_variables(Resolved, Vars),
    labeling(Vars).

:- export test_common_free_buffer/3.
test_common_free_buffer(Proc,Pci,Resolved) :-
    init, add_pci, add_process,
    BUFFER_SIZE = 1024,
    Proc = name{node_id: ["OUT", "PROC0"]},
    Pci = name{node_id: ["OUT", "PCI0"]},
    common_free_buffer(BUFFER_SIZE, Proc, Pci, Resolved).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Bit Array Representation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Constrains word to be a N bit word
assert_word(W, N) :-
    dim(W,[N]), W :: [0 .. 1].

% This beauty converts words (array of bit values) to a numeric representation.
word_to_num(W, Num) :-
    dim(W, [Len]),
    (for(I,1,Len), fromto(0,In,Out,NumT), param(W) do
        Out = W[I] * 2^(I-1) + In),
    Num $= eval(NumT).

% A part of word is etracted into Subword, Range specifies
subword(Word,Subword, Range) :- 
    SW is Word[Range],
    array_list(Subword,SW).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Block Remappable Nodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic node_block_conf/3. %(NodeId, VPN, PPN).
:- dynamic node_block_meta/3. %(NodeId, BlockSizeBits, OutNodeId)

node_translate_block(InNodeId, [memory, [VAddr]], OutNodeId, [memory, [PAddr]]) :-
    node_block_meta(InNodeId, BlockSizeBits, OutNodeId),
    % Bit-Lookup Offset and VPN
    assert_word(VAW, 48),
    word_to_num(VAW, VAddr),
    subword(VAW, VAOffsetW, 1 .. BlockSizeBits),
    VPNStartBit is BlockSizeBits + 1,
    subword(VAW, VPNW, VPNStartBit .. 48),
    word_to_num(VPNW, VPN),

    % Lookup PPN and PA offset
    node_block_conf(InNodeId, VPN, PPN),
    PAOffsetW = VAOffsetW,
    
    % Stich together PA
    assert_word(PAW, 48), % TODO bit size for physical address?
    subword(PAW, VAOffsetW, 1 .. 21), 
    subword(PAW, PPNW, 22 .. 48),
    word_to_num(PPNW, PPN),

    word_to_num(PAW, PAddr).

:- export assert_block_delta/2.
assert_block_delta(NodeId, BlockDelta) :-
    (foreach((VPN,PPN), BlockDelta),param(NodeId) do
        assert(node_block_conf(NodeId, VPN, PPN))
    ).

:- export test_node_translate_block/0. 
test_node_translate_block :-
    assert(node_block_conf([], 0, 1)),
    assert(node_block_meta([], 21, ["OUT"])),
    node_translate_block([], [memory,[1000]], ["OUT"], [memory, [2098152]]).


% Same signature as node_translate
node_translate_pt(InNodeId, [K, [VAddr]], OutNodeId, [K, [PAddr]]) :-
    node_pt(InNodeId, PtIndex, OutNodeId),
    pt_translate(PtIndex, VAddr, PAddr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% X86 Page table configurable nodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% node_pt stores the root page table for per node
:- dynamic node_pt/3. % NodeId, RootPt, OutNodeId
:- dynamic pt/3. % PtIndex, Offset, NextPtIndex
:- export pt/3.
    
:- export split_vpn/2.
split_vpn(VPN, Parts) :-
    assert_word(VPNW, 27),  % 3 x 9 = 27
    word_to_num(VPNW, VPN),
    subword(VPNW, L1W, 1 .. 9),
    word_to_num(L1W, L1),
    subword(VPNW, L2W, 10 .. 18),
    word_to_num(L2W, L2),
    subword(VPNW, L3W, 19 .. 27),
    word_to_num(L3W, L3),
    Parts = [L3,L2,L1].

:- export test_split_vpn/0.
test_split_vpn :-
    VA = 16'40201, % hex(1 | 1<<9 | 1<<18)
    split_vpn(VA, [1,1,1]).


pt_alloc(Idx, Idx) :-
    not(pt(Idx,_,_)).

pt_alloc(Try, Idx) :-
    NextTry is Try + 1,
    pt_alloc(NextTry, Idx).

:- export pt_alloc/1.
pt_alloc(Idx) :-
    pt_alloc(0,Idx), !,
    assert(pt(Idx,-1,-1)).

% Inner function for pt_delta, better not use anywhere else.
pt_delta_append_if_new(In, Out, pt(A,B,C)) :-
    nonvar(A), nonvar(B),
    % C magic: nonvar, or already installed, or it will be new allocated
    (nonvar(C) ; (pt(A,B,C) ; pt_alloc(C))),
    (pt(A,B,C) -> In = Out ; (Out = [pt(A,B,C) | In])).

% Get the difference between the PT facts (which reflect in memory contents)
% and the newly to be inserted block translate facts.
% BlockDelta :: [(VPN, PPN), ...]
% PtDelta :: [pt(Idx, Offset, NextIdx), ...]
:- export pt_delta/3.
pt_delta(NodeId, BlockDelta, PtDelta) :-
    %findall((VPN, PPN), node_block_conf(NodeId, VPN, PPN), Maps),
    (foreach((VPN,PPN), BlockDelta),param(NodeId),fromto([], In, Out, PtDelta) do
        node_pt(NodeId, RootPt, _),
        split_vpn(VPN, [L3,L2,L1]),
        pt_delta_append_if_new(In,Out1,   pt(RootPt, L3, L2Idx)),
        pt_delta_append_if_new(Out1,Out2, pt(L2Idx, L2, L1Idx)),
        pt_delta_append_if_new(Out2,Out,  pt(L1Idx, L1, PPN))
    ).

:- export test_pt_delta/1.
test_pt_delta(PtDelta) :-
    pt_alloc(Root),
    assert(node_pt([],Root,["OUT"])),
    pt_delta([], [(0,23)], PtDelta).
    % PtDelta = [pt(2, 0, 23), pt(1, 0, 2), pt(0, 0, 1)] module reordering.

% For a list of PTs, assert the facts.
:- export assert_pt_delta/2.
assert_pt_delta(NodeId, PtDelta) :-
    (foreach(X, PtDelta) do
        assert(X)
    ).
