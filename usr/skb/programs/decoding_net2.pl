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

:- use_module(allocator).


%% node_accept(InNodeId, InAddr :: block).
:- dynamic node_accept/2.

%% node_translate_dyn(InNodeId, InAddr :: block, OutNodeId, OutAddr :: block).
:- dynamic node_translate_dyn/4.

%% node_overlay(InNodeId, OutNodeId).
:- dynamic node_overlay/2.

%% This fact keeps track of blocks that are in use.
%% node_in_use(NodeId, Block).
:- dynamic node_in_use/2.

%:- export node_accept/2.
%:- export node_overlay/2.
%:- export node_in_use/2.
:- export struct(block(base,limit,props)).
:- export struct(region(node_id,blocks)).
:- export struct(name(node_id,address)).

:- lib(ic).

%% True, if any possible matching between two node ids A and B may exist.
%node_reachable(A,A).
%node_reachable(A,B) :-
%    node_translate_dyn(A,_,Mid,_),
%    node_reachable(Mid, B).
%
%node_reachable(A,B) :-
%    node_block_meta(A,_,Mid),
%    node_reachable(Mid, B).

iaddress_aligned([], _).
iaddress_aligned([A | As], Bits) :-
    BlockSize is 2^Bits,
    BlockNum #>= 0,
    A #= BlockNum * BlockSize,
    iaddress_aligned(As, Bits).

address_aligned([_, IAddress], Bits) :-
    iaddress_aligned(IAddress, Bits).

name_aligned(Name, Bits) :-
    name{address: Addr} = Name,
    address_aligned(Addr, Bits).

test_alignment :-
    iaddress_gt([536870912], IAddr),
    iaddress_aligned(IAddr, 21),
    labeling(IAddr),
    writeln(IAddr).

test_alignment2 :-
    init, add_pci, add_process,
    Proc = region{node_id: ["OUT", "PROC0", "PROC0"]},
    free_region_aligned(Proc, [memory, [1024]]),
    writeln(Proc).

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

iaddress_gte([], []).
iaddress_gte([S | Ss], [B | Bs]) :-
    S #=< B,
    iaddress_gte(Ss,Bs).

% Will only compare addresses of the same kind
address_gte([K, ISmaller], [K, IBigger]) :-
    iaddress_gte(ISmaller, IBigger).

iaddress_sub([], [], []).
iaddress_sub([A | As], [B | Bs], [C | Cs]) :-
    C is A - B,
    iaddress_sub(As,Bs,Cs).

% A - B = C ---> address_sub(A,B,C)
address_sub([K, IA], [K, IB], [K, IC]) :-
    iaddress_sub(IA, IB, IC).

iaddress_add([], [], []).
iaddress_add([A | As], [B | Bs], [C | Cs]) :-
    C is A + B,
    iaddress_add(As,Bs,Cs).

% A + B = C ---> address_add(A,B,C)
address_add([K, IA], [K, IB], [K, IC]) :-
    iaddress_add(IA, IB, IC).

iaddress_add_const([], _, []).
iaddress_add_const([A | As], B, [C | Cs]) :-
    C is A + B,
    iaddress_add_const(As,B,Cs).

% A + B = C ---> address_add(A,B,C)
address_add_const([K, IA], B, [K, IC]) :-
    iaddress_add_const(IA, B, IC).

iblock_iaddress_gt([], []).
iblock_iaddress_gt([Block | Bs], [Addr | As]) :-
    block{
        limit: Limit
    } = Block,
    Addr #>  Limit,
    iblock_iaddress_gt(Bs, As).

block_address_gt([K, IBlocks], [K, IAddress]) :-
    iblock_iaddress_gt(IBlocks, IAddress).


iblock_limit_iaddress([], []).
iblock_limit_iaddress([Block | Bs], [Addr | As]) :-
    block{
        limit: Addr
    } = Block,
    iblock_limit_iaddress(Bs, As).

% Turn the limit of the blocks into an address
block_limit_address([K, IBlocks], [K, IAddress]) :-
    iblock_limit_iaddress(IBlocks, IAddress).

iblock_base_iaddress([], []).
iblock_base_iaddress([Block | Bs], [Addr | As]) :-
    block{
        base: Addr
    } = Block,
    iblock_base_iaddress(Bs, As).

% Turn the base of the blocks into an address
block_base_address([K, IBlocks], [K, IAddress]) :-
    (var(IBlocks),var(IAddress), fail) ; 
    iblock_base_iaddress(IBlocks, IAddress).

% Turn a region into a base name
region_base_name(Region, Name) :-
    Region = region{node_id: NodeId, blocks: Blocks},
    Name = name{node_id:NodeId, address: Base},
    block_base_address(Blocks, Base).

% Turn a region into a limit name
region_limit_name(Region, Name) :-
    Region = region{node_id: NodeId, blocks: Blocks},
    block_limit_address(Blocks, Base),
    Name = name{node_id:NodeId, address: Base}.

iblock_isize([],[]).
iblock_isize([A | As],[B | Bs]) :-
    block{
        base: Base,
        limit: Limit
    } = A,
    (
        (var(B), B is Limit - Base) ;
        (var(Limit), Limit is Base + B)
    ),
    iblock_isize(As, Bs).

block_size([K, IBlocks], [K, ISize]) :-
    iblock_isize(IBlocks, ISize).

region_size(Region, Size) :-
    region{ blocks: Blocks } = Region,
    block_size(Blocks, Size).

:- export accept/2.
accept(NodeId, Addr) :-
    node_accept(NodeId, Block),
    address_match(Addr, Block).

iaddr_iblock_map([], [], [], []).
iaddr_iblock_map([SrcAddr | A], [SrcBlock | B], [DstAddr | C], [DstBlock | D]) :-
    SrcBlock = block{base:SrcBase},
    DstBlock = block{base:DstBase},
    DstAddr #= SrcAddr - SrcBase + DstBase,
    iaddr_iblock_map(A,B,C,D).

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



block_translate([SrcK, ISrcAddr], [SrcK, SrcBlock], [DstK, IDstAddr], [DstK, DstBlock]) :-
    iaddr_iblock_map(ISrcAddr, SrcBlock, IDstAddr, DstBlock).

test_block_translate :-
    block_translate(
        [memory,[1]],
        [memory, [block{base:0, limit:1024}]],
        Dst,
        [memory, [block{base:100,limit:2000}]]),
    Dst = [memory, [101]].
    

does_not_translate(NodeId, [AKind,IAddr]) :-
    %TODO take node_translate_block into account
    findall(B, node_translate_dyn(NodeId, B, _, _), Blocks),
    (foreach([AKind, IBlock], Blocks),param(IAddr),param(AKind) do
        iblocks_nomatch(IAddr, IBlock)
    ).

test_does_not_translate :-
    assert(node_translate_dyn(
        ["In"], [memory, [block{base:1000,limit:2000}]],
        ["Out1"], [memory, [block{base:0,limit:1000}]])),
    does_not_translate(["In"], [memory, [500]]).
    

% Takes translate and overlays predicates into account.
%:- export translate/4.
translate(SrcNodeId, SrcAddr, DstNodeId, DstAddr) :-
    % The conditions check if, independently of Dst, a matching translate block
    % exists. Only if it doesnt, we consider the overlay
    (
        node_translate_dyn(SrcNodeId, SrcBlock, CandidId, CandidBlock),
        address_match(SrcAddr, SrcBlock),
        DstNodeId = CandidId,
        block_translate(SrcAddr, SrcBlock, DstAddr, CandidBlock)
    ;
        node_translate_block(SrcNodeId, SrcAddr, DstNodeId, DstAddr)
    ;
        does_not_translate(SrcNodeId, SrcAddr),
        SrcAddr = DstAddr,
        node_overlay(SrcNodeId, DstNodeId)
    ).

test_translate :-
    add_pci,
    translate(["IN", "IOMMU0", "PCI0"], [memory, [1]], OutNodeId, OutAddr),
    OutNodeId = ["OUT", "IOMMU0", "PCI0"],
    OutAddr = [memory, [1]].

test_translate2 :-
    %Setup
    assert(node_translate_dyn(
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


accept_dummy(NodeId, AcS) :-
    node_accept(NodeId, AcS).

:- export accept/1.
accept(Name) :-
    name{
        node_id:NodeId,
        address:Addr
    } = Name,
    accept(NodeId, Addr).

accept(Region) :-
    region_base_name(Region,Base),
    accept(Base),
    region_limit_name(Region,Limit),
    accept(Limit).


:- export translate/2.
translate(SrcName,DstName) :-
    name{
        node_id:SrcNodeId,
        address:SrcAddr
    } = SrcName,
    name{
        node_id:DstNodeId,
        address:DstAddr
    } = DstName,
    translate(SrcNodeId,SrcAddr,DstNodeId, DstAddr).

test_translate_name :-
    add_pci,
    translate(
        name{node_id:["IN", "IOMMU0", "PCI0"], address: [memory, [1]]},
        Out),
    Out = name{node_id: ["OUT", "IOMMU0", "PCI0"], address: [memory, [1]]}.

% Reflexive, transitive closure of translate
:- export decodes_to/2.
decodes_to(S,S).

decodes_to(SrcName,DstName) :-
    translate(SrcName,Name),
    decodes_to(Name,DstName).

:- export resolve/2.
resolve(SrcName,DstName) :-
    name{} = SrcName,
    name{} = DstName,
    decodes_to(SrcName,DstName),
    accept(DstName).

% TODO: Resolve on regions should work like route.
resolve(SrcRegion,DstRegion) :-
    to_name(SrcRegion,SrcName),
    to_name(DstRegion,DstName),
    resolve(SrcName,DstName),
    to_region(SrcName,SrcRegion),
    to_region(DstName,DstRegion).

test_resolve :-
    %Setup
    assert(node_translate_dyn(
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

test_resolve2(Out) :-
    %Setup
    assert(node_translate_dyn(
        ["In1"], [memory, [block{base:1000,limit:2000}]],
        ["Out1"], [memory, [block{base:0,limit:1000}]])),
    assert(node_translate_dyn(
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

test_resolve3(Out) :-
    %Setup
    assert(node_translate_dyn(
        ["In1"], [memory, [block{base:1000,limit:2000}]],
        ["Out1"], [memory, [block{base:0,limit:1000}]])),
    assert(node_translate_dyn(
        ["In2"], [memory, [block{base:6000,limit:7000}]],
        ["Out1"], [memory, [block{base:0,limit:1000}]])),
    assert(node_accept(["Out1"], [memory,[block{base:0, limit:2000}]])),
    InRegion = region{node_id:["In1"], blocks:[memory, [block{base:1000, limit:1500}]]},
    resolve(InRegion,Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Load sockeye compiled decoding nets and instantiate modules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- export load_net/1.
load_net(File) :-
    ensure_loaded(File).

:- export load_module/2.
load_module(Mod, Id) :-
    call(Mod, Id).

:- export load_net_module/2.
load_net_module(File, Mod) :-
    ensure_loaded(File),
    call(Mod, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Node enumeration. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- export alloc_node_enum/1.
:- dynamic enum_node_id/2.
alloc_node_enum(N) :- alloc_one(node_enum, N).

get_or_alloc_node_enum(NodeId, Enum) :-
    enum_node_id(Enum, NodeId) ;
    ( alloc_node_enum(Enum), assert(enum_node_id(Enum, NodeId)) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% X86 Support. Complements the sockeye file, should really be moved into its own file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- export init/0.
:- export add_pci_alloc/1.
:- export add_pci/1.
:- export add_process/1.
:- export add_process_alloc/1.

:- dynamic pci_address_node_id/2.
:- export pci_address_node_id/2.
:- dynamic process_node_id/2.
:- export process_node_id/2.

% This uses the memory_region facts (defined in the main module) to 
% find a region above 4G that we will manage.
initial_dram_block(Block) :- %a
    % Find the usable DRAM using the existing SKB facts
    call(mem_region_type, RamType, ram)@eclipse,
    findall((Base, Size), call(memory_region,Base,Bits,Size,RamType,Data)@eclipse, MemCandidates),
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
    Block = block{base:Base, limit: Limit}.


init :-
    add_SYSTEM([]),
    DRAM_ID = ["DRAM"],
    initial_dram_block(Block),
    assert(node_accept(["DRAM"], [memory, [Block]])), 
    printf("Decoding net initialized using %p as DRAM\n", Block).

add_pci :-
    add_pci(["PCI0"]).

add_pci(Id) :-
    PCIBUS_ID = ["PCIBUS"],
    PCIIN_ID = ["IN" | Id],
    PCIOUT_ID = ["OUT" | Id],
    add_PCI_IOMMU(Id),
    % Mark IOMMU block remappable
    assert(node_block_meta(["IN", "IOMMU0" | Id], 21, ["OUT", "IOMMU0" | Id])), % Make MMU configurable
    % And assign a root PT
    pt_alloc(Root),
    assert(node_pt(["IN", "IOMMU0", Id], Root, ["OUT","IOMMU0",Id])),
    % connect the output to the systems pci bus
    assert(node_overlay(PCIOUT_ID, PCIBUS_ID)),
    % Now insert the BAR into the PCI bus address space
    assert(node_translate_dyn(PCIBUS_ID, [memory,[block{base:1024,limit:2048}]], PCIIN_ID, [memory, [block{base:1024,limit:2048}]])).

add_pci_alloc(Addr) :-
    alloc_node_enum(Enum),
    add_pci([Enum]),
    % Set it to the node id where addresses are issued from the PCI device
    assert(enum_node_id(Enum, ["OUT", "PCI0", Enum])),
    assert(pci_address_node_id(Addr, Enum)).

add_process_alloc(Enum) :-
    alloc_node_enum(Enum),
    add_process([Enum]),
    % Set it to the node id where addresses are issued from the process
    assert(enum_node_id(Enum, ["OUT", "PROC0", Enum])).
    %assert(process_node_id(ProcId, Enum)).


% Make ID argument if we want to add multiple.
add_process :-
    add_process(["PROC0"]).

add_process(Id) :-
    DRAM_ID = ["DRAM"],
    add_PROC_MMU(Id),

    % Mark MMU block remappable
    MMU_IN_ID = ["IN", "MMU0" | Id],
    MMU_OUT_ID = ["OUT", "MMU0" | Id],
    assert(node_block_meta(MMU_IN_ID, 21, MMU_OUT_ID)), % Make MMU configurable
    pt_alloc(Root),
    assert(node_pt(MMU_IN_ID, Root, MMU_OUT_ID)),

    OUT_ID = ["OUT" | Id],
    assert(node_overlay(OUT_ID, DRAM_ID)),
    % Reserve memory for the process, the OUT/PROC0 node is the one where
    % initially the process (virtual) addresses are issued.
    assert(node_in_use(["OUT", "PROC0" | Id], [memory, [block{base:0, limit: 65535}]])).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%%% Mark ranges used and Query them 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- export free_region/3.
% Puts IC constraints on the variables
free_region(NodeId, _, Out) :-
   % Not a very smart allocator, finds the highest addr in use and append
   % Therefore can ignore Size
   findall(X, node_in_use(NodeId, X), UsedBlockLi),
   block_address_gt([memory, [block{limit: -1}]], Out), % TODO: Works only for 1 Dim addr.
   (foreach(UsedBlock, UsedBlockLi), param(Out) do
       block_address_gt(UsedBlock, Out)
   ).

:- export free_region/2.
% Puts IC constraints on the variables
free_region(Name, Size) :-
    name{
        node_id: NodeId,
        address: Out
    } = Name,
    free_region(NodeId, Size, Out).

free_region(Region, Size) :-
    region_base_name(Region, Name),
    free_region(Name, Size).

% Resolves the variables
free_region_aligned(Region, Size) :-
    is_list(Size),
    region_base_name(Region, BaseName),
    free_region(BaseName, Size),
    name_aligned(BaseName, 21),
    term_variables(BaseName, BaseNameVars),
    labeling(BaseNameVars),
    region_size(Region, Size).

%:- export free_region/1.
%free_region(Region) :-
%    region_size(Region, Size), % Determine size using the base/limit in the region.
%    free_region(Region, Size).


%% NodeId:: Addr, Size :: Addr, Out :: Addr
:- export alloc_range/3.
alloc_range(NodeId, Size, Out) :-
   free_region(NodeId, Size, Out),
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

mark_range_in_use(Region) :-
    Region = region{ node_id: NodeId, blocks: Blocks },
    assert(node_in_use(NodeId, Blocks)).

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

% Find a unused buffer, using already set up routing.
% Node1 :: Addr, Node2 :: Name, Resolved :: Name
common_free_buffer_existing(BufferSize, Node1, Node2, Resolved)  :-
    free_region(Node1, [memory, [BufferSize]]),
    free_region(Node2, [memory, [BufferSize]]),
    resolve(Node1, Resolved),
    resolve(Node2, Resolved),
    free_region(Resolved, BufferSize),
    term_variables(Resolved, Vars),
    labeling(Vars).

test_common_free_buffer_existing(Proc,Pci,Resolved) :-
    init, add_pci, add_process,
    BUFFER_SIZE = 1024,
    Proc = name{node_id: ["OUT", "PROC0", "PROC0"]},
    Pci = name{node_id: ["OUT", "PCI0", "PCI0"]},
    common_free_buffer_existing(BUFFER_SIZE, Proc, Pci, Resolved).

% Like common_free_buffer_existing, but allow reconfiguration of nodes (routing)
% Find two regions N1Region and N2Region, that resolve to a free region.
:- export common_free_buffer/5.
common_free_buffer(Size, N1Region, N2Region, ResRegion,Route)  :-
    is_list(Size),
    
    N1Region = region{blocks: [memory, [_]]},
    N2Region = region{blocks: [memory, [_]]},
    ResRegion = region{blocks: [memory, [block{base:Base, limit: Limit}]]},

    % nail down the input regions first
    free_region_aligned(N1Region, Size),
    free_region_aligned(N2Region, Size),

    route(N1Region, ResRegion, R1),
    route(N2Region, ResRegion, R2),

    free_region(ResRegion, Size),
    labeling([Base,Limit]), 
    union(R1,R2,Route).

% Find two regions N1Region and N2Region, that resolve to an existing result region.
:- export common_free_map/5.
common_free_map(Size, N1Region, N2Region, ResRegion,Route)  :-
    is_list(Size),
    
    N1Region = region{blocks: [memory, [_]]},
    N2Region = region{blocks: [memory, [_]]},
    ResRegion = region{blocks: [memory, [block{base:Base, limit: Limit}]]},

    % nail down the input regions first
    free_region_aligned(N1Region, Size),
    free_region_aligned(N2Region, Size),

    route(N1Region, ResRegion, R1),
    route(N2Region, ResRegion, R2),

    labeling([Base,Limit]), 
    union(R1,R2,Route).

% the function called from mem_serv
:- export alloc_common/3.
% Allocate a Buffer with Bits size, reachable from N1 and N2. Mark the resolved 
% region as in use.
alloc_common(Bits, N1Enum, N2Enum)  :-
    enum_node_id(N1Enum, N1Id),
    enum_node_id(N2Enum, N2Id),
    R1 = region{node_id: N1Id, blocks: [memory, [block{base:R1Addr}]]},
    R2 = region{node_id: N2Id, blocks: [memory, [block{base:R2Addr}]]},
    Size is 2 ^ Bits - 1,
    common_free_buffer([memory, [Size]], R1, R2, ResR, _),
    ResR = region{node_id: ResRId, blocks: [memory, [block{base:ResRAddr}]]},
    get_or_alloc_node_enum(ResRId, ResEnum),
    mark_range_in_use(ResR),
    writeln([name(R1Addr, N1Enum),name(R2Addr, N2Enum),name(ResRAddr, ResEnum)]).

% Find names in N1 and N2 that resolve to ResAddr, then mark those used.
:- export map_common/4.
map_common(Bits, ResRAddr, N1Enum, N2Enum)  :-
    enum_node_id(N1Enum, N1Id),
    enum_node_id(N2Enum, N2Id),
    R1 = region{node_id: N1Id, blocks: [memory, [block{base:R1Addr}]]},
    R2 = region{node_id: N2Id, blocks: [memory, [block{base:R2Addr}]]},
    Size is 2 ^ Bits - 1,
    ResR = region{blocks: [memory, [block{base:ResRAddr}]]},
    common_free_map([memory, [Size]], R1, R2, ResR, _),
    ResR = region{node_id: ResRId},
    get_or_alloc_node_enum(ResRId, ResEnum),
    mark_range_in_use(R1),
    mark_range_in_use(R2),
    writeln([name(R1Addr, N1Enum),name(R2Addr, N2Enum),name(ResRAddr, ResEnum)]).


% Translate a name from one Node to a name to another node, so they
% resolve to the same ressource.
:- export change_view/2.
change_view(Name1, Name2) :- 
    resolve(Name1, D),
    resolve(Name2, D).

test_change_view(pci) :- 
    init, add_pci, add_process,
    Proc = name{node_id: ["OUT", "PROC0", "PROC0"], address: [memory, [0]]},
    Pci = name{node_id: ["OUT", "PCI0", "PCI0"]},
    change_view(Proc, Pci).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Bit Array Representation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

array_all_eq([], _).
array_all_eq([A | Bs], A) :- array_all_eq(Bs,A).
array_all_eq(Arr, A) :- array_list(Arr, ArrLi), array_all_eq(ArrLi, A).

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

% As of now, block remappable implies that the node is one dimension
:- dynamic node_block_conf/3. %(NodeId, VPN, PPN).
:- dynamic node_block_meta/3. %(NodeId, BlockSizeBits, OutNodeId)

% Translate using Block Conf. Same signature as node_translate
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
    
    % Stich together PA
    assert_word(PAW, 48), % TODO bit size for physical address?
    subword(PAW, VAOffsetW, 1 .. 21), 
    subword(PAW, PPNW, 22 .. 48),
    word_to_num(PPNW, PPN),

    word_to_num(PAW, PAddr).

test_node_translate_block :-
    assert(node_block_conf([], 0, 1)),
    assert(node_block_meta([], 21, ["OUT"])),
    node_translate_block([], [memory,[1000]], ["OUT"], [memory, [2098152]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Routing 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

one_block_upper_limit(Name, Limit) :-
    Name = name{
        node_id: NodeId,
        address: Address
    },
    (( % Block remappable?
      node_block_meta(NodeId, BlockSizeBits, _),
      Address = [K, [A]],
      Limit = [K, [ILimit]],
      assert_word(AW, 48),
      word_to_num(AW, A),
      assert_word(LimitW, 48), % TODO bit size for physical address?
      UpperStartBit is BlockSizeBits + 1,
      subword(AW, UpperW, UpperStartBit .. 48), 
      subword(LimitW, UpperW, UpperStartBit .. 48),
      subword(LimitW, LowerW, 1 .. BlockSizeBits),
      array_all_eq(LowerW, 1),
      word_to_num(LimitW, ILimit)
    ) ; (
      % In a translate block?
      node_translate_dyn(NodeId, Block, _, _),
      address_match(Address, Block),
      block_limit_address(Block, Limit)
    ) ; (
      node_overlay(NodeId, _),
      Limit = [memory, [281474976710656]] % Default limit
    )).

test_one_block_upper_limit :-
    assert(node_translate_dyn(
        ["In"], [memory, [block{base:1000,limit:2000}]],
        ["Out1"], [memory, [block{base:0,limit:1000}]])),
    one_block_upper_limit(name{node_id:["In"],address:[memory, [1400]]}, Limit),
    writeln(Limit),

    assert(node_block_meta(["In1"], 21, ["Out1"])),
    one_block_upper_limit(name{node_id:["In1"],address:[memory, [1400]]}, Limit2),
    writeln(Limit2).

% Internal function for region route

% Route Step for names
route_step(SrcName, NextName, Route) :-
    SrcName = name{},
    NextName = name{},
    ((
        translate(SrcName, NextName),
        Route = []
    ) ; (
        % In this case, we can assume, there is no block map existing,
        % But, we have to check if the node supports block mapping, then
        % we can install this
        can_translate(SrcName, NextName, Config),
        Route = [Config]
    )).

% Route Step for regions
route_step(SrcRegion, NextRegion, Route) :-
    % This will only work, if the source region will translate to the same node
    % block this should be ensured by the block splitting of the region route
    region_base_name(SrcRegion, SrcBase),
    region_limit_name(SrcRegion, SrcLimit),
    route_step(SrcBase, NextBase, Route1),
    route_step(SrcLimit, NextLimit, Route2),
    region_base_name(NextRegion, NextBase),
    region_limit_name(NextRegion, NextLimit),
    union(Route1, Route2, Route).

% Routing functionality for ranges addresses (represented as region)
% This one 
route(SrcRegion, DstRegion, Route) :-
    SrcRegion = region{node_id: SrcNodeId, blocks: SrcBlocks},
    DstRegion = region{node_id: DstNodeId, blocks: DstBlocks},
    block_base_address(SrcBlocks, SrcBase), % SrcBase = [memory, [0,1,2]]
    block_limit_address(SrcBlocks, SrcLimit), % SrcLimit = [memory, [10,11,12]]
    block_base_address(DstBlocks, DstBase),
    block_limit_address(DstBlocks, DstLimit),
    one_block_upper_limit(name{node_id:SrcNodeId, address:SrcBase}, BlockLimit),
    % BlockLimit = [memory, [2000]]
    ((
        address_gte(SrcLimit , BlockLimit),
        % Great, SrcRegion fits completly in translate block
        route_step(SrcRegion, NextRegion, R1),
        ( accept(NextRegion) -> (
            DstRegion = NextRegion,
            Route = R1
        ) ; (
            route(NextRegion, DstRegion, R2),
            union(R1, R2, Route)
        ))
    ) ; ( 
        % Only allow this if the block has to be split
        not(address_gte(SrcLimit , BlockLimit)),

        % Route first block
        block_base_address(NewSrcBlocks, SrcBase), % Keep the base
        block_limit_address(NewSrcBlocks, BlockLimit), 

        block_base_address(NewDstBlocks, DstBase), % Keep the base
        address_sub(BlockLimit, SrcBase, BlockSize),
        address_add(DstBase, BlockSize, NewDstLimit),
        block_limit_address(NewDstBlocks, NewDstLimit), 
        route(
            region{node_id:SrcNodeId, blocks: NewSrcBlocks},
            region{node_id:DstNodeId, blocks: NewDstBlocks},
            R1), 

        % Construct remainder
        address_add_const(BlockLimit, 1, AfterBlock),
        block_base_address(RemSrcBlocks, AfterBlock), 
        block_limit_address(RemSrcBlocks, SrcLimit),  % keep limit

        address_add_const(NewDstLimit, 1, AfterDstLimit), 
        block_base_address(RemDstBlocks, AfterDstLimit), 
        block_limit_address(RemDstBlocks, DstLimit), % keep limit
        route(
            region{node_id:SrcNodeId, blocks: RemSrcBlocks},
            region{node_id:DstNodeId, blocks: RemDstBlocks},
            R2),

        % Concat routes
        union(R1, R2, Route)
    )).
        
    
% Routing functionality for single addresses (represented as name)
route(SrcName, DstName, Route) :-
    SrcName = name{},
    DstName = name{},
    route_step(SrcName, NextName, R1),
    ( accept(NextName) -> (
        DstName = NextName,
        Route = R1
    ) ; (
        route(NextName, DstName, R2),
        union(R1, R2, Route)
    )).

route_and_install(SrcName, DstName, Route) :-
    route(SrcName,DstName, Route),
    term_variables(Route, RouteVars),
    labeling(RouteVars),
    install_route(Route).


test_route(Route) :-
    init, add_pci, add_process,
    MMU_IN_ID = ["IN", "MMU0", "PROC0"],
    MMU_OUT_ID = ["OUT", "MMU0", "PROC0"],
    DRAM_ID = ["DRAM"],
    retract(node_translate_dyn(MMU_IN_ID,_,_,_)), % Make sure there is no route
    assert(node_block_meta(MMU_IN_ID, 21, MMU_OUT_ID)),
    alloc_range(DRAM_ID, [memory, [1024]], FreeDRAM),
    SrcName=name{node_id: MMU_IN_ID, address: [memory, [0]]},
    DstName=name{node_id: DRAM_ID, address: FreeDRAM},
    writeln(("Routing from", SrcName, " to ", DstName)),
    route(SrcName, DstName, Route),
    term_variables(Route, RouteVars),
    labeling(RouteVars),
    writeln(("Calculated", Route)).

test_route2 :-
    init, add_pci, add_process,
    MMU_IN_ID = ["IN", "MMU0", "PROC0"],
    MMU_OUT_ID = ["OUT", "MMU0", "PROC0"],
    DRAM_ID = ["DRAM"],
    retract(node_translate_dyn(MMU_IN_ID,_,_,_)), % Make sure there is no route
    assert(node_block_meta(MMU_IN_ID, 21, MMU_OUT_ID)),
    alloc_range(DRAM_ID, [memory, [1024]], FreeDRAM),
    SrcName1=name{node_id: MMU_IN_ID, address: [memory, [0]]},
    DstName1=name{node_id: DRAM_ID, address: FreeDRAM},
    writeln(("Routing from", SrcName1, " to ", DstName1)),
    route_and_install(SrcName1, DstName1, Route1),
    writeln(("Calculated (1)", Route1)),
    SrcName2=name{node_id: MMU_IN_ID, address: [memory, [0]]},
    DstName2=name{node_id: DRAM_ID, address: FreeDRAM},
    writeln(("Routing from", SrcName2, " to ", DstName2)),
    route_and_install(SrcName2, DstName2, Route2),
    writeln(("Calculated (2) ", Route2)).


test_route_region_small :-
    assert(node_block_meta(["IN"], 21, ["OUT"])),
    assert(node_accept(["OUT"], [memory, [block{base:0, limit:100000}]])),
    route(
        region{node_id:["IN"], blocks:[memory,[block{base:0, limit:1000}]]},
        region{node_id:["OUT"], blocks:[memory,[block{base: 0, limit: 1000}]]},
        Route),
    term_variables(Route, RouteVars),
    labeling(RouteVars),

    % check if it worked
    member((["IN"], 0, 0), Route),
    writeln(("Calculated", Route)).

test_route_region_two :-
    Limit = 4194302, % 0x1fffff * 2
    assert(node_block_meta(["IN"], 21, ["OUT"])),
    assert(node_accept(["OUT"], [memory, [block{base:0, limit:4194303}]])),
    route(
        region{node_id:["IN"], blocks:[memory,[block{base:0, limit:Limit}]]},
        region{node_id:["OUT"], blocks:[memory,[block{base: 0, limit: Limit}]]},
        Route),
    term_variables(Route, RouteVars),
    labeling(RouteVars),

    member((["IN"], 0, 0), Route),
    member((["IN"], 1, 1), Route),
    writeln(("Calculated", Route)).


test_route_all :-
    test_route_region_small,
    writeln("region small: passed"),
    test_route_region_two,
    writeln("region two: passed").


% It is possible, to remap SrcName to DstName using the block remapping?
can_translate(SrcName, DstName, (SrcNodeId, (VPN, PPN))) :-
    name{ node_id: SrcNodeId, address: [K, [ISrcAddr]] } = SrcName,
    name{ node_id: DstNodeId, address: [K, [IDstAddr]] } = DstName,
    node_block_meta(SrcNodeId, BlockSizeBits, DstNodeId),
    split_vaddr(ISrcAddr, BlockSizeBits, [VPN, Offset]),
    split_vaddr(IDstAddr, BlockSizeBits, [PPN, Offset]),
    not(node_block_conf(SrcNodeId, VPN, _)). % No mapping with this VPN

:- export install_route/1.
install_route([]).

install_route([null | Ri]) :-
    install_route(Ri).

install_route([(NodeId, VPN, PPN) | Ri]) :-
    assert(node_block_conf(NodeId, VPN, PPN)),
    install_route(Ri).

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% X86 Page table configurable nodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% node_pt stores the root page table for per node
:- dynamic node_pt/3. % NodeId, RootPt, OutNodeId
:- dynamic pt/3. % PtIndex, Offset, NextPtIndex
:- export pt/3.

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

split_vaddr(VA, BlockSizeBits, [VPN, Offset]) :-
    assert_word(VAW, 48),
    word_to_num(VAW, VA),
    subword(VAW, OffsetW, 1 .. BlockSizeBits),
    word_to_num(OffsetW, Offset),
    VPNWStart is BlockSizeBits + 1,
    subword(VAW, VPNW, VPNWStart .. 48),
    word_to_num(VPNW, VPN).

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
    % PtDelta = [pt(2, 0, 23), pt(1, 0, 2), pt(0, 0, 1)] modulo reordering.

% For a list of PTs, assert the facts.
:- export assert_pt_delta/1.
assert_pt_delta(PtDelta) :-
    (foreach(X, PtDelta) do
        assert(X)
    ).

:- export route_node_ids/2.
route_node_ids([], []).
route_node_ids([(NodeId,_) | As], Res) :-
    route_node_ids(As,Res1),
    union([NodeId], Res1, Res).

:- export route_conf_for_id/3.
route_conf_for_id([], _, []).
route_conf_for_id([(NodeId,Conf) | As], NodeId, Res) :-
    route_conf_for_id(As, NodeId, Res1),
    union([Conf],Res1, Res).

route_conf_for_id([_ | As], NodeId, Res) :-
    route_conf_for_id(As, NodeId, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Big tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- export test_all/0.
test_all :-
    init, add_pci, add_process,
    BUFFER_SIZE = [memory, [1024]],
    ProcId = ["OUT", "PROC0", "PROC0"],
    Proc = region{node_id: ProcId},
    PciId = ["OUT", "PCI0", "PCI0"],
    Pci = region{node_id: PciId},
    common_free_buffer(BUFFER_SIZE, Proc, Pci, Resolved, Route),
    writeln(("Free buffer reachable from Proc and Pci found", Resolved)),
    writeln(("View from PCI ", Pci)),
    writeln(("View from Proc ", Proc)),
    writeln(("Using Block-Configuration ", Route)),
    route_node_ids(Route, NodeIds),
    (foreach(NodeId, NodeIds),param(Route) do
        route_conf_for_id(Route, NodeId, BlockDelta),
        pt_delta(NodeId, BlockDelta, PtDelta),
        writeln(("New PT entries for ", NodeId, " -> ", PtDelta))
    ),
    writeln("Installing Route"),
    install_route(Route),
    !, % make sure not to back track past the state modifying install_route
    region_base_name(Proc,ProcBaseAddr), % The first byte of the common region.
    PciBaseAddr = name{node_id:PciId},
    change_view(ProcBaseAddr, PciBaseAddr),
    writeln(("Lookup from Proc to PCI BaseAddr", PciBaseAddr)).

:- export test_mem_if/0.
test_mem_if :-
    add_pci_alloc(addr(0,1,2)),
    pci_address_node_id(addr(0,1,2), PciEnum),
    add_process_alloc(proc(0)),
    process_node_id(proc(0), ProcEnum),
    alloc_common(21, PciEnum, ProcEnum).


:- export dec_net_debug/0.
dec_net_debug :- listing.
