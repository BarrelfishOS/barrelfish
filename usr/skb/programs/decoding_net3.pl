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
% NodeId = identifier list 
% IAddr = [1,2,3] 
% Addr = [kind, 1]
% IBlock block{..}
% Block = [kind, block{..}]

:- module(decoding_net3).

:- use_module(allocator3).
:- use_module(decoding_net3_state).


%%% Bottom layer is storing the following facts in the State
% accept(Region)
% mapping(SrcRegion, DstName)
% overlay(SrcNodeId, OutNodeId)
% block_meta(NodeId, Bits, OutNodeId)  -- Metadata for block reconfigurable nodes
% block_conf(NodeId, VPN, PPN)         -- For block reconfigurable nodes
% in_use(NodeId, Block)                -- Subset of accepted ranges that has been allocated

state_valid([]).
state_valid([accept(_) | As]) :- state_valid(As).
state_valid([mapping(_,_) | As]) :- state_valid(As).
state_valid([overlay(_,_) | As]) :- state_valid(As).
state_valid([block_meta(_,_,_) | As]) :- state_valid(As).
state_valid([block_conf(_,_,_) | As]) :- state_valid(As).
state_valid([in_use(_,_) | As]) :- state_valid(As).

:- export struct(block(base,limit)).
:- export struct(region(node_id,blocks)).
:- export struct(name(node_id,address)).

:- lib(ic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Utilities for building the model layer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inf_value(9223372036854775808).

% TODO: Works only for one dimension.
% ScanPoints is a list of points where the scanline (scanhyperplane?) should stop.
scan_points(S, NodeId, ScanPoints) :-
    Reg = region{node_id: NodeId},
    findall(Reg, state_query(S, mapping(Reg, _)), RegLi),
    (foreach(Reg, RegLi), fromto([0], In, Out, Ptz) do 
        Reg = region{blocks: [_, block{base: B, limit: L}]},
        LP is L + 1,
        append(In, [B,LP], Out)
    ),
    inf_value(Inf),
    append(Ptz,[Inf], ScanPoints).

% Max is bigger than Min and Max is smaller than all the bigger mapping bases
max_not_translated_pt(S, NodeId, Min, Max) :-
    Reg = region{node_id: NodeId},
    % Make sure Min is not in any Mapping.
    not(state_query(S, mapping(region{node_id:NodeId,blocks:[_,block{base:Min}]}, _))),
    inf_value(Inf),
    findall(Reg, state_query(S, mapping(Reg, _)), RegLi),
    (foreach(Reg, RegLi), param(Min), fromto(Inf, MaxIn, MaxOut, MaxMatch) do 
        Reg = region{blocks: [_, block{base: B}]},
        (
            ( B =< Min, MaxOut=MaxIn ) ; 
            ( min(MaxIn, B, MaxOut) )
        )
    ),
    Max is MaxMatch - 1,
    Max > Min.
    

:- export test_scan_points/0.
test_scan_points :-
    S = [
        mapping(
        region{node_id: ["IN"], blocks: [memory, block{base:100, limit:200}]},
        name{node_id: ["OUT"], address: [memory, 1]}),
        mapping(
        region{node_id: ["Dummy"], blocks: [memory, block{base:7, limit:77}]},
        name{node_id: ["OUT"], address: [memory, 1]})
        ],
    scan_points(S, ["IN"], Points),
    member(0, Points),
    member(100, Points),
    member(201, Points),
    not(member(7, Points)),
    not(member(77, Points)).

:- export test_max_not_translated_pt/0.
test_max_not_translated_pt :-
    S = [
        mapping(
        region{node_id: ["IN"], blocks: [memory, block{base:100, limit:200}]},
        name{node_id: ["OUT"], address: [memory, 1]}),
        mapping(
        region{node_id: ["Dummy"], blocks: [memory, block{base:7, limit:77}]},
        name{node_id: ["OUT"], address: [memory, 1]})
        ],
    scan_points(S, ["IN"], Points),
    max_not_translated_pt(S, ["IN"], 0, 99),
    not(max_not_translated_pt(S, ["IN"], 100, _)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Model layer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

translate(S, SrcRegion, DstBase) :-
    state_query(S, mapping(SrcRegion, DstBase)).

% Transform the overlays into translate, but only where they don't match
% an existing translate.
translate(S, SrcRegion, DstBase) :-
   SrcRegion = region{node_id:SrcNodeId},
   state_query(S, overlay(SrcNodeId, OverlayDest)), 
   scan_points(S, SrcNodeId, ScanPoints),
   member(Base, ScanPoints),
   max_not_translated_pt(S, SrcNodeId, Base, Limit),
   SrcRegion = region{blocks:[memory, block{base: Base, limit: Limit}]},
   DstBase = name{node_id:OverlayDest, address: [memory, Base]}.

:- export test_translate/0.
test_translate :- 
    %Setup
    S = [
        mapping(
            region{node_id:["In"], blocks:[memory, block{base:1000,limit:2000}]},
            name{node_id: ["Out1"], address: [memory, 0]}),
        overlay(["In"], ["Out2"])
      ],
    Src = region{node_id:["In"]},
    %findall((Src,Dest), translate(S, Src, Dest), Li),
    %(foreach((Src,Dest), Li) do
    %    printf("Src=%p, Dest=%p\n", [Src,Dest])
    %),
    translate(S, 
        region{node_id:["In"], blocks:[memory, block{base:1000,limit:2000}]},
        name{node_id: ["Out1"], address: [memory, 0]}),
    translate(S, 
        region{node_id:["In"], blocks:[memory, block{base:0,limit:999}]},
        name{node_id: ["Out2"], address: [memory, 0]}),
    inf_value(Inf), Inf1 is Inf - 1,
    translate(S, 
        region{node_id:["In"], blocks:[memory, block{base:2001,limit:Inf1}]},
        name{node_id: ["Out2"], address: [memory, 2001]}).



%%%%% This is the old stricter "does not translate" predicate
%%%does_not_translate(NodeId, [AKind,IAddr]) :-
%%%    %TODO take node_translate_block into account
%%%    findall(B, node_translate_dyn(NodeId, B, _, _), Blocks),
%%%    (foreach([AKind, IBlock], Blocks),param(IAddr),param(AKind) do
%%%        iblocks_nomatch(IAddr, IBlock)
%%%    ).
%%%
%%%test_does_not_translate :-
%%%    assert(node_translate_dyn(
%%%        ["In"], [memory, [block{base:1000,limit:2000}]],
%%%        ["Out1"], [memory, [block{base:0,limit:1000}]])),
%%%    does_not_translate(["In"], [memory, [500]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iaddress_aligned(A, Bits) :-
    BlockSize is 2^Bits,
    BlockNum #>= 0,
    A #= BlockNum * BlockSize.

address_aligned([_, IAddress], Bits) :-
    iaddress_aligned(IAddress, Bits).

name_aligned(Name, Bits) :-
    name{address: Addr} = Name,
    address_aligned(Addr, Bits).

:- export test_alignment/0.  
test_alignment :-
    536870912 #< IAddr,
    iaddress_aligned(IAddr, 21),
    labeling([IAddr]),
    IAddr = 538968064.

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
    iblock_match(IAddr, IBlocks).

address_match_region(Addr, region{blocks:Blocks}) :-
    address_match(Addr, Blocks).

iaddress_gt(S, B) :-
    S #< B.

% Will only compare addresses of the same kind
address_gt([K, ISmaller], [K, IBigger]) :-
    iaddress_gt(ISmaller, IBigger).

% Will only compare addresses of the same kind
address_gte([K, ISmaller], [K, IBigger]) :-
    ISmaller #=< IBigger.

% A - B = C ---> address_sub(A,B,C)
address_sub([K, IA], [K, IB], [K, IC]) :-
    IC is IA - IB. 

% A + B = C ---> address_add(A,B,C)
address_add([K, IA], [K, IB], [K, IC]) :-
    IC is IA + IB.

% A + B = C ---> address_add(A,B,C)
address_add_const_ic([K, IA], B, [K, IC]) :-
    IC #= IA + B.

% A + B = C ---> address_add(A,B,C)
address_add_const([K, IA], B, [K, IC]) :-
    IC is IA + B.

address_var([K, IA]) :-
    var(K) ; var(IA).

iblock_iaddress_gt(Block, Addr) :-
    block{
        limit: Limit
    } = Block,
    Addr #> Limit.

block_address_gt([K, IBlocks], [K, IAddress]) :-
    iblock_iaddress_gt(IBlocks, IAddress).


block_block_contains([K, A], [K, B]) :-
    A = block{base:ABase, limit: ALimit},
    B = block{base:BBase, limit: BLimit},
    ABase >= BBase,
    ABase =< BLimit,
    ALimit >= BBase,
    ALimit =< BLimit.

% region_region_contains(A,B) --> A is part of B
region_region_contains(region{node_id:N, blocks:AB}, region{node_id:N, blocks:BB}) :-
    block_block_contains(AB, BB).

iblock_iblock_intersection(A, B, I) :-
    A = block{base:ABase, limit: ALimit},
    B = block{base:BBase, limit: BLimit},
    % Case 4: B contained entirely in B.
    (((ABase =< BBase, BLimit =< ALimit) -> I = B) ;
    (
    % Case 1: A contained entirely in B.
    (((BBase =< ABase, ALimit =< BLimit) -> I = A) ;
    (
        % Case 2: B overlaps on the right of A. BBase in A.
        (ABase =< BBase, BBase =< ALimit, I = block{base: BBase, limit: ALimit}) ;

        % Case 3: B overlaps on the left of A. BLimit in A
        (ABase =< BLimit, BLimit =< ALimit, I = block{base: ABase, limit: BLimit})
    )))).

block_block_intersection([K, IABlock], [K, IBBlock], [K, ISBlock]) :-
    iblock_iblock_intersection(IABlock, IBBlock, ISBlock).

region_region_intersection(region{node_id:N, blocks:AB}, region{node_id:N, blocks:BB}, Is) :-
    block_block_intersection(AB, BB, BIs),
    Is = region{node_id: N, blocks: BIs}.

:- export test_region_region_intersection/0.
test_region_region_intersection :-
    A1 = region{node_id:["ID"], blocks:[memory, block{base: 50, limit: 100}]},
    B1 = region{node_id:["ID"], blocks:[memory, block{base: 0, limit: 200}]},
    region_region_intersection(A1,B1,A1),
    A2 = region{node_id:["ID"], blocks:[memory, block{base: 50, limit: 100}]},
    B2 = region{node_id:["ID"], blocks:[memory, block{base: 75, limit: 200}]},
    I2 = region{node_id:["ID"], blocks:[memory, block{base: 75, limit: 100}]},
    region_region_intersection(A2,B2,I2),
    A3 = region{node_id:["ID"], blocks:[memory, block{base: 50, limit: 100}]},
    B3 = region{node_id:["ID"], blocks:[memory, block{base: 0, limit: 75}]},
    I3 = region{node_id:["ID"], blocks:[memory, block{base: 50, limit: 75}]},
    region_region_intersection(A3,B3,I3),
    A4 = region{node_id:["ID"], blocks:[memory, block{base: 0, limit: 100}]},
    B4 = region{node_id:["ID"], blocks:[memory, block{base: 50, limit: 70}]},
    region_region_intersection(A4,B4,B4),
    A5 = region{node_id:["ID"], blocks:[memory, block{base: 0, limit: 100}]},
    B5 = region{node_id:["ID"], blocks:[memory, block{base: 200, limit: 300}]},
    not(region_region_intersection(A5,B5,_)).


% Calculates PartSrcRegion and PartSrc Name, such that PartSrcRegion is the 
% intersection between Src and FullSrcRegion.
intersecting_translate_block(Src, FullSrcRegion, FullSrcName, PartSrcRegion, PartSrcName) :-
    Src = region{}.

% Turn the limit of the blocks into an address
block_limit_address([K, Block], [K, Addr]) :-
    block{
        limit: Addr
    } = Block.

% Turn the base of the blocks into an address
block_base_address([K, Block], [K, Addr]) :-
    (var(IBlocks), var(IAddress), fail) ; 
    block{
        base: Addr
    } = Block.

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

block_size([K, A], [K, B]) :-
    block{
        base: Base,
        limit: Limit
    } = A,
    (
        (var(Limit), Limit is Base + B - 1) ;
        (B is Limit - Base + 1)
    ).

region_size(Region, Size) :-
    region{ blocks: Blocks } = Region,
    block_size(Blocks, Size).

regions_size([], 0).
regions_size([Region | Regions], Size) :-
    region_size(Region, [_, A]),
    regions_size(Regions, B),
    Size is A + B.

iaddr_iblock_map(SrcAddr, SrcBlock, DstAddr, DstBase) :-
    SrcBlock = block{base:SrcBase},
    DstAddr #= SrcAddr - SrcBase + DstBase.

test_iaddr_iblock_map :-
    iaddr_iblock_map([1],[block{base:0, limit:1024}], Dst, [100]),
    Dst = [101].

%% Convert from region (encoded as block) to names
region_name_match(Region,Name) :-
    region{
        node_id:Id,
        blocks: Blocks % Blocks = [Kind, block{...}]
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

addr_to_blocks([K, IAddr], [K, IBlocks]) :-
    iaddr_to_iblock_one(IAddr, IBlocks).

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



% block_translate(A,BaseA,B,BaseB) ==> A-BaseA = B-BaseB
block_translate([SrcK, ISrcAddr], [SrcK, SrcBlock], [DstK, IDstAddr], [DstK, IDstBase]) :-
    iaddr_iblock_map(ISrcAddr, SrcBlock, IDstAddr, IDstBase).

test_block_translate :-
    block_translate(
        [memory,[1]],
        [memory, [block{base:0, limit:1024}]],
        Dst,
        [memory, [100]]),
    Dst = [memory, [101]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Queries (that query the model layer)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


accept_name(S, Name) :-
    name{
        node_id:NodeId,
        address:Addr
    } = Name,
    CandidateRegion = region{node_id: NodeId},
    state_query(S, accept(CandidateRegion)),
    address_match_region(Addr, CandidateRegion).


accept_region(S, Region) :-
    Region = region{node_id: RId},
    CandidateRegion = region{node_id: RId},
    state_query(S, accept(CandidateRegion)),
    region_region_contains(Region, CandidateRegion).

accept_regions(S, []).
accept_regions(S, [R | Rs]) :-
    accept_region(S, R),
    accept_regions(S, Rs).

test_accept_name :-
    S = [accept(region{node_id:["In"], blocks: [memory, block{base: 50, limit:100}]})],
    accept_name(S, name{node_id:["In"], address: [memory, 75]}).

test_accept_region :-
    S = [accept(region{node_id:["In"], blocks: [memory, block{base: 50, limit:100}]})],
    accept_region(S, region{node_id:["In"], blocks: [memory, block{base:75, limit:80}]}).

decode_step_name(S, SrcName, name{node_id: DstId, address: DstAddr}) :-
    translate(S, SrcRegion, name{node_id: DstId, address: DstBaseAddr}),
    region_name_match(SrcRegion, SrcName),
    SrcRegion = region{blocks:SrcBlocks},
    SrcName = name{address:SrcAddr},
    block_translate(SrcAddr, SrcBlocks, DstAddr, DstBaseAddr).

%% We represent holes in the resolved set as region with node_id = hole
hole_region(region{node_id: hole, blocks: Bs}, region{blocks: Bs}).
is_hole(R) :- R = region{node_id: hole}.

%% Translate SrcRegion into DstRegion using the InCandidate :: region and
%% OutCandidate :: name, 
%% Only works if the SrcRegion is completly contained in the InCandidate.
decode_step_region_matching(S, InCandidate, OutCandidate, SrcRegion, DstRegion) :-
    region_region_contains(SrcRegion, InCandidate),
    region_base_name(SrcRegion, name{address:SrcAddr}),
    InCandidate = region{blocks:InBlocks},
    OutCandidate = name{node_id: OutNodeId, address: DstBaseAddr},
    block_translate(SrcAddr, InBlocks, DstAddr, DstBaseAddr),
    region_base_name(DstRegion, name{node_id: OutNodeId, address: DstAddr}),
    region_size(SrcRegion, Size),
    region_size(DstRegion, Size).

% This recursion iterates over all translate blocks. If they match
% the srcRegion, it will split the hole (which has to be found at this
% point in DstCurr)
decode_step_region_part(_, _, X, X, []).
decode_step_region_part(S, SrcRegion, DstCurr, DstEnd, [(In,Out) | Tlx]) :-
    (foreach(Dst, DstCurr), param(SrcRegion), param(In), param(Out), param(S),
     fromto([], NewDstIn, NewDstOut, DstNext) do
        Dst = region{node_id: hole, blocks: DstBlocks},
        SrcRegion = region{node_id: SrcRegionId},
        DstInSrc = region{node_id: SrcRegionId, blocks: DstBlocks},
        region_region_intersection(In, DstInSrc, Is) -> (
            Is = region{blocks: [_, block{base:IsB, limit: IsL}]},
            SrcRegion = region{blocks: [_, block{base:SrcB, limit: SrcL}]},
            IsBBefore is IsB - 1,
            IsLAfter is IsL + 1,
            HoleLeft = region{node_id: hole, blocks: [_, block{base: SrcB, limit: IsBBefore}]},
            decode_step_region_matching(S, In, Out, Is, Overlap),
            HoleRight = region{node_id: hole, blocks: [_, block{base: IsLAfter, limit: SrcL}]},
            append(NewDstIn, [HoleLeft, Overlap, HoleRight], NewDstOut)
        ) ; (
            append(NewDstIn, [Dst], NewDstOut)
        )   
    ),
    decode_step_region_part(S, SrcRegion, DstNext, DstEnd, Tlx).

decode_step_region_new(S, SrcRegion, NextRegions) :-
    findall((In,Out), translate(S, In, Out), Tlx),
    hole_region(SrcHole, SrcRegion),
    decode_step_region_part(S, SrcRegion, [SrcHole], NextRegionsTmp, Tlx),
    (foreach(Reg, NextRegionsTmp),
     fromto([], NewDstIn, NewDstOut, NextRegions) do
     ((not(is_hole(Reg)) -> append(NewDstIn, [Reg], NewDstOut)) ; (
        Reg = region{blocks: [_, block{base:B, limit: L}]},
        (L >= B) -> append(NewDstIn, [Reg], NewDstOut) ; NewDstOut = NewDstIn
     )
    )).


:- export test_decode_step_region_new/0.
test_decode_step_region_new :-
    % The simple case: everything falls into one translate block
    S = [
        mapping(
        region{node_id: ["IN"], blocks: [memory, block{base:0, limit:100}]},
        name{node_id: ["OUT1"], address: [memory, 10]}),
        mapping(
        region{node_id: ["IN"], blocks: [memory, block{base:200, limit:300}]},
        name{node_id: ["OUT2"], address: [memory, 200]})
        ],

    decode_step_region_new(S,
        region{node_id:["IN"], blocks: [memory, block{base:50, limit: 70}]},
        Out1),
    Out1 = [region{node_id:["OUT1"], blocks: [memory, block{base:60, limit: 80}]}],

    decode_step_region_new(S,
        region{node_id:["IN"], blocks: [memory, block{base:50, limit: 199}]},
        Out2),
    Out2 = [region{node_id:["OUT1"], blocks: [memory, block{base:60, limit: 110}]},
            Hole2],
    region_size(Hole2, [_, 99]),

    decode_step_region_new(S,
        region{node_id:["IN"], blocks: [memory, block{base:50, limit: 200}]},
        Out3),
    Out3 = [
        region{node_id:["OUT1"], blocks: [memory, block{base:60, limit: 110}]},
        _,
        region{node_id:["OUT2"], blocks: [memory, block{base:200, limit: 200}]}
        ],
    decode_step_region_new(S,
        region{node_id:["IN"], blocks: [memory, block{base:150, limit: 350}]},
        Out4),
    Out4 = [_, region{node_id:["OUT2"], blocks: [memory, block{base:200, limit: 300}]}, _].
     

% TODO: this currently only considers the case when SrcRegion fits entirely in 
% one translate src block.
decode_step_region(S, SrcRegion, NextRegions) :-
    translate(S, InCandidate, OutCandidate),
    region_region_contains(SrcRegion, InCandidate),
    region_base_name(SrcRegion, name{address:SrcAddr}),
    InCandidate = region{blocks:InBlocks},
    OutCandidate = name{node_id: OutNodeId, address: DstBaseAddr},
    block_translate(SrcAddr, InBlocks, DstAddr, DstBaseAddr),
    region_base_name(DstRegion, name{node_id: OutNodeId, address: DstAddr}),
    region_size(SrcRegion, Size),
    region_size(DstRegion, Size),
    NextRegions = [DstRegion].


% Like decode_step_region, but consider additional configuration entries.
% TODO: Only works if SrcRegion matches exactly a Configuration block.
% This function uses IC internally,but labels the outputs.
decode_step_region_conf_one(S, SrcRegion, DstRegion, block_conf(SrcId, VPN, PPN)) :-
    SrcRegion = region{node_id: SrcId, blocks: [Kind, block{base: SrcB, limit: SrcL}]},
    state_query(S, block_meta(SrcId, Bits, OutNodeId)),
    DstRegion = region{node_id: OutNodeId, blocks: [Kind, block{base: DestB, limit: DestL}]},
    RSize is SrcL - SrcB + 1,
    RSize is 2^Bits,
    split_vaddr(SrcB, Bits, [VPN, Offset]),
    split_vaddr(DestB, Bits, [PPN, Offset]),
    DestL #= DestB + RSize - 1,
    labeling([PPN, VPN]).

decode_step_region_conf(S, SrcRegion, DstRegions, Confs) :-
    % TODO: WIP
    SrcRegion = region{node_id: SrcId, blocks: [Kind, block{base: SrcB, limit: SrcL}]},
    state_query(S, block_meta(SrcId, Bits, OutNodeId)),
    Size is 2^Bits,
    split_region(SrcRegion, Size, SplitSrc),
    (foreach(Src, SplitSrc),
     fromto([],DstIn,DstOut,DstRegions),
     fromto([],ConfIn,ConfOut,Confs),
     param(S) do
        decode_step_region_conf_one(S, Src, Dst, Conf),
        append(DstIn, [Dst], DstOut),
        append(ConfIn, [Conf], ConfOut)
    ).

split_region(Region, Size, Splits) :-
    % TODO IMPLEMENT ME
    Splits = [Region].

:- export test_split_region/0.
test_split_region :-
    InR = region{node_id:["IN"], blocks: [memory, block{base:0, limit: 8}]},
    Size = 4,
    split_region(InR, Size, Out).

:- export test_decode_step_region_conf_one/0.
test_decode_step_region_conf_one :-
    S = [block_meta(["IN"], 21, ["OUT"])],
    Base = 0,
    Limit is Base + 2^21 - 1,
    SrcRegion = region{node_id: ["IN"], blocks: [memory, block{base:Base, limit:Limit}]},
    decode_step_region_conf_one(S, SrcRegion, Out1, Conf1),
    %printf("Out1 (free)=%p, Conf1=%p\n",[Out1, Conf1]),
    TestBase is 512 * 2^21,
    Out2 = region{node_id:["OUT"], blocks: [memory, block{base:TestBase}]},
    decode_step_region_conf_one(S, SrcRegion, Out2, Conf2),
    %printf("Out2 (fixed)=%p, Conf2=%p\n",[Out2, Conf2]),
    Conf2 = block_conf(["IN"], 0, 512).

:- export test_decode_step_region_conf2/0.
test_decode_step_region_conf2 :-
    S = [block_meta(["IN"], 21, ["OUT"])],
    Base = 0,
    Limit is Base + 2^22 - 1, % Note the second 2 in 22, this remaps two blocks
    SrcRegion = region{node_id: ["IN"], blocks: [memory, block{base:Base, limit:Limit}]},
    decode_step_region_conf(S, SrcRegion, [Out1], Conf1),
    printf("Out1 (free)=%p, Conf1=%p\n",[Out1, Conf1]).
    %TestBase is 512 * 2^21,
    %Out2 = region{node_id:["OUT"], blocks: [memory, block{base:TestBase}]},
    %decode_step_region_conf(S, SrcRegion, [Out2], Conf2),
    %%printf("Out2 (fixed)=%p, Conf2=%p\n",[Out2, Conf2]),
    %Conf2 = [block_conf(["IN"], 0, 512)].

decode_step_regions(S, [], []).
decode_step_regions(S, [A | As], Regs) :-
    decode_step_region(S, A, RegsA),
    decode_step_regions(S, As, RegsB),
    append(RegsA, RegsB, Regs).

decode_step_regions_conf(S, [], [], []).
decode_step_regions_conf(S, [A | As], Regs, Conf) :-
    decode_step_region_conf(S, A, RegsA, ConfA),
    decode_step_regions_conf(S, As, RegsB, ConfB),
    append(RegsA, RegsB, Regs),
    append(ConfA, ConfB, Conf).


test_decode_step_region1 :-
    % The simple case: everything falls into one translate block
    S = [
        mapping(
        region{node_id: ["IN"], blocks: [memory, block{base:0, limit:100}]},
        name{node_id: ["OUT"], address: [memory, 1]})
        ],

    decode_step_region(S,
        region{node_id:["IN"], blocks: [memory, block{base:50, limit: 70}]},
        Out),
    Out = [region{node_id:["OUT"], blocks: [memory, block{base:51, limit: 71}]}].

:- export test_decode_step_region2/0.
test_decode_step_region2 :-
    % Complicated case, overlapping translate
    S = [
        mapping(
        region{node_id: ["IN"], blocks: [memory, block{base:0, limit:100}]},
        name{node_id: ["OUT1"], address: [memory, 10]}),
        mapping(
        region{node_id: ["IN"], blocks: [memory, block{base:200, limit:300}]},
        name{node_id: ["OUT2"], address: [memory, 20]}),
        mapping(
        region{node_id: ["IN"], blocks: [memory, block{base:400, limit:500}]},
        name{node_id: ["OUT3"], address: [memory, 30]})
        ],

    decode_step_region(S,
        region{node_id:["IN"], blocks: [memory, block{base:50, limit: 450}]},
        Out),
    printf("decode_step_region returns %p\n", Out).

:- export test_decode_step_name/0.
test_decode_step_name :-
    S = [mapping(
        region{
            node_id: ["IN"],
            blocks: [memory, block{base:0, limit:100}]
        },
        name{node_id: ["OUT"], address: [memory, 1]})],

    decode_step_name(S,
        name{node_id:["IN"], address: [memory, 1]},
        name{node_id:OutNodeId, address: OutAddr}),
    OutNodeId = ["OUT"],
    OutAddr = [memory, 2].

:- export test_decode_step_name2/0.
test_decode_step_name2 :-
    %Setup
    S = [
        mapping(
            region{node_id:["In"], blocks:[memory, block{base:1000,limit:2000}]},
            name{node_id: ["Out1"], address: [memory, 0]}),
        overlay(["In"], ["Out2"])
      ],
    % Test the translate block
    decode_step_name(S,
        name{node_id:["In"], address:[memory, 1000]},
        name{node_id:["Out1"], address: [memory, 0]}
    ),
    % Test the overlay
    decode_step_name(S,
        name{node_id:["In"], address:[memory, 0]},
        name{node_id:["Out2"], address:[memory, 0]}
    ),
    % make sure the upper limit is respected.
    decode_step_name(S,
        name{node_id:["In"], address: [memory, 2500]}, 
        name{node_id:["Out2"], address: [memory, 2500]}), 
    % make sure no within block translation to overlay exists
    not(decode_step_name(S,
        name{node_id: ["In"], address: [memory, 1000]}, 
        name{node_id: ["Out2"], address: [memory, 1000]})).

:- export test_decode_step_name3/0.
test_decode_step_name3 :-
    %Setup
    S = [
        mapping(
            region{node_id:["In"], blocks:[memory, block{base:1000,limit:2000}]},
            name{node_id: ["Out1"], address: [memory, 0]}),
        mapping(
            region{node_id:["In2"], blocks:[memory, block{base:2000,limit:3000}]},
            name{node_id: ["Out1"], address: [memory, 0]}),
        overlay(["In"], ["Out2"])
      ],
    % Test the translate block
    Src = name{node_id:["In"]},
    decode_step_name(S,
        Src,
        name{node_id:["Out1"], address: [memory, 0]}
    ),
    Src = name{node_id:["In"], address: [memory, 1000]}.


% Reflexive, transitive closure of decode_step_*
:- export decodes_name/3.
decodes_name(_, N,N).
decodes_name(S, SrcName, DstName) :-
    decode_step_name(S, SrcName, NextName),
    decodes_name(S, NextName, DstName).

:- export resolve_name/3.
resolve_name(S, SrcName, DstName) :-
    name{} = SrcName,
    name{} = DstName,
    decodes_name(S, SrcName,DstName),
    accept_name(S, DstName).

decodes_regions(_, N, N).
decodes_regions(S, SrcRegions, DstRegions) :-
    decode_step_regions(S, SrcRegions, NextRegions),
    decodes_regions(S, NextRegions, DstRegions).

resolve_regions(S, SrcRegions, DstRegions) :-
    decodes_regions(S, SrcRegions, DstRegions),
    accept_regions(S, DstRegions).

:- export test_resolve_name/0.
test_resolve_name :-
    %Setup
    S = [
        mapping(
            region{node_id:["In"], blocks: [memory, block{base:1000,limit:2000}]},
            name{node_id: ["Out1"], address : [memory, 0]}),
        overlay(["In"], ["Out2"]),
        accept(region{node_id:["Out1"], blocks: [memory,block{base:0, limit:2000}]}),
        accept(region{node_id:["Out2"], blocks: [memory,block{base:0, limit:2000}]})
        ],
    % Hit the translate block
    resolve_name(S,
        name{node_id:["In"], address:[memory, 1000]},
        name{node_id:["Out1"], address:[memory, 0]}),
    % Hit the overlay 
    resolve_name(S,
        name{node_id:["In"], address:[memory, 500]},
        name{node_id:["Out2"], address:[memory, 500]}).

test_resolve_name2 :-
    %Setup
    S = [mapping(
            region{node_id: ["In1"], blocks: [memory, block{base:1000,limit:2000}]},
            name{node_id: ["Out1"], address: [memory, 0]}),
        mapping(
            region{node_id:["In2"], blocks:[memory, block{base:6000,limit:7000}]},
            name{node_id:["Out1"], address: [memory, 0]}),
        accept(region{node_id:["Out1"], blocks: [memory, block{base:0, limit:2000}]})
        ],
    % Reverse lookup
    resolve_name(S,
        name{node_id:["In1"], address:[memory, 1000]},
        R),
    resolve_name(S,
        name{node_id:["In2"], address:Out},
        R),
    Out = [memory, 6000].

test_resolve3(Out) :-
    %Setup
    assert(node_translate_dyn(
        ["In1"], [memory, [block{base:1000,limit:2000}]],
        ["Out1"], [memory, [block{base:0,limit:1000}]])),
    assert(node_translate_dyn(
        ["In2"], [memory, [block{base:6000,limit:7000}]],
        ["Out1"], [memory, [block{base:0,limit:1000}]])),
    assert(node_accept(["Out1"], [memory,[block{base:0, limit:2000}]])),
    InRegion = region{node_id:["In1"], blocks:[memory, block{base:1000, limit:1500}]},
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
:- export enum_node_id/2.
alloc_node_enum(N) :- alloc_one(node_enum, N).

get_or_alloc_node_enum(NodeId, Enum) :-
    enum_node_id(Enum, NodeId) ;
    ( alloc_node_enum(Enum), assert(enum_node_id(Enum, NodeId)) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% VNode Allocator.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic vnode_region/1.

vnode_meta(PageSize, PoolSize) :-
    PageSize is 2 ^ 12, % 4Kb Pages
    PoolSize is 2048.   % Number of pages

% TODO: Test me
vnode_alloc(BaseAddr) :-
    vnode_region(Reg),
    region_base_name(Reg, RegName),
    alloc_one(vnodes, Slot),
    vnode_meta(PageSize,_),
    RegName = name{address: Addr},
    Offset is PagesSize * Slot,
    address_add_const(Addr, Offset, NewAddr),
    NewAddr = [memory, [BaseAddr]].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% X86 Support. Complements the sockeye file, should really be moved into its own file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- export init/0.
:- export add_pci_alloc/1.
:- export add_pci/1.
:- export add_process/1.
:- export add_process_alloc/1.
:- export dram_nodeid/1.
:- export alloc_root_vnodeslot/2.
:- export free_root_vnodeslot/2.

:- dynamic pci_address_node_id/2.
:- export pci_address_node_id/2.
:- dynamic process_node_id/2.
:- export process_node_id/2.

alloc_root_vnodeslot(NodeId, Slot) :-
    alloc_one(root_vnodeslot(NodeId), Tmp),
    Slot is Tmp + 2.

free_root_vnodeslot(NodeId, Slot) :-
    Tmp is Slot - 2,
    free_one(root_vnodeslot(NodeId), Tmp).

dram_nodeid(NodeId) :- NodeId = ["DRAM"].

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


init(NewS) :-
    state_empty(S1),
    add_SYSTEM([]),
    DRAM_ID = ["DRAM"],
    initial_dram_block(Block),
    state_add(S1, accept(["DRAM"], [memory, [Block]]), S2), 
    get_or_alloc_node_enum(S2, DRAM_ID, DRAM_ENUM, S3),
    printf("Decoding net initialized using %p as DRAM. DRAM nodeid: %p\n",
        [Block, DRAM_ENUM]),

    % Manage space for vnodes
    vnode_meta(PageSize, PoolSize),
    VnodePoolSize is PageSize * PoolSize,
    Size = [VnodePoolSize],
    alloc_range(S2, DRAM_ID, [memory, Size], BaseOut, S3),
    mark_range_in_use(S3, DRAM_ID, BaseOut, Size, S4),
    in_use(DRAM_ID, Region),
    assert(vnode_region(Region)),
    writeln("Using for PageTables:"), writeln(Region).

add_pci(S, NewS) :-
    add_pci(S, ["PCI0"], addr(0,0,0), NewS).

iommu_enabled :-
    call(iommu_enabled,0,_)@eclipse.

add_pci(S0, Id, Addr, NewS) :-
    Addr = addr(_,_,_),
    PCIBUS_ID = ["PCIBUS"],
    PCIIN_ID = ["IN" | Id],
    PCIOUT_ID = ["OUT" | Id],
    (iommu_enabled -> (
        add_PCI_IOMMU(S0, Id, S1), 
        % Mark IOMMU block remappable
        state_add(S1, block_meta(["IN", "IOMMU0" | Id], 21, ["OUT", "IOMMU0" | Id]), S2)
        %% And assign a root PT
        %pt_alloc(Root),
        %assert(node_pt(["IN", "IOMMU0", Id], Root, ["OUT","IOMMU0",Id]))
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
     fromto(S3, SIn, SOut, NewS) do
        BarId = [BarNum, "BAR" | Id],
        BarEnd is BarStart + BarSize,
        state_add(SIn, accept(region{
            node_id: BarId,
            blocks: [memory, block{base:BarStart,limit:BarEnd}]}), SIn1),
        state_add(SIn1,
            mapping(region{
                node_id: PCIBUS_ID,
                blocks: [memory,block{base:BarStart,limit:BarEnd}]
            },
            name{
                node_id: BarId,
                address: [memory, BarStart]
            }),
            SOut)
    ).

add_pci_alloc(S, Addr, NewS) :-
    alloc_node_enum(S, Enum, S1),
    add_pci(S1, [Enum], Addr, S2),
    % Set it to the node id where addresses are issued from the PCI device
    OutNodeId = ["OUT", "PCI0", Enum],
    state_add(S2, enum_node_id(Enum, OutNodeId), S3),
    state_add(S3, pci_address_node_id(Addr, Enum), NewS).

add_process_alloc(S, Enum, NewS) :-
    alloc_node_enum(Enum),
    add_process([Enum]),
    % Set it to the node id where addresses are issued from the process
    assert(enum_node_id(Enum, ["OUT", "PROC0", Enum])).
    %assert(process_node_id(ProcId, Enum)).


% Make ID argument if we want to add multiple.
add_process(S, NewS) :-
    add_process(S, ["PROC0"], NewS).

add_process(S, Id, NewS) :-
    DRAM_ID = ["DRAM"],
    add_PROC_MMU(S, Id, S1),

    % Mark MMU block remappable
    MMU_IN_ID = ["IN", "MMU0" | Id],
    MMU_OUT_ID = ["OUT", "MMU0" | Id],
    state_add(S1, node_block_meta(MMU_IN_ID, 21, MMU_OUT_ID), S2), % Make MMU configurable
    %pt_alloc(S2, Root, S3),
    state_add(S2, node_pt(MMU_IN_ID, Root, MMU_OUT_ID), S4),

    OUT_ID = ["OUT" | Id],
    state_add(S4, overlay(OUT_ID, DRAM_ID), S5),
    % Reserve memory for the process, the OUT/PROC0 node is the one where
    % initially the process (virtual) addresses are issued.
    Limit = 1099511627775, % (512 << 31) - 1
    state_add(S5, in_use(["OUT", "PROC0" | Id], [memory, [block{base:0, limit: Limit}]]), NewS).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%%% Mark ranges used and Query them 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Puts IC constraints on the variables
free_region(S, NodeId, _, Out) :-
   % Not a very smart allocator, finds the highest addr in use and append
   % Therefore can ignore Size
   findall(X, state_query(S, in_use(NodeId, X)), UsedBlockLi),
   block_address_gt([memory, block{limit: -1}], Out), % TODO: Works only for 1 Dim addr.
   (foreach(UsedBlock, UsedBlockLi), param(Out) do
       block_address_gt(UsedBlock, Out)
   ).

% Puts IC constraints on the variables
free_region(S, Name, Size) :-
    name{
        node_id: NodeId,
        address: Out
    } = Name,
    free_region(S, NodeId, Size, Out).

free_region(S, Region, Size) :-
    region_base_name(Region, Name),
    free_region(S, Name, Size).

% Resolves the variables
free_region_aligned(S, Region, Size) :-
    is_list(Size),
    region_base_name(Region, BaseName),
    free_region(S, BaseName, Size),
    name_aligned(BaseName, 21),
    term_variables(BaseName, BaseNameVars),
    labeling(BaseNameVars),
    region_size(Region, Size).

% Resolves the variables
free_accepted_region_aligned(S, Region, Size) :-
    is_list(Size),
    region_base_name(Region, BaseName),
    free_region(S, BaseName, Size),
    name_aligned(BaseName, 21),
    accept(BaseName),
    term_variables(BaseName, BaseNameVars),
    labeling(BaseNameVars),
    region_size(Region, Size).

%:- export free_region/1.
%free_region(Region) :-
%    region_size(Region, Size), % Determine size using the base/limit in the region.
%    free_region(Region, Size).


%% NodeId:: Addr, Size :: Addr, Out :: Addr
alloc_range(S, NodeId, Size, Out) :-
   free_region(S, NodeId, Size, Out),
   term_variables(Out, OutVars),
   labeling(OutVars).


% After finding a range with alloc range, you actually want to mark it used
% with this function.
mark_range_in_use(S, NodeId, [Kind, A], Size, NewS) :-
    Limit is A + Size,
    UsedBlock = block{
        base: A,
        limit: Limit
    },    
    state_add(S, in_use(NodeId, [Kind, UsedBlock]), NewS).

mark_range_in_use(S, Name, ISize, NewS) :-
    name{
        node_id: NodeId,
        address: Addr
    } = Name,
    mark_range_in_use(NodeId, Addr, ISize).

mark_range_in_use(S, Region, NewS) :-
    Region = region{ node_id: NodeId, blocks: Blocks },
    state_add(S, in_use(NodeId, Blocks), NewS).


mark_range_free(S, NodeId, Base, NewS) :-
    state_remove(S, in_use(NodeId, [memory, [block{base: Base}]]), NewS).

:-export test_alloc_range/0.
test_alloc_range :-
    Id = [],
    state_empty(S),
    % Test setup
    mark_range_in_use(S, Id, [memory, 0], 1000, S1),
    
    % First allocation
    Size = 1000,
    alloc_range(S1, Id, [memory, Size], Out),
    mark_range_in_use(S1, Id, Out, Size, S2),
    Out = [memory, 1001],

    % Second allocation
    Size2 = 5000,
    alloc_range(S2, Id, [memory, Size2], Out2),
    mark_range_in_use(S2, Id, Out2, Size2, _),
    Out2 = [memory, 2002].

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
common_free_buffer(Size, N1Region, N2Region, ResRegion, Route)  :-
    is_list(Size),
    
    N1Region = region{blocks: [memory, _]},
    N2Region = region{blocks: [memory, _]},
    ResRegion = region{blocks: [memory, block{base:Base, limit: Limit}]},

    % nail down the regions 
    free_region_aligned(N1Region, Size),
    free_region_aligned(N2Region, Size),
    free_accepted_region_aligned(ResRegion, Size),
    accept(ResRegion),

    route(N1Region, ResRegion, R1),
    route(N2Region, ResRegion, R2),

    union(R1,R2,Route).

% Find two regions N1Region and N2Region, that resolve to an existing result region.
:- export common_free_map/5.
common_free_map(Size, N1Region, N2Region, ResRegion,Route)  :-
    is_list(Size),
    
    N1Region = region{blocks: [memory, _]},
    N2Region = region{blocks: [memory, _]},
    ResRegion = region{blocks: [memory, block{base:Base, limit: Limit}]},

    % nail down the input regions first
    free_region_aligned(N1Region, Size),
    free_region_aligned(N2Region, Size),

    route(N1Region, ResRegion, R1),
    route(N2Region, ResRegion, R2),

    labeling([Base,Limit]), 
    union(R1,R2,Route).

% the function called from mem_serv
:- export alloc_common/4.
% Allocate a Buffer with Bits size, reachable from N1 and N2. Mark the resolved 
% region as in use.
alloc_common(Bits, N1Enum, N2Enum, DestEnum)  :-
    enum_node_id(N1Enum, N1Id),
    enum_node_id(N2Enum, N2Id),
    enum_node_id(DestEnum, DestId),
    R1 = region{node_id: N1Id, blocks: [memory, block{base:R1Addr}]},
    R2 = region{node_id: N2Id, blocks: [memory, block{base:R2Addr}]},
    Dest = region{node_id: DestId, blocks: [memory, block{base:DestAddr}]},
    Size is 2 ^ Bits - 1,
    common_free_buffer([memory, [Size]], R1, R2, Dest, _),
    mark_range_in_use(Dest),
    writeln([name(R1Addr, N1Enum),name(R2Addr, N2Enum),name(DestAddr, DestEnum)]).

% Like alloc_common/4, but tries to determine destination node automatically.
:- export alloc_common/3.
alloc_common(Bits, N1Enum, N2Enum)  :-
    DRAM = ["DRAM"],
    get_or_alloc_node_enum(DRAM, DramEnum),
    alloc_common(Bits, N1Enum, N2Enum, DramEnum).

% Find names in N1 and N2 that resolve to ResAddr, then mark those used.
:- export map_common/4.
map_common(Bits, ResRAddr, N1Enum, N2Enum)  :-
    enum_node_id(N1Enum, N1Id),
    enum_node_id(N2Enum, N2Id),
    R1 = region{node_id: N1Id, blocks: [memory, block{base:R1Addr}]},
    R2 = region{node_id: N2Id, blocks: [memory, block{base:R2Addr}]},
    Size is 2 ^ Bits - 1,
    ResR = region{blocks: [memory, block{base:ResRAddr}]},
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
        not(address_gte(SrcLimit, BlockLimit)),

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

route_step_new(S, SrcRegions, NextRegions, Route) :-
    (decode_step_regions(S, SrcRegions, NextRegions), Route=[]) ;
    decode_step_regions_conf(S, SrcRegions, NextRegions, Route).

route_new(S, SrcRegions, DstRegions, Route) :-
    SrcRegion = region{node_id: SrcNodeId, blocks: SrcBlocks},
    DstRegion = region{node_id: DstNodeId, blocks: DstBlocks},
    route_step_new(S, SrcRegions, NextRegions, R1),
    ( accept_regions(S, NextRegions) -> (
        DstRegions = NextRegions,
        Route = R1
    ) ; (
        route_new(S, NextRegions, DstRegions, R2),
        union(R1, R2, Route)
    )).

:- export test_route_new/0.
test_route_new :-
    Upper is 512 * 1024 * 1024,
    Limit2M is 2^21 - 1,
    S = [
        mapping(
            region{node_id: ["IN"], blocks: [memory, block{base:0, limit:Upper}]},
            name{node_id: ["MMU"], address: [memory, 0]}),
        block_meta(["MMU"], 21, ["RAM"]),
        accept(region{node_id: ["RAM"], blocks: [memory, block{base:0, limit: Upper}]})
        ],
    state_valid(S),

    route_new(S,
        [region{node_id:["IN"], blocks: [memory, block{base:0, limit: Limit2M}]}],
        OutRegions, Route),
    OutRegions = [region{node_id:["RAM"], blocks: [memory, block{base:0, limit: Limit2M}]}],
    Route = [block_conf(["MMU"], 0, 0)].



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
        region{node_id:["IN"], blocks:[memory,block{base:0, limit:1000}]},
        region{node_id:["OUT"], blocks:[memory,block{base: 0, limit: 1000}]},
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
        region{node_id:["IN"], blocks:[memory, block{base:0, limit:Limit}]},
        region{node_id:["OUT"], blocks:[memory, block{base: 0, limit: Limit}]},
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_test(Test) :-
    (
        call(Test),
        printf("Test %p succeeds!\n", Test)
    ) ; (
        printf("!!! Test %p failed !!!\n", Test)
    ).

:- export run_all_tests/0.
run_all_tests :-
    run_test(test_alignment),
    run_test(test_scan_points),
    run_test(test_max_not_translated_pt),
    run_test(test_translate),
    run_test(test_accept_name),
    run_test(test_accept_region),
    run_test(test_decode_step_name),
    run_test(test_decode_step_name2),
    run_test(test_decode_step_name3),
    run_test(test_decode_step_region1),
    run_test(test_resolve_name),
    run_test(test_resolve_name2),
    run_test(test_decode_step_region_conf_one),
    run_test(test_route_new),
    run_test(test_split_vpn),
    run_test(test_alloc_range),
    run_test(test_region_region_intersection).
