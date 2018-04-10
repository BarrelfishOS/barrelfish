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

:- module(decoding_net4).
:- use_module(allocator3).
:- use_module(decoding_net3_state).


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
state_valid([in_use(_,_) | As]) :- state_valid(As).

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

% In case of existing mapping
translate_region(S, SrcRegion, DstRegion) :-
    region_mapping(S, SrcRegion, mapping(InCandidate, OutCandidate)),
    region_base_name(SrcRegion, name{address:SrcAddr}),
    InCandidate = region{block:InBlock},
    OutCandidate = name{node_id: OutNodeId, address: DstBaseAddr},
    block_translate(SrcAddr, InBlock, DstAddr, DstBaseAddr),
    region_base_name(DstRegion, name{node_id: OutNodeId, address: DstAddr}),
    region_size(SrcRegion, Size),
    region_size(DstRegion, Size).

% In case of overlay
translate_region(S, SrcRegion, DstRegion) :-
    not(region_mapping(S, SrcRegion, _)),
    SrcRegion = region{node_id: SrcId, block:B},
    state_query(S, overlay(SrcId, OutId)),
    DstRegion = region{node_id: OutId, block:B}.

translate_name(S, SrcName, name{node_id: DstId, address: DstAddr}) :-
    name_mapping(S, SrcName, mapping(InCandidate, OutCandidate)),
    SrcName = name{address:SrcAddr},
    InCandidate = region{block:InBlock},
    OutCandidate = name{node_id: DstId, address: DstBaseAddr},
    block_translate(SrcAddr, InBlock, DstAddr, DstBaseAddr).

translate_name(S, SrcName, name{node_id: DstId, address: DstAddr}) :-
    not(name_mapping(S, SrcName, _)),
    SrcName = name{node_id: SrcId, address:DstAddr},
    state_query(S, overlay(SrcId, DstId)).

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
    name{} = SrcName,
    name{} = DstName,
    decodes_name(S, SrcName,DstName),
    accept_name(S, DstName).

resolve_region(S, SrcRegion, DstRegion) :-
    decodes_region(S, SrcRegion, DstRegion),
    accept_region(S, DstRegion).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Utilities 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% block_translate(A,BaseA,B,BaseB) ==> A-BaseA = B-BaseB
block_translate(SrcAddr, SrcBlock, DstAddr, DstBase) :-
    SrcBlock = block{base:SrcBase},
    DstAddr #= SrcAddr - SrcBase + DstBase.

region_base_name(Region, Name) :-
    Region = region{node_id: NodeId, block: block{base:Base}},
    Name = name{node_id:NodeId, address: Base}.

region_size(Region, Size) :-
    region{ block: block{base:Base, limit:Limit} } = Region,
    (
        (var(Limit), Limit is Base + Size - 1) ;
        (Size is Limit - Base + 1)
    ).

block_size(block{base:B, limit: L}, Size) :-
    Size is L - B + 1.

address_block_match(A, block{base: B, limit: L}) :-
    B #=< A,
    A #=< L.

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
    ABase =< BLimit,
    ALimit >= BBase,
    ALimit =< BLimit.

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
    %Setup
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
    run_test(test_translate_name),
    run_test(test_accept_name),
    run_test(test_resolve_name1),
    run_test(test_resolve_name2),
    run_test(test_accept_region).
