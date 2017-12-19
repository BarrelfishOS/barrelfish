%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(decoding_net).
:- export load_net/1.
:- export decodes_to/2.
:- export resolve/2.
:- export node/2.

:- dynamic node/2.
:- export enum_translate_range/3.
:- export name_to_enum/2.


:- export struct(node(id:node_id,spec:node_spec)).
:- export struct(node_id(name,namespace)).
:- export struct(node_spec(type,accept,translate)).
:- local struct(map(src_block,dest_node,dest_base,dest_props)).
:- local struct(block(base,limit,props)).

:- export struct(name(node_id:node_id,address)).
:- export struct(region(node_id:node_id,base,size)).
:- export struct(meta(node_id:node_id,key,value)).

%% Used to enumerate names on nodes, to use them in capabilities.
:- local struct(region_enum_block(node_id:node_id, in_base, in_limit, enum_base, enum_limit)).


:- lib(ic).

:- set_flag(syntax_option,based_bignums).
:- set_flag(syntax_option,iso_base_prefix).

:- dynamic meta/3.
:- dynamic region_enum_block/5.

%% Load a precompiled decoding net
load_net(File) :-
    ensure_loaded(File).

%% Convert from regions to names
to_name(Region,Name) :-
    region{
        node_id:Id,
        base:Base,
        size:Size
    } = Region,
    Addr #>= Base,
    Addr #< Base + Size,
    Name = name{
        node_id:Id,
        address:Addr
    }.

%% Convert from names to regions
to_region(Name,Region) :-
    name{
        node_id:Id,
        address:Addr
    } = Name,
    get_bounds(Addr,Min,Max),
    Size is Max - Min + 1,
    ( get_domain_size(Addr,Size) ->
            Region = region{
            node_id:Id,
            base:Min,
            size:Size
        }
    ;
        writeln(stderr,"Name conversion to region failed: Non continuous domain for address"),
        fail
    ).

%% Address range in block
block_range(Block,Range) :-
    block{
        base:Base,
        limit:Limit
    } = Block,
    Range = Base..Limit.

maps_to_name([Map|_],Addr,Name) :-
        map{
        src_block:SrcBlock,
        dest_node:Dest,
        dest_base:DestBase
    } = Map,
    name{
        node_id:Dest,
        address:DestAddr
    } = Name,
    block_range(SrcBlock,Range),
    Addr :: Range,
    block{base:SrcBase} = SrcBlock,
    DestAddr #= Addr - SrcBase + DestBase.

maps_to_name([_|Maps],Addr,Name) :-
    maps_to_name(Maps,Addr,Name).    

translate(NodeSpec,Addr,Name) :-
    node_spec{translate:Translate} = NodeSpec,
    maps_to_name(Translate,Addr,Name).

accept(NodeSpec,Addr) :-
    node_spec{accept:Accept} = NodeSpec,
    (
        foreach(Block,Accept),
        fromto([],Prev,Next,Ranges)
    do
        block_range(Block,Range),
        Next = [Range|Prev]
    ),
    Addr :: Ranges.

accepted_name(Name) :-
    name{
        node_id:NodeId,
        address:Addr
    } = Name,
    node{
        id:NodeId,
        spec:NodeSpec
    },
    accept(NodeSpec,Addr).

decode_step(SrcName,DestName) :-
    name{
        node_id:NodeId,
        address:Addr
    } = SrcName,
    node{
        id:NodeId,
        spec:NodeSpec
    },
    translate(NodeSpec,Addr,DestName).

% Reflexive, transitive closure of decode_step
decodes_to(SrcName,DestName) :-
    SrcName = DestName.

decodes_to(SrcName,DestName) :-
    decode_step(SrcName,Name),
    decodes_to(Name,DestName).

resolve(SrcName,DestName) :-
    name{} = SrcName,
    name{} = DestName,
    decodes_to(SrcName,DestName),
    accepted_name(DestName).

resolve(SrcRegion,DestRegion) :-
    to_name(SrcRegion,SrcName),
    to_name(DestRegion,DestName),
    resolve(SrcName,DestName),
    to_region(SrcName,SrcRegion),
    to_region(DestName,DestRegion).

ic_add_pred(A,B, R) :-
    R #= A + B.

enum_translate_range_block(NodeId, InBase, InLimit, EnumBase, EnumLimit) :-
    region_enum_block(NodeId, InBase, InLimit, EnumBase, EnumLimit) ;
    (
        findall((B,L), region_enum_block(_,_,_,B,L), LiPair),
        ( 
            foreach((_,InL), LiPair),
            foreach(InBound, LiBound)
        do
            InBound #= InL + 1
        ),
        maxlist([0|LiBound], MaxEnumBase),

        EnumBase = MaxEnumBase,
        EnumLimit #= EnumBase + InLimit - InBase,
        assert(region_enum_block(NodeId, InBase, InLimit, EnumBase, EnumLimit))
    ).


% enumerate all inputs of the nodeid. If one exists, return existing enumeration.
% Assumes that accept and translate blocks are non overlapping.
enum_translate_range(NodeId, Base, Limit) :-
        node{id:NodeId, spec: node_spec{translate:TS}},
        %% Extract blocks from translate set 
        (
            foreach(TSSpec,TS),
            foreach(RSSpec, RS)
        do
            TSSpec = map(block(Base, Limit, _), _, _, _),
            RSSpec = (Base, Limit)
        ),

        %% TODO extract blocks from accept and append to Blocks

        %% Pass blocks into enum_translate_range_block, return aggregate in 
        %% EnumBase and EnumLimit
        (
            foreach((Base, Limit),RS),
            param(NodeId),
            foreach(EBase, EBaseList),
            foreach(EMax, EMaxList)
        do
            enum_translate_range_block(NodeId, Base, Limit, EBase, ELimit),
            EMax #= ELimit
        ),
        maxlist(EMaxList, Limit),
        minlist(EBaseList, Base).

name_to_enum(Name, EnumInt) :-
    EBase #=< EnumInt,
    EnumInt #=< ELimit,
    region_enum_block(NodeId, Base, Limit, EBase, ELimit),
    Addr #= Base + EnumInt - EBase,
    Name = name{node_id:NodeId, address: Addr}.
