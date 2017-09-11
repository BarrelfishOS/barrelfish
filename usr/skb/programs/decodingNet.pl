%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(decodingNet).
:- export loadnet/1.
:- export resolve/2.
:- export toRegion/2.
:- export node/2.

:- dynamic node/2.

:- export struct(node(id:nodeId,spec:nodeSpec)).
:- export struct(nodeId(name,namespace)).
:- export struct(nodeSpec(type,accept,translate)).
:- local struct(map(srcBlock,destNode,destBase)).
:- local struct(block(base,limit)).

:- export struct(name(nodeId:nodeId,address)).
:- export struct(region(nodeId:nodeId,base,size)).

:- lib(ic).

:- set_flag(syntax_option,based_bignums).
:- set_flag(syntax_option,iso_base_prefix).

%% Load a precompiled decoding net
loadnet(File) :- [File].

%% Address range in block
blockRange(Block,Range) :-
    block{
        base:Base,
        limit:Limit
    } = Block,
    Range = Base..Limit.

%% Address ranges in block list
blockListRanges(Blocks,Ranges) :-
    (
        foreach(Block,Blocks),
        fromto([],Prev,Next,Ranges)
    do
        blockRange(Block,Range),
        Next = [Range|Prev]
    ).
    
%% Extract block ranges from map list
mapListRanges(Maps,Ranges) :-
    (
        foreach(map{srcBlock:Block},Maps),
        fromto([],Prev,Next,Blocks)
    do
        Next = [Block|Prev]
    ),
    blockListRanges(Blocks,Ranges).

mapsToName(Map,Addr,Name) :-
    map{
        srcBlock:SrcBlock,
        destNode:Dest,
        destBase:DestBase
    } = Map,
    name{
        nodeId:Dest,
        address:DestAddr
    } = Name,
    blockRange(SrcBlock,Range),
    Addr :: Range,
    block{base:SrcBase} = SrcBlock,
    DestAddr #= Addr - SrcBase + DestBase.

listMapsToName([M|Maps],Addr,Name) :-
    mapsToName(M,Addr,Name);
    listMapsToName(Maps,Addr,Name).    

translateMap(NodeSpec,Addr,Name) :-
    nodeSpec{translate:Translate} = NodeSpec,
    listMapsToName(Translate,Addr,Name).

translate(Node,Addr,Name) :-
    translateMap(Node,Addr,Name).

accept(NodeSpec,Addr) :-
    nodeSpec{accept:Accept} = NodeSpec,
    blockListRanges(Accept,Ranges),
    Addr :: Ranges.

acceptedName(Name) :-
    name{
        nodeId:NodeId,
        address:Addr
    } = Name,
    node{
        id:NodeId,
        spec:NodeSpec
    },
    accept(NodeSpec,Addr).

decodeStep(SrcName,DestName) :-
    name{
        nodeId:NodeId,
        address:Addr
    } = SrcName,
    node{
        id:NodeId,
        spec:NodeSpec
    },
    translate(NodeSpec,Addr,DestName).

decodesTo(SrcName,DestName) :-
    SrcName = DestName.

decodesTo(SrcName,DestName) :-
    decodeStep(SrcName,Name),
    decodesTo(Name,DestName).

resolve(SrcName,DestName) :-
    decodesTo(SrcName,DestName),
    acceptedName(DestName).

toRegion(Name,Region) :-
    name{
        nodeId:Id,
        address:Addr
    } = Name,
    get_min(Addr,Min),get_max(Addr,Max),
    Size is Max - Min + 1,
    Region = region{
        nodeId:Id,
        base:Min,
        size:Size
    }.
  