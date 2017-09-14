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
:- export resolve/2.
:- export to_region/2.
:- export node/2.

:- dynamic node/2.

:- export struct(node(id:node_id,spec:node_spec)).
:- export struct(node_id(name,namespace)).
:- export struct(node_spec(type,accept,translate)).
:- local struct(map(src_block,dest_node,dest_base)).
:- local struct(block(base,limit)).

:- export struct(name(node_id:node_id,address)).
:- export struct(region(node_id:node_id,base,size)).

:- lib(ic).

:- set_flag(syntax_option,based_bignums).
:- set_flag(syntax_option,iso_base_prefix).

%% Load a precompiled decoding net
load_net(File) :- [File].

%% Address range in block
block_range(Block,Range) :-
    block{
        base:Base,
        limit:Limit
    } = Block,
    Range = Base..Limit.

%% Address ranges in block list
block_list_ranges(Blocks,Ranges) :-
    (
        foreach(Block,Blocks),
        fromto([],Prev,Next,Ranges)
    do
        block_range(Block,Range),
        Next = [Range|Prev]
    ).
    
%% Extract block ranges from map list
map_list_ranges(Maps,Ranges) :-
    (
        foreach(map{src_block:Block},Maps),
        fromto([],Prev,Next,Blocks)
    do
        Next = [Block|Prev]
    ),
    block_list_ranges(Blocks,Ranges).

maps_to_name(Map,Addr,Name) :-
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

list_maps_to_name([M|Maps],Addr,Name) :-
    maps_to_name(M,Addr,Name);
    list_maps_to_name(Maps,Addr,Name).    

translate_map(NodeSpec,Addr,Name) :-
    node_spec{translate:Translate} = NodeSpec,
    list_maps_to_name(Translate,Addr,Name).

translate(Node,Addr,Name) :-
    translate_map(Node,Addr,Name).

accept(NodeSpec,Addr) :-
    node_spec{accept:Accept} = NodeSpec,
    block_list_ranges(Accept,Ranges),
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

decodes_to(SrcName,DestName) :-
    SrcName = DestName.

decodes_to(SrcName,DestName) :-
    decode_step(SrcName,Name),
    decodes_to(Name,DestName).

resolve(SrcName,DestName) :-
    decodes_to(SrcName,DestName),
    accepted_name(DestName).

to_region(Name,Region) :-
    name{
        node_id:Id,
        address:Addr
    } = Name,
    get_min(Addr,Min),get_max(Addr,Max),
    Size is Max - Min + 1,
    Region = region{
        node_id:Id,
        base:Min,
        size:Size
    }.
  