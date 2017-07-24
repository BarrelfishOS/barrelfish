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
:- export net/2.
:- export loadnet/1.
:- export resolve/2.
:- export toRegion/2.

:- lib(ic).

:- dynamic net/2.

%% Load a precompiled decoding net
loadnet(File) :- [File].

%% Address range in block
blockRange(block(Base,Limit),Range) :-
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
        foreach(map(Block,_,_),Maps),
        fromto([],Prev,Next,Blocks)
    do
        Next = [Block|Prev]
    ),
    blockListRanges(Blocks,Ranges).

mapsToName(map(SrcBlock,Dest,DestBase),Addr,Name) :-
    name(Dest,DestAddr) = Name,
    blockRange(SrcBlock,Range),
    Addr :: Range,
    block(SrcBase,_) = SrcBlock,
    DestAddr #= Addr - SrcBase + DestBase.

listMapsToName([M|Maps],Addr,Name) :-
    mapsToName(M,Addr,Name);
    listMapsToName(Maps,Addr,Name).    

translateMap(node(_,_,Translate),Addr,Name) :-
    listMapsToName(Translate,Addr,Name).

translate(Node,Addr,Name) :-
    translateMap(Node,Addr,Name).

accept(node(_,Accept,_),Addr) :-
    blockListRanges(Accept,Ranges),
    Addr :: Ranges.

acceptedName(name(NodeId,Addr)) :-
    net(NodeId,Node),
    accept(Node,Addr).

decodeStep(name(NodeId,Addr),Name) :-
    net(NodeId,Node),
    translate(Node,Addr,Name).

decodesTo(SrcName,DestName) :-
    decodeStep(SrcName,Name),
    (   DestName = Name
    ;   decodesTo(Name,DestName)
    ).

resolve(SrcName,DestName) :-
    (   DestName = SrcName
    ;   decodesTo(SrcName,DestName)
    ),
    acceptedName(DestName).

toRegion(Name,Region) :-
    name(Id,Addr) = Name,
    get_min(Addr,Min),get_max(Addr,Max),
    Size is Max - Min + 1,
    Region = (Id, Min, Size).
  