%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- lib(ic).

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

translateMap(node(_,Translate,_),Addr,Name) :-
    listMapsToName(Translate,Addr,Name).

translateOverlay(node(Accept,Translate,Overlay),Addr,Name) :-
    blockListRanges(Accept,ARanges),
    neg(Addr :: ARanges),
    mapListRanges(Translate,TRanges),
    neg(Addr :: TRanges),
    Name = name(Overlay,Addr).

translate(Node,Addr,Name) :-
    translateMap(Node,Addr,Name);
    translateOverlay(Node,Addr,Name).

accept(node(Accept,_,_),Addr) :-
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

findTargets(SrcName,DestName) :-
    resolve(SrcName,DestName),
    name(_,Addr) = DestName,
    labeling([Addr]).
    

findOrigins(SrcName,DestName) :-
    resolve(SrcName,DestName),
    name(_,Addr) = SrcName,
    labeling([Addr]).

findTargetRange(SrcName,SrcRange,DestRange) :-
    resolve(SrcName,DestName),
    name(SrcId,SrcAddr) = SrcName,
    name(DestId,DestAddr) = DestName,
    get_min(SrcAddr,SrcMin),get_max(SrcAddr,SrcMax),
    get_min(DestAddr,DestMin),get_max(DestAddr,DestMax),
    SrcRange = (SrcId,SrcMin,SrcMax),
    DestRange = (DestId,DestMin,DestMax),
    labeling([DestMin]).