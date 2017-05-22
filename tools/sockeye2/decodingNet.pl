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

%% C constrains Addr to be in block
inBlock(block(Base,Limit),Addr,C) :-
    C = (Addr :: [Base..Limit]).

%% C constrains Addr to be in one of the blocks
inBlockList(Blocks,Addr,C) :-
    (
        foreach(block(Base,Limit),Blocks),
        fromto([],Prev,Next,Intervals)
    do
        Next = [Base..Limit|Prev]
    ),
    C = (Addr :: Intervals).
    
%% Extract blocks from map list
blocksInMapList(Maps,Blocks) :-
    (
        foreach(map(Block,_,_),Maps),
        fromto([],Prev,Next,Blocks)
    do
        Next = [Block|Prev]
    ).

mapsToName(map(SrcBlock,Dest,DestBase),Addr,Name) :-
    name(Dest,DestAddr) = Name,
    inBlock(SrcBlock,Addr,C),call(C),
    block(SrcBase,_) = SrcBlock,
    DestAddr #= Addr - SrcBase + DestBase.

listMapsToName([M|Maps],Addr,Name) :-
    mapsToName(M,Addr,Name);
    listMapsToName(Maps,Addr,Name).    

translateMap(node(_,Translate,_),Addr,Name) :-
    listMapsToName(Translate,Addr,Name).

translateOverlay(node(Accept,Translate,Overlay),Addr,Name) :-
    inBlockList(Accept,Addr,C1),call(neg C1),
    blocksInMapList(Translate,TBlocks),
    inBlockList(TBlocks,Addr,C2),call(neg C2),
    Name = name(Overlay,Addr).

translate(Node,Addr,Name) :-
    translateMap(Node,Addr,Name);
    translateOverlay(Node,Addr,Name).

accept(node(Accept,_,_),Addr) :-
    inBlockList(Accept,Addr,C),
    call(C).

acceptedName(Name) :-
    name(NodeId,Addr) = Name,
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
