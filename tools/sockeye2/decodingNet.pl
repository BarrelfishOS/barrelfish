%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% :- module(decodingNet,[resolve/2]).

:- lib(ic).

blockContains(block(Base, Limit),Addr) :-
    %% between(Base,Limit,Addr).
    Addr :: Base .. Limit.

blockListContains([B|Blocks],Addr) :-
    blockContains(B,Addr);
    blockListContains(Blocks,Addr).

mapsToName(map(SrcBlock,Dest,DestBase),Addr,Name) :-
    name(Dest,DestAddr) = Name,
    blockContains(SrcBlock,Addr),
    block(SrcBase,_) = SrcBlock,
    DestAddr #= Addr - SrcBase + DestBase.

listMapsToName([M|Maps],Addr,Name) :-
    mapsToName(M,Addr,Name);
    listMapsToName(Maps,Addr,Name).    

accept(node(Accept,_,_),Addr) :-
    blockListContains(Accept,Addr).

translateMap(node(_,Translate,_),Addr,Name) :-
    listMapsToName(Translate,Addr,Name).

translateOverlay(Node,Addr,Name) :-
    not(accept(Node,Addr)),
    not(translateMap(Node,Addr,Name)),
    node(_,_,Overlay) = Node,
    Name = name(Overlay,Addr).

translate(Node,Addr,Name) :-
    translateMap(Node,Addr,Name);
    translateOverlay(Node,Addr,Name).

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
