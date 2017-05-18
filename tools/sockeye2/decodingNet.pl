%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(decodingNet,[resolve/2]).

blockContains(block(Base, Limit),Addr) :-
    between(Base,Limit,Addr).

blockListContains([B|Blocks],Addr) :-
    blockContains(B,Addr);
    blockListContains(Blocks,Addr).

mapsToName(map(Src,Dest,DestBase),Addr,Name) :-
    blockContains(Src,Addr),
    block(SrcBase,_) = Src,
    DestAddr is Addr - SrcBase + DestBase,
    Name = name(Dest,DestAddr).

listMapsToName([M|Maps],Addr,Name) :-
    mapsToName(M,Addr,Name);
    listMapsToName(Maps,Addr,Name).    

accept(node(Accept,_,_),Addr) :-
    blockListContains(Accept,Addr).

translate(node(_,Translate,_),Addr,Name) :-
    listMapsToName(Translate,Addr,Name),!.

translate(Node,Addr,Name) :-
    not(accept(Node,Addr)),
    node(_,_,Overlay) = Node,
    Name = name(Overlay,Addr).

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
