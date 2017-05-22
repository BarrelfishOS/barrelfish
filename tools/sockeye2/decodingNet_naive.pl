blockContains(block(Base, Limit),Addr) :-
    between(Base,Limit,Addr).

blockListContains([B|Blocks],Addr) :-
    blockContains(B,Addr),!;
    blockListContains(Blocks,Addr).

mapsToName(map(SrcBlock,Dest,DestBase),Addr,Name) :-
    name(Dest,DestAddr) = Name,
    blockContains(SrcBlock,Addr),
    block(SrcBase,_) = SrcBlock,
    DestAddr is Addr - SrcBase + DestBase.

listMapsToName([M|Maps],Addr,Name) :-
    mapsToName(M,Addr,Name);
    listMapsToName(Maps,Addr,Name).    

accept(node(Accept,_,_),Addr) :-
    blockListContains(Accept,Addr).

translateMap(node(_,Translate,_),Addr,Name) :-
    listMapsToName(Translate,Addr,Name).

translateOverlay(node(_,_,Overlay),Addr,Name) :-
    Name = name(Overlay,Addr).

translate(Node,Addr,Name) :-
    translateMap(Node,Addr,Name),!;
    (   not(accept(Node,Addr))
    ,   translateOverlay(Node,Addr,Name)
    ).

acceptedName(Name) :-
    name(NodeId,Addr) = Name,
    net(NodeId,Node),
    accept(Node,Addr).

decodeStep(name(NodeId,Addr),Name) :-
    net(NodeId,Node),
    translate(Node,Addr,Name).

decodesTo(SrcName,DestName) :-
    decodeStep(SrcName,Name),
    (   DestName = Name,!
    ;   decodesTo(Name,DestName)
    ).

resolve(SrcName,DestName) :-
    (   DestName = SrcName
    ;   decodesTo(SrcName,DestName)
    ),
    acceptedName(DestName).
