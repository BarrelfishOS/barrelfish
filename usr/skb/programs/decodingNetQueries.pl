%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(decodingNet).

%% Printing
printRange((SrcId,SrcMin,SrcMax)) :-
    printf("%a [0x%16R..0x%16R]",
        [ SrcId,SrcMin,SrcMax ]
    ).

printSrcDestRanges(SrcRange,DestRange) :-
    printRange(SrcRange),
    write(" -> "),
    printRange(DestRange),
    writeln("").

printSharedRanges(Range1,SharedRange,Range2) :-
    printRange(Range1),
    write(" -> "),
    printRange(SharedRange),
    write(" <- "),
    printRange(Range2),
    writeln("").

%% Helper predicates
resolveToRange(SrcName,DestName,SrcRange,DestRange) :-
    resolve(SrcName,DestName),
    toRange(SrcName,SrcRange),
    toRange(DestName,DestRange).

all(Pred) :- findall(_,Pred,_).

%% Queries
findTargetRange(NodeId) :-
    SrcName = name(NodeId,_),
    resolveToRange(SrcName,_,SrcRange,DestRange),
    printSrcDestRanges(SrcRange,DestRange).

findOriginRange(NodeId) :-
    DestName = name(NodeId,_),
    resolveToRange(_,DestName,SrcRange,DestRange),
    printSrcDestRanges(SrcRange,DestRange).

findDeviceFrame(NodeId,DeviceId) :-
    SrcName = name(NodeId,_),
    DestName = name(DeviceId,_),
    net(DeviceId,node(device,_,_,_)),
    resolveToRange(SrcName,DestName,SrcRange,DestRange),
    printSrcDestRanges(SrcRange,DestRange).

findInterruptLine(NodeId,DeviceId) :-
    SrcName = name(DeviceId,_),
    DestName = name(NodeId,_),
    resolveToRange(SrcName,DestName,SrcRange,DestRange),
    printSrcDestRanges(SrcRange,DestRange).

findSharedMemoryFrame(NodeId,DeviceId) :-
    NodeName = name(NodeId,_),
    DeviceName = name(DeviceId,_),
    SharedName = name(SharedId,_),
    net(SharedId,node(memory,_,_,_)),
    resolve(NodeName,SharedName),
    resolve(DeviceName,SharedName),
    toRange(NodeName,NodeRange),
    toRange(SharedName,SharedRange),
    toRange(DeviceName,DeviceRange),
    printSharedRanges(NodeRange,SharedRange,DeviceRange).

findDeviceId(NodeId,Addr) :-
    SrcName = name(NodeId,Addr),
    resolve(SrcName,name(DeviceId,_)),
    writeln(DeviceId).