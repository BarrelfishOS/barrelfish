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

findTargetRanges(NodeId) :-
    SrcName = name(NodeId,_),
    findall((SrcRange,DestRange),findRanges(SrcName,_,SrcRange,DestRange),List),
    (foreach((Src,Dest),List) do printSrcDestRanges(Src,Dest)).


findOriginRanges(NodeId) :-
    DestName = name(NodeId,_),
    findall((SrcRange,DestRange),findRanges(_,DestName,SrcRange,DestRange),List),
    (foreach((Src,Dest),List) do printSrcDestRanges(Src,Dest)).

findDeviceFrame(NodeId,DeviceId) :-
    SrcName = name(NodeId,_),
    DestName = name(DeviceId,_),
    findRanges(SrcName,DestName,SrcRange,DestRange),
    printSrcDestRanges(SrcRange,DestRange).

findInterruptLine(NodeId,DeviceId) :-
    SrcName = name(DeviceId,_),
    DestName = name(NodeId,_),
    findRanges(SrcName,DestName,SrcRange,DestRange),
    printSrcDestRanges(SrcRange,DestRange).

findSharedMemoryFrame(NodeId,DeviceId) :-
    NodeName = name(NodeId,_),
    DevName = name(DeviceId,_),
    findRanges(NodeName,_,NodeRange,SharedRange),
    findRanges(DevName,_,DeviceRange,SharedRange),
    printSharedRanges(NodeRange,SharedRange,DeviceRange).  