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
:- use_module(decodingNetQueries).
:- local loadnet/1.

%%%%%%%%%%%%%%
%% Printing %%
%%%%%%%%%%%%%%
printNodeid(NodeId) :-
    nodeId{
        name:Name,
        namespace:Namespace
    } = NodeId,
    reverse([Name|Namespace], IdList),
    join_string(IdList,".",String),
    write(String).

printRegion(Region) :-
    region{
        nodeId:Id,
        base:Addr,
        size:Size
    } = Region,
    End is Addr + Size - 1,
    printNodeid(Id),
    printf(" [0x%16R..0x%16R]",
        [ Addr,End ]
    ).

printSrcDestRegions((SrcRegion,DestRegion)) :-
    printRegion(SrcRegion),
    write(" -> "),
    printRegion(DestRegion),
    writeln("").

printSharedRegions((Region1,Region2,SharedRegion)) :-
    printRegion(Region1),
    write(" -> "),
    printRegion(SharedRegion),
    write(" <- "),
    printRegion(Region2),
    writeln("").


%%%%%%%%%%%%%%%%%%%%%%%
%% Helper predicates %%
%%%%%%%%%%%%%%%%%%%%%%%
loadnet(Name) :- 
    concat_string(["sockeyefacts/",Name],File),
    decodingNet:loadnet(File).

all(Pred) :- findall(_,Pred,_).


%%%%%%%%%%%%%
%% Queries %%
%%%%%%%%%%%%%
findTargetRegion(NodeId) :-
    findTargetRegion(NodeId,Result),
    printSrcDestRegions(Result).

findOriginRegion(NodeId) :-
    findOriginRegion(NodeId,Result),
    printSrcDestRegions(Result).

findDeviceRegion(NodeId,DeviceId) :-
    findDeviceRegion(NodeId,DeviceId,Result),
    printSrcDestRegions(Result).

findMemoryRegion(NodeId,MemoryId) :-
    findMemoryRegion(NodeId,MemoryId,Result),
    printSrcDestRegions(Result).

findSharedMemoryRegion(NodeId,DeviceId) :-
    findSharedMemoryRegion(NodeId,DeviceId,Result),
    printSharedRegions(Result).

findDeviceId(NodeId,Addr) :-
    findDeviceId(NodeId,Addr,Result),
    writeln(Result).

findInterruptLine(NodeId,DeviceId) :-
    findInterruptLine(NodeId,DeviceId,Result),
    printSrcDestRegions(Result).