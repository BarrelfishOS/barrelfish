%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(decodingNetQueries).
:- export findTargetRegion/2.
:- export findOriginRegion/2.
:- export findDeviceRegion/3.
:- export findMemoryRegion/3.
:- export findSharedMemoryRegion/3.
:- export findDeviceId/3.
:- export findInterruptLine/3.

:- use_module(decodingNet).


%%%%%%%%%%%%%%%%%%%%%%%
%% Helper predicates %%
%%%%%%%%%%%%%%%%%%%%%%%
resolveToRegion(SrcName,DestName,SrcRegion,DestRegion) :-
    resolve(SrcName,DestName),
    toRegion(SrcName,SrcRegion),
    toRegion(DestName,DestRegion).


%%%%%%%%%%%%%
%% Queries %%
%%%%%%%%%%%%%
findTargetRegion(NodeId,Result) :-
    SrcName = name{nodeId:NodeId},
    resolveToRegion(SrcName,_,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).

findOriginRegion(NodeId,Result) :-
    DestName = name{nodeId:NodeId},
    resolveToRegion(_,DestName,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).

%% Address space queries
findDeviceRegion(NodeId,DeviceId,Result) :-
    SrcName = name{nodeId:NodeId},
    DestName = name{nodeId:DeviceId},
    node{
        id:DeviceId,
        type:device
    },
    resolveToRegion(SrcName,DestName,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).

findMemoryRegion(NodeId,MemoryId,Result) :-
    SrcName = name{nodeId:NodeId},
    DestName = name{nodeId:MemoryId},
    node{
        id:MemoryId,
        type:memory
    },
    resolveToRegion(SrcName,DestName,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).

findSharedMemoryRegion(NodeId,DeviceId,Result) :-
    NodeName = name{nodeId:NodeId},
    DeviceName = name{nodeId:DeviceId},
    SharedName = name{nodeId:SharedId},
    node{
        id:SharedId,
        type:memory
    },
    resolve(NodeName,SharedName),
    resolve(DeviceName,SharedName),
    toRegion(NodeName,NodeRegion),
    toRegion(SharedName,SharedRegion),
    toRegion(DeviceName,DeviceRegion),
    Result = (NodeRegion,DeviceRegion,SharedRegion).

findDeviceId(NodeId,Addr,Result) :-
    SrcName = name{
        nodeId:NodeId,
        address:Addr
    },
    DestName = name{nodeId:DeviceId},
    resolve(SrcName,DestName),
    Result = DeviceId.

%% Interrupt queries
findInterruptLine(NodeId,DeviceId,Result) :-
    SrcName = name{nodeId:DeviceId},
    DestName = name{nodeId:NodeId},
    resolveToRegion(SrcName,DestName,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).