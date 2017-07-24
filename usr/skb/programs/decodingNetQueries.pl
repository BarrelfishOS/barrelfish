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
    SrcName = name(NodeId,_),
    resolveToRegion(SrcName,_,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).

findOriginRegion(NodeId,Result) :-
    DestName = name(NodeId,_),
    resolveToRegion(_,DestName,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).

%% Address space queries
findDeviceRegion(NodeId,DeviceId,Result) :-
    SrcName = name(NodeId,_),
    DestName = name(DeviceId,_),
    net(DeviceId,node(device,_,_)),
    resolveToRegion(SrcName,DestName,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).

findMemoryRegion(NodeId,MemoryId,Result) :-
    SrcName = name(NodeId,_),
    DestName = name(MemoryId,_),
    net(MemoryId,node(memory,_,_)),
    resolveToRegion(SrcName,DestName,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).

findSharedMemoryRegion(NodeId,DeviceId,Result) :-
    NodeName = name(NodeId,_),
    DeviceName = name(DeviceId,_),
    SharedName = name(SharedId,_),
    net(SharedId,node(memory,_,_)),
    resolve(NodeName,SharedName),
    resolve(DeviceName,SharedName),
    toRegion(NodeName,NodeRegion),
    toRegion(SharedName,SharedRegion),
    toRegion(DeviceName,DeviceRegion),
    Result = (NodeRegion,DeviceRegion,SharedRegion).

findDeviceId(NodeId,Addr,Result) :-
    SrcName = name(NodeId,Addr),
    resolve(SrcName,name(DeviceId,_)),
    Result = DeviceId.

%% Interrupt queries
findInterruptLine(NodeId,DeviceId,Result) :-
    SrcName = name(DeviceId,_),
    DestName = name(NodeId,_),
    resolveToRegion(SrcName,DestName,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).