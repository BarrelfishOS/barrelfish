%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(decoding_net_queries).
:- export find_target_region/2.
:- export find_origin_region/2.
:- export find_device_region/3.
:- export find_memory_region/3.
:- export find_shared_memory_region/3.
:- export find_device_id/3.
:- export find_interrupt_line/3.

:- use_module(decoding_net).


%%%%%%%%%%%%%%%%%%%%%%%
%% Helper predicates %%
%%%%%%%%%%%%%%%%%%%%%%%
resolve_to_region(SrcName,DestName,SrcRegion,DestRegion) :-
    resolve(SrcName,DestName),
    to_region(SrcName,SrcRegion),
    to_region(DestName,DestRegion).


%%%%%%%%%%%%%
%% Queries %%
%%%%%%%%%%%%%
find_target_region(NodeId,Result) :-
    SrcName = name{node_id:NodeId},
    resolve_to_region(SrcName,_,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).

find_origin_region(NodeId,Result) :-
    DestName = name{node_id:NodeId},
    resolve_to_region(_,DestName,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).

%% Address space queries
find_device_region(NodeId,DeviceId,Result) :-
    SrcName = name{node_id:NodeId},
    DestName = name{node_id:DeviceId},
    node{
        id:DeviceId,
        type:device
    },
    resolve_to_region(SrcName,DestName,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).

find_memory_region(NodeId,MemoryId,Result) :-
    SrcName = name{node_id:NodeId},
    DestName = name{node_id:MemoryId},
    node{
        id:MemoryId,
        type:memory
    },
    resolve_to_region(SrcName,DestName,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).

find_shared_memory_region(NodeId,DeviceId,Result) :-
    NodeName = name{node_id:NodeId},
    DeviceName = name{node_id:DeviceId},
    SharedName = name{node_id:SharedId},
    node{
        id:SharedId,
        type:memory
    },
    resolve(NodeName,SharedName),
    resolve(DeviceName,SharedName),
    to_region(NodeName,NodeRegion),
    to_region(SharedName,SharedRegion),
    to_region(DeviceName,DeviceRegion),
    Result = (NodeRegion,DeviceRegion,SharedRegion).

find_device_id(NodeId,Addr,Result) :-
    SrcName = name{
        node_id:NodeId,
        address:Addr
    },
    DestName = name{node_id:DeviceId},
    resolve(SrcName,DestName),
    Result = DeviceId.

%% Interrupt queries
find_interrupt_line(NodeId,DeviceId,Result) :-
    SrcName = name{node_id:DeviceId},
    DestName = name{node_id:NodeId},
    resolve_to_region(SrcName,DestName,SrcRegion,DestRegion),
    Result = (SrcRegion,DestRegion).