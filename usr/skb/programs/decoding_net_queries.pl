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
:- export find_device_region/2.
:- export find_memory_region/2.
:- export find_shared_memory_region/3.

:- export discover_core/1.
:- export discover_device/1.

:- export kernel_page_table/4.
:- export driver_register_regions/3.
:- export driver_interrupt_enum/2.

:- use_module(decoding_net).


%%%%%%%%%%%%%
%% Queries %%
%%%%%%%%%%%%%
%% Address space queries
find_device_region(SrcRegion,DestRegion) :-
    region{node_id:DeviceId} = DestRegion,
    node{
        id:DeviceId,
        type:device
    },
    resolve(SrcRegion,DestRegion).

find_memory_region(SrcRegion,DestRegion) :-
    region{node_id:MemoryId} = DestRegion,
    node{
        id:MemoryId,
        type:memory
    },
    resolve(SrcRegion,DestRegion).

find_shared_memory_region(SrcRegion,DestRegion,SharedRegion) :-
    region{node_id:SharedId} = SharedRegion,
    node{
        id:SharedId,
        type:memory
    },
    resolve(SrcRegion,SharedRegion),
    resolve(DestRegion,SharedRegion).

%%%%%%%%%%%%%%%%%%%%%%%%%%
% Core/Device Management %
%%%%%%%%%%%%%%%%%%%%%%%%%%
discover_core(Core) :-
    node{id:Id,type:core},
    decodes_to(name{name:Core,namespace:[]},name{node_id:Id}).

discover_device(Device) :-
    node{name:Device,type:device}.

kernel_page_table(CoreID,MemoryIds,DeviceIds,PageTable) :-
    meta{
        node_id:Core,
        key:hw_id,
        value:CoreID
    },
    (
        param(Core),
        foreach(Id,MemoryIds),
        fromto([],Prev,Next,MemoryRecords)
    do
        (
            find_memory_region(region{node_id:Core,namespace:[],base:Base,size:Size},region{name:Id})
        ;
            node_id{name:CoreName} = Core,
            exitError("Can't find memory '%a' from Core '%a'",[Id,CoreName])
        ),
        Next = [pt_memory(Base,Size)|Prev]
    ),
    (
        param(Core),
        foreach(Id,DeviceIds),
        fromto([],Prev,Next,DeviceRecords)
    do
        (
            find_device_region(region{node_id:Core,namespace:[],base:Base,size:Size},region{name:Id})
        ;
            node_id{name:CoreName} = Core,
            exitError("Can't find device '%a' from Core '%a'",[Id,CoreName])
        ),
        Next = [pt_device(Base,Size)|Prev]
    ),
    length(MemoryRecords,NumMemory),
    length(DeviceRecords,NumDevices),
    PageTable = page_table(NumMemory,NumDevices,MemoryRecords,DeviceRecords).

driver_register_regions(CoreID,DeviceIds,Regions) :-
    meta{
        node_id:Core,
        key:hw_id,
        value:CoreID
    },
    (
        param(Core),
        foreach(Id,DeviceIds),
        fromto([],Prev,Next,Rs)
    do
        find_device_region(region{node_id:Core,namespace:[],base:Addr,size:Size},region{name:Id}),
        Next = [reg(Addr,Size)|Prev]
    ),
    reverse(Rs,Regions).

driver_interrupt_enum(DeviceIds, Interrupts) :-
    (
        foreach(Id,DeviceIds),
        fromto([],Prev,Next,Is)
    do
        NodeId = node_id{name:Id, namespace:['Int']},
        enum_translate_range(NodeId, Base, Limit),
        Next = [int(Base,Limit)|Prev]
    ),

    reverse(Is,Interrupts).
