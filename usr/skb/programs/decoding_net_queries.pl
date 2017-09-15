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
