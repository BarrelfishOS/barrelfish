%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(decoding_net).
:- use_module(decoding_net_queries).
:- local load_net/1.

%%%%%%%%%%%%%%
%% Printing %%
%%%%%%%%%%%%%%
print_node_id(NodeId) :-
    node_id{
        name:Name,
        namespace:Namespace
    } = NodeId,
    reverse([Name|Namespace], IdList),
    join_string(IdList,".",String),
    write(String).

print_region(Region) :-
    region{
        node_id:Id,
        base:Addr,
        size:Size
    } = Region,
    End is Addr + Size - 1,
    print_node_id(Id),
    printf(" [0x%16R..0x%16R]",
        [ Addr,End ]
    ).

print_src_dest_regions((SrcRegion,DestRegion)) :-
    print_region(SrcRegion),
    write(" -> "),
    print_region(DestRegion),
    writeln("").

print_shared_regions((Region1,Region2,SharedRegion)) :-
    print_region(Region1),
    write(" -> "),
    print_region(SharedRegion),
    write(" <- "),
    print_region(Region2),
    writeln("").


%%%%%%%%%%%%%%%%%%%%%%%
%% Helper predicates %%
%%%%%%%%%%%%%%%%%%%%%%%
load_net(Name) :- 
    concat_string(["sockeyefacts/",Name],File),
    decoding_net:load_net(File).

all(Pred) :- findall(_,Pred,_).


%%%%%%%%%%%%%
%% Queries %%
%%%%%%%%%%%%%
find_target_region(NodeId) :-
    find_target_region(NodeId,Result),
    print_src_dest_regions(Result).

find_origin_region(NodeId) :-
    find_origin_region(NodeId,Result),
    print_src_dest_regions(Result).

find_device_region(NodeId,DeviceId) :-
    find_device_region(NodeId,DeviceId,Result),
    print_src_dest_regions(Result).

find_memory_region(NodeId,MemoryId) :-
    find_memory_region(NodeId,MemoryId,Result),
    print_src_dest_regions(Result).

find_shared_memory_region(NodeId,DeviceId) :-
    find_shared_memory_region(NodeId,DeviceId,Result),
    print_shared_regions(Result).

find_device_id(NodeId,Addr) :-
    find_device_id(NodeId,Addr,Result),
    writeln(Result).

find_interrupt_line(NodeId,DeviceId) :-
    find_interrupt_line(NodeId,DeviceId,Result),
    print_src_dest_regions(Result).