%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2018, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(decoding_net2).
:- export node_accept/2.
:- export node_translate/4.


%% node_accept(InNodeId, InAddr :: block).
:- dynamic node_accept/2.

%% node_translate(InNodeId, InAddr :: block, OutNodeId, OutAddr :: block).
:- dynamic node_translate/4.

:- export struct(block(base,limit,props)).

:- lib(ic).
