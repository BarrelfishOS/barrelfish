%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2018, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This module wraps the state modifying functions of the decoding net.
% It is written in such a way, that it can be reimplemented with assert/retract
% state modifications.


:- module(decoding_net3_state).

:- export state_empty/1.
:- export state_add/3.
:- export state_remove/3.
:- export state_union/3.
:- export state_query/2.

state_empty([]).

state_add(O, Fact, N) :-
    append(O, [Fact], N).  

state_remove(O, Fact, N) :-
    subtract(O, [Fact], N).

state_union(S1, S2, N) :-
    union(S1,S2,N).

state_query(O, Fact) :-
    member(Fact, O).
