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


%% node_accept(InNodeId, InAddr :: block).
:- dynamic node_accept/2.

%% node_translate(InNodeId, InAddr :: block, OutNodeId, OutAddr :: block).
:- dynamic node_translate/4.

%% node_overlay(InNodeId, OutNodeId).
:- dynamic node_overlay/2.

:- export node_accept/2.
:- export node_translate/4.
:- export struct(block(base,limit,props)).

:- lib(ic).

block_match(A, block{base: B, limit: L}) :-
    B #=< A,
    A #=< L.

blocks_match_any(A, [B | Bs]) :-
    block_match(A, B) ; blocks_match_any(A, Bs).

blocks_match_any_ic(A, B) :-
    blocks_match_any(A,B),
    labeling([A]).

:- export block_values/2.
% Union of blocks. [block{base:0,limit:5},block{base:33,limit:35}] -> 0,1,..,5,33,..,35
block_values(Blocks, Values) :-
    findall(X, blocks_match_any_ic(X, Blocks), Values).
    

blocks_match([], []).
blocks_match([A|As], [B|Bs]) :-
    block_match(A,B),
    blocks_match(As, Bs).


% For a ic constrained variable
blocks_match_ic(X,Bs) :-
    length(Bs,LiLe),
    length(X,LiLe),
    blocks_match(X, Bs),
    labeling(X).

:- export block_crossp/2.
% Cross product of blocks
block_crossp(Blocks, Values) :-
    findall(X, blocks_match_ic(X, Blocks), Values).

%test :-
%    addr_match( (0,0), (block{base:0, limit:0})).


% Ideas for the address thingy:
% findall(X, addr_match(X, Constraints), XLi),
% (forall(X,XLi) do .... )
%
