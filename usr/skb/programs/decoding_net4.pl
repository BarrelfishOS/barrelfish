%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2018, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
% Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Some Conventions:
% NodeId = identifier. list of strings
% Addr : Int
% Name: name(NodeId, Addr)
% Region: region(NodeId, Block)
% Block: block(Base : Addr, Limit : Addr)

:- lib(ic).

%%% We make use of some static state by querying the following predicates, see
%%% decoding_net4_support.
% accept(Region)
% translate(SrcRegion, DstName)
% overlay(SrcNodeId, OutNodeId)
% configurable(SrcNodeId, Bits, OutNodeId)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Persisted state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
 * ===========================================================================
 * Dynamic State
 * ===========================================================================
 */


:- export state_empty/1.
state_empty(state([],[],[])).


/*
 * ---------------------------------------------------------------------------
 * Add to state
 * ---------------------------------------------------------------------------
 */


:- export state_add_mapping/4.
state_add_mapping(state(M, F, A), SrcReg, DstName,
                  state([mapping(SrcReg, DstName) | M], F, A)).

:- export state_add_free/4.
% TODO: This should probably add to the list?
state_add_free(state(M, F, A), NodeId, Blks,
               state(M, [free(NodeId, Blks) | F], A)).

:- export state_add_avail/4.
state_add_avail(state(M, F, A), NodeId, C,
                state(M, F, [avail(NodeId, C) | A])).


/*
 * ---------------------------------------------------------------------------
 * Remove from State
 * ---------------------------------------------------------------------------
 */


state_remove([], _, []).
state_remove([Fact|Tail], Fact, Out) :-
    state_remove(Tail, Fact, Out).
state_remove([Head|Tail], Fact, [Head | SubOut]) :-
    state_remove(Tail, Fact, SubOut).

:- export state_remove_mapping/4.
state_remove_mapping(state(M, F, A), SrcReg, DstName, state(M1, F, A)) :-
    state_remove(M, mapping(SrcReg, DstName), M1).

:- export state_remove_free/4.
state_remove_free(state(M, F, A), NodeId, Blks, state(M, F1, A)) :-
    state_remove(F, free(NodeId, Blks), F1).

:- export state_remove_avail/4.
state_remove_avail(state(M, F, A), NodeId, C, state(M, F, A1)) :-
    state_remove(A, avail(NodeId, C), A1).


/*
 * ---------------------------------------------------------------------------
 * State queries
 * ---------------------------------------------------------------------------
 */


state_has_fact([Fact|_], Fact).
state_has_fact([_|Tail], Fact) :-
    state_has_fact(Tail, Fact).

:- export state_has_mapping/3.
state_has_mapping(state(M, _, _), SrcReg, DstName) :-
    state_has_fact(M, mapping(SrcReg, DstName)).

:- export state_has_free/3.
state_has_free(state(_, F, _), NodeId, Blks) :-
    state_has_fact(F,free(NodeId, Blks)).

:- export state_has_avail/3.
state_has_avail(state(_, _, A), NodeId, C) :-
    state_has_fact(A,avail(NodeId, C)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Model layer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% region_region_contains(A, B).. A \in B but not B \in A
region_region_contains(region(N, block(ABase, ALimit)),
                       region(N, block(BBase, BLimit))) :-
    ABase #>= BBase,  BLimit #>= ABase,
    ALimit #>= BBase, BLimit #>= ALimit.

% region_name_translate(A, ASrc, DstBaseName, DstRegion)
region_name_translate(region(SrcId, block(ABase, ALimit)),
                      region(SrcId, block(ASrcBase, _)),
                      name(DstId, DstBase),
                      region(DstId, block(BBase, BLimit))) :-
    Offset #= ABase - ASrcBase,
    BBase #= Offset + DstBase,
    BLimit #= ALimit - ABase + BBase.


% translate(State, Source Region, Dest Name)
% The region needs to be matched exactly.
translate(_, SrcReg, DstName) :- translate(SrcReg, DstName).
translate(S, SrcReg, DstName) :- state_has_mapping(S, SrcReg, DstName).

%translate(_, region(SrcId, block(Base, _)), name(DstId, Base)) :- overlay(SrcId, DstId).

translate_region(S, SrcReg, region(DstId,block(DstBase, DstLimit))) :-
    translate(S, SrcCand, name(DstId, AbsDstBase)),
    region_region_contains(SrcReg, SrcCand),
    region_name_translate(SrcReg, SrcCand, name(DstId, AbsDstBase),
                          region(DstId,block(DstBase, DstLimit))).

translate_region(_, region(SrcId,B), region(DstId,B)) :-
    overlay(SrcId, DstId).

decodes_region(_, A, A).
decodes_region(S, A, B) :-
    translate_region(S, A, Next),
    decodes_region(S, Next, B).

% Decode until accept
resolves_region(S, A, B) :-
    decodes_region(S, A, B),
    accept(C),
    region_region_contains(B,C).

%% Decode configurable node input
%resolves_conf_region(S, region(SrcId, SrcB), region(DstId, DstB)) :-
%    decodes_region(S, region(SrcId, SrcB), region(DstId, DstB)),
%    configurable(DstId, _, _).

nodes_slots_avail(_, []).
nodes_slots_avail(S, [N | Ns]) :-
    nodes_slots_avail(S, Ns),
    state_has_avail(S, N, A),
    A #>= 0.


%%% Flattening. move to support?, materialize?
% TODO: The flatteing is shaky. It matches exactly on the
% translate and accept, but it should do a contains + translate
% the input region. However, it should not produce any wrong results.
% Assumptions: all input regions of accepts and translates all translates to
% that input region cover the whole input regions.

%    region_region_contains(Dst, AccC).
flat_step(region(SrcId, B), [], region(NextId, B)) :-
    overlay(SrcId, NextId).

flat_step(region(SrcId, block(SrcBase,SrcLimit)), [], region(NextId, block(NextBase, NextLimit))) :-
    translate(region(SrcId, block(SrcBase,SrcLimit)), name(NextId, NextBase)),
    NextLimit #= SrcLimit - SrcBase + NextBase.

flat_step(region(SrcId,_), [SrcId], region(NextId, _)) :-
    configurable(SrcId, _, NextId).

flat_step_rec(R, [], R).
flat_step_rec(Src, CN, Dst) :-
    flat_step(Src, CN1, Next),
    flat_step_rec(Next, CN2, Dst),
    append(CN1, CN2, CN).

flat(Src, CNodes, Dst) :-
    flat_step_rec(Src, CNodes, Dst),
    accept(Dst).







%%%%%

region_size(region(_, block(B,L)), Size) :-
    Size #= L - B + 1.

% In = Before + Split + After
% Case1:  No Before (return a -1,-1) for before
region_split(
    region(I, block(InB,InL)),
    region(I, block(-1,-1)),
    region(I, block(SplitB,SplitL)),
    region(I, block(AfterB,AfterL))) :-
        InL #>= InB,
        SplitL #>= SplitB,
        AfterL #>= AfterL,
        SplitB #= InB,
        SplitL #= AfterB - 1,
        AfterL #= InL.

region_split(
    region(I, block(InB,InL)),
    region(I, block(BeforeB,BeforeL)),
    region(I, block(SplitB,SplitL)),
    region(I, block(AfterB,AfterL))) :-
        InL #>= InB,
        BeforeL #>= BeforeB,
        SplitL #>= SplitB,
        AfterL #>= AfterL,
        BeforeB #= InB,
        SplitB #= BeforeL + 1,
        SplitL #= AfterB - 1,
        AfterL #= InL.

region_aligned(Region, Bits, BlockNum) :-
    Region = region(_, block(Base, _)),
    BlockSize is 2^Bits,
    BlockNum #>= 0,
    Base #= BlockNum * BlockSize.

alloc(S, Size, region(DstId,DstBlock), SNew) :-
   state_has_free(S, DstId, [FirstBlk | RmBlk]),
   region_region_contains(region(DstId,DstBlock), region(DstId, FirstBlk)),
   region_aligned(region(DstId,DstBlock), 21, NumBlock),
   region_size(region(DstId,DstBlock), Size),
   labeling([NumBlock]),
   region_split(region(DstId,FirstBlk), region(_,BeforeBlk),
                region(DstId,DstBlock), region(_,AfterBlk)  ),
   % Mark region free: Move Before to the end (its probably empty or
   % an unaligned padding), make the split of the first block the first element.
   state_remove_free(S, DstId, [FirstBlk | RmBlk], S1),
   append([AfterBlk], RmBlk, RmBlk1),
   append(RmBlk1, [BeforeBlk], RmBlk2),
   state_add_free(S1, DstId, RmBlk2, SNew).

alloc(S, Size, Dst, SrcId1, SNew) :-
    flat(region(SrcId1,_), CNodes1, DstReachable),
    nodes_slots_avail(S, CNodes1),
    region_region_contains(Dst, DstReachable),
    alloc(S, Size, Dst, SNew).

alloc(S, Size, Dst, SrcId1, SrcId2, SNew) :-
    flat(region(SrcId2,_), CNodes1, DstReachable),
    nodes_slots_avail(S, CNodes1),
    region_region_contains(Dst, DstReachable),
    alloc(S, Size, Dst, SrcId1, SNew).


% Translate region using the flat. Will not respect any dynamic translates and
% ignores configurable nodes.
region_translate_flat(region(SrcId, SrcB), region(DstId, DstB)) :-
	flat(region(SrcId, SrcBB), [], region(DstId, DstBB)),
	region_region_contains(region(SrcId, SrcB), region(SrcId, SrcBB)),
	region_region_contains(region(DstId, DstB), region(DstId, DstBB)),
	DstBB = block(DstBBBase, _),
    region_name_translate(region(SrcId,SrcB), region(SrcId, SrcBB),
					      name(DstId, DstBBBase), region(DstId,DstB)).

state_decrement_avail(S, NodeId, Amount, NewS) :-
    state_remove_avail(S, NodeId, C, S1),
    CNew is C - Amount,
    CNew #>= 0,
    state_add_avail(S1, NodeId, CNew, NewS).

bits_aligned_superregion(region(Id, block(Base, Limit)), Bits, region(Id, block(SuperBase, SuperLimit))) :-     
    BlockSize is 2^Bits,     
    SuperBase #= (Base // BlockSize) * BlockSize,     
    SuperLimit #= (Limit // BlockSize + 1) * BlockSize - 1.     

% Map case 1: We can reach Src without passing any reconfigurable nodes.
map(S, region(SrcId, SrcB), region(DstId, DstB), NewS) :-
    %region_translate_flat(region(SrcId, SrcB), region(DstId, DstB)),
    decodes_region(S, region(SrcId, SrcB), region(DstId, DstB)),
	S = NewS.

% Map case 2: We can reach Src with passing a reconfigurable node, but
% the node already has a mapping installed.
map(S, region(SrcId, SrcB), region(DstId, DstB), NewS) :-
	flat(region(SrcId, SrcBB), ConfNodes, region(DstId, DstBB)),
	region_region_contains(region(SrcId, SrcB), region(SrcId, SrcBB)),
	region_region_contains(region(DstId, DstB), region(DstId, DstBB)),
    nodes_slots_avail(S, ConfNodes),

    append(_, [LastConfNodeIn], ConfNodes),
    configurable(LastConfNodeIn, _, LastConfNodeOut),

    % Now, move Dst to the LastConfNodeOut NS
    region_translate_flat(region(LastConfNodeOut, ConfOutBlk), region(DstId,DstB)),
    % Check if we have a matching dynamic translate in S
    translate_region(S, region(LastConfNodeIn, ConfInBlk),
                     region(LastConfNodeOut, ConfOutBlk)),
    % Now recurse, since we reuse a mapping, nothing needs to be added to NewS.
    map(S, region(SrcId, SrcB), region(LastConfNodeIn, ConfInBlk), NewS).

% Map case 3: We can reach Src with passing a reconfigurable node, the configurable
% node needs a new mapping.
map(S, region(SrcId, SrcB), region(DstId, DstB), NewS) :-
	flat(region(SrcId, SrcBB), ConfNodes, region(DstId, DstBB)),
	region_region_contains(region(SrcId, SrcB), region(SrcId, SrcBB)),
	region_region_contains(region(DstId, DstB), region(DstId, DstBB)),
    nodes_slots_avail(S, ConfNodes),

    append(_, [LastConfNodeIn], ConfNodes),
    configurable(LastConfNodeIn, Bits, LastConfNodeOut),

    % Move Dst to the LastConfNodeOut NS
    region_translate_flat(region(LastConfNodeOut, ConfOutBlk), region(DstId,DstB)),

    % We get a bit aligned superregion of that, and allocate a same sized block
    % in the conf nodes input space. This will be the mapping that we insert.
    bits_aligned_superregion(
        region(LastConfNodeOut, ConfOutBlk), Bits,
        region(LastConfNodeOut, block(ConfOutBlABase, ConfOutBlALimit))),
    ConfOutBlASize #= ConfOutBlALimit - ConfOutBlABase + 1,
    BlockSize is 2^Bits,
    NumBlocks #= ConfOutBlASize // BlockSize,
    % Bits align conf node blocks.
    region_aligned(region(LastConfNodeIn, block(ConfInBlkB, ConfInBlkL)), Bits, _),
    alloc(S, ConfOutBlASize, region(LastConfNodeIn, block(ConfInBlkB, ConfInBlkL)), S1),
    state_add_mapping(S1,
        region(LastConfNodeIn, block(ConfInBlkB, ConfInBlkL)),
        name(LastConfNodeOut, ConfOutBlABase), S2),
    state_decrement_avail(S2, LastConfNodeIn, NumBlocks, S3),

    % Then, we use the just inserted mapping to translate our output address
    % back to the input address with the supermapping we just installed
    region_name_translate(
        region(LastConfNodeOut, ConfOutBlk),
        region(LastConfNodeOut, block(ConfOutBlABase, ConfOutBlALimit)),
        name(LastConfNodeIn, ConfInBlkB),
        ConfInR
    ),
    map(S3, region(SrcId, SrcB), ConfInR, NewS).


