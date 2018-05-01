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
    not(Head = Fact),
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
 * Modify State
 * ---------------------------------------------------------------------------
 */
state_decrement_avail(S, NodeId, Amount, NewS) :-
    state_has_avail(S, NodeId, C),
    state_remove_avail(S, NodeId, C, S1),
    CNew is C - Amount,
    CNew #>= 0,
    state_add_avail(S1, NodeId, CNew, NewS).


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



/*
 * ---------------------------------------------------------------------------
 * Utilities
 * ---------------------------------------------------------------------------
 */

region_size(region(_, block(B,L)), Size) :-
    Size #= L - B + 1.

% In = Before + Split + After
% Case1:  No Before (return a 1,0) for before
region_split(
    region(I, block(InB,InL)),
    region(I, block(1,0)),
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

% region_region_contains(A, B).. A \in B but not B \in A
region_region_contains(region(N, block(ABase, ALimit)),
                       region(N, block(BBase, BLimit))) :-
    ABase #>= BBase,  BLimit #>= ABase,
    ALimit #>= BBase, BLimit #>= ALimit.

% region_name_translate(A, ASrc, DstBaseName, DstRegion)
% A has to be contained in ASrc, then DstRegion will be the result
% of a ASrc->DstBaseName mapping.
region_name_translate(region(SrcId, block(ABase, ALimit)),
                      region(SrcId, block(ASrcBase, _)),
                      name(DstId, DstBase),
                      region(DstId, block(BBase, BLimit))) :-
    Offset #= ABase - ASrcBase,
    BBase #= Offset + DstBase,
    BLimit #= ALimit - ABase + BBase.

bits_aligned_superregion(region(Id, block(Base, Limit)), Bits,
        region(Id, block(SuperBase, SuperLimit))) :-     
    BlockSize is 2^Bits,     
    SuperBase #= (Base // BlockSize) * BlockSize,     
    SuperLimit #= (Limit // BlockSize + 1) * BlockSize - 1.     


/*
 * ---------------------------------------------------------------------------
 * Query the state.
 * ---------------------------------------------------------------------------
 */

% translate(State, Source Region, Dest Name)
% The region needs to be matched exactly.
translate(_, SrcReg, DstName) :- translate(SrcReg, DstName).
translate(S, SrcReg, DstName) :- state_has_mapping(S, SrcReg, DstName).

% Translate any region, it will search for a matching translate(..) and 
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

resolves_region(S, A, B) :-
    decodes_region(S, A, B),
    accept(C),
    region_region_contains(B,C).

nodes_slots_avail(_, []).
nodes_slots_avail(S, [N | Ns]) :-
    nodes_slots_avail(S, Ns),
    state_has_avail(S, N, A),
    A #>= 0.


free_list_member([Blk | Rm], Blk).
free_list_member([_ | Rm], Blk) :- free_list_member(Rm, Blk).


/*
 * ---------------------------------------------------------------------------
 * Free list
 * ---------------------------------------------------------------------------
 */

% Assumption: Free list is sorted.
% C1: Inserting block, is before existing block -> insert and terminate.
free_list_insert_sorted([block(FB,FL) | InLi], block(B,L), [block(B, L) | [block(FB,FL) | InLi]]) :- 
    FBM #= FB - 1,
    FBM #>= L.

% C2: Inserting block, is after existing block -> recurse and prepend
free_list_insert_sorted([block(FB,FL) | InLi], block(B,L), [block(FB,FL) | NextLi]) :- 
    %FL #=< B - 1,
    BM #= B - 1,
    BM #>= FL,
    free_list_insert_sorted(InLi, block(B,L), NextLi).

% C3: No more remaining blocks to check
free_list_insert_sorted([], block(B,L), [block(B,L)]).

% C1: Empty lists are coalesced
free_list_coalesce([], []).

% C2: One element lists are coalesced
free_list_coalesce([A], [A]).

% C3: Merge touching blocks 
free_list_coalesce([block(AB,AL) | [block(BB,BL) | InLi]], OutLi) :- 
    AL #= BB - 1,
    free_list_coalesce([block(AB, BL) | InLi], OutLi).

% C4: Ignore upcoming 
free_list_coalesce([block(AB,AL) | [block(BB,BL) | InLi]], [block(AB,AL) | NextLi]) :- 
    BBM #= BB - 1,
    BBM #>= AL, % not touching
    free_list_coalesce([block(BB, BL) | InLi], NextLi).

free_list_insert(Li, block(1,0), Li). % ignore empty blocks
free_list_insert(InLi, block(A,B), OutLi) :- 
    B #>= A,
    free_list_insert_sorted(InLi, block(A,B), TmpLi),
    free_list_coalesce(TmpLi, OutLi).

free_list_remove_first([Blk | Li], Blk, Li).
free_list_remove_first([FstBlk | Li], Blk, [FstBlk | NextLi]) :-
    not(FstBlk = Blk),
    free_list_remove_first(Li, Blk, NextLi).

% Returns holes in the free list. Min/Max are the values used for 
% Marking the beginning and end of the possible allocated region.
% Otherwise, we don't know where the 
%C1: Minimum until the first block.
free_list_allocated([block(AB,AL) | _], Min, Max, block(Min,ABM)) :- 
    ABM #= AB - 1,
    Min #=< ABM.

%C2: Between two blocks.
free_list_allocated([block(_,AL) | [block(BB,_) | _]], _, _, block(ALP, BBM)) :- 
    ALP #= AL + 1,
    BBM #= BB - 1.

%C4: At the end
free_list_allocated([block(_,AL)], _, Max, block(ALP, Max)) :- 
    ALP #= AL + 1,
    Max #>= AL.

% C4: Anything not involving the first block, make sure you can't use the min any
% more.
free_list_allocated([_ | Li], Min, Max, B) :- 
    free_list_allocated(Li, Max, Max, B).
    

        



%%% Flattening. move to support?, materialize?
% TODO: The flatteing is shaky. It matches exactly on the translate and accept,
% but it should do a contains + translate the input region. However, this
% version should produce a subset of the true results.  Assumptions: all input
% regions of accepts and translates all translates to that input region cover
% the whole input regions.

flat_step(region(SrcId, B), [], region(NextId, B)) :-
    overlay(SrcId, NextId).

flat_step(
    region(SrcId, block(SrcBase,SrcLimit)), [],
    region(NextId, block(NextBase, NextLimit))) :-
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


/*
 * ---------------------------------------------------------------------------
 * Alloc and Map
 * ---------------------------------------------------------------------------
 */
alloc(S, Size, region(DstId,DstBlock), SNew) :-
   state_has_free(S, DstId, FreeBlks),
   free_list_member(FreeBlks, CurrentBlk),
   region_region_contains(region(DstId,DstBlock), region(DstId, CurrentBlk)),
   region_aligned(region(DstId,DstBlock), 21, NumBlock),
   region_size(region(DstId,DstBlock), Size),
   labeling([NumBlock]),
   region_split(region(DstId,CurrentBlk), region(_,BeforeBlk),
                region(DstId,DstBlock), region(_,AfterBlk)  ),
   % Mark region free: Move Before to the end (its probably empty or
   % an unaligned padding), make the split of the first block the first element.
   state_remove_free(S, DstId, FreeBlks, S1),
   free_list_remove_first(FreeBlks, CurrentBlk, FreeBlks1),
   free_list_insert(FreeBlks1, AfterBlk, FreeBlks2),
   free_list_insert(FreeBlks2, BeforeBlk, FreeBlks3),
   state_add_free(S1, DstId, FreeBlks3, SNew).

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

free(S, region(Id,FreeBlock), SNew) :-
   state_has_free(S, DstId, FreeBlks),
   state_remove_free(S, DstId, FreeBlks1, S1),
   free_list_insert(FreeBlks1, FreeBlock, FreeBlks2),
   state_add_free(S1, DstId, FreeBlks2, SNew).

% Map Rec Works like map, but gets a hint (second last argument) which
% configurable nodes have to be passed.
% Map Rec case 1: We can reach Src without passing any reconfigurable nodes.
map_rec(S, region(SrcId, SrcB), region(DstId, DstB), [], NewS) :-
    decodes_region(S, region(SrcId, SrcB), region(DstId, DstB)),
    S = NewS.

% Map Rec case 2: We can reach Src with passing a reconfigurable node, but
% the node already has a mapping installed.
map_rec(S, region(SrcId, SrcB), region(DstId, DstB), ConfNodes, NewS) :-
    append(NextConfNodes, [LastConfNodeIn], ConfNodes),
    configurable(LastConfNodeIn, _, LastConfNodeOut),

    % Now, move Dst to the LastConfNodeOut NS
    decodes_region(S, region(LastConfNodeOut, ConfOutBlk), region(DstId,DstB)),

    % Check if we have a matching dynamic translate in S
    translate_region(S, region(LastConfNodeIn, ConfInBlk),
                     region(LastConfNodeOut, ConfOutBlk)),

    % Now recurse, since we reuse a mapping, nothing needs to be added to NewS.
    map_rec(S, region(SrcId, SrcB), region(LastConfNodeIn, ConfInBlk), NextConfNodes, NewS).

% Map Rec case 3: We can reach Src with passing a reconfigurable node, the configurable
% node needs a new mapping.
map_rec(S, region(SrcId, SrcB), region(DstId, DstB), ConfNodes, NewS) :-
    append(NextConfNodes, [LastConfNodeIn], ConfNodes),
    configurable(LastConfNodeIn, Bits, LastConfNodeOut),

    % Move Dst to the LastConfNodeOut NS
    decodes_region(S, region(LastConfNodeOut, ConfOutBlk), region(DstId,DstB)),

    % We get a bit aligned superregion of that, and allocate a same sized block
    % in the conf nodes input space. This will be the mapping that we insert.
    bits_aligned_superregion(
        region(LastConfNodeOut, ConfOutBlk), Bits,
        region(LastConfNodeOut, block(ConfOutBlABase, ConfOutBlALimit))),
    ConfOutBlASize #= ConfOutBlALimit - ConfOutBlABase + 1,
    BlockSize is 2^Bits,
    NumBlocks #= ConfOutBlASize // BlockSize,

    % Allocate in node for the configruable node
    region_aligned(region(LastConfNodeIn, block(ConfInBlkB, ConfInBlkL)), Bits, _),
    alloc(S, ConfOutBlASize, region(LastConfNodeIn, block(ConfInBlkB, ConfInBlkL)), S1),

    % Add mapping
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
    map_rec(S3, region(SrcId, SrcB), ConfInR, NextConfNodes, NewS).

map(S, region(SrcId, SrcB), DstUnresolvedR, NewS) :-
    % Resolve the Destination Region first.
    resolves_region(S, DstUnresolvedR, region(DstId, DstB)),
    flat(region(SrcId, SrcBB), ConfNodes, region(DstId, DstBB)),
    region_region_contains(region(SrcId, SrcB), region(SrcId, SrcBB)),
    region_region_contains(region(DstId, DstB), region(DstId, DstBB)),
    nodes_slots_avail(S, ConfNodes),
    map_rec(S, region(SrcId, SrcB), region(DstId, DstB), ConfNodes, NewS).


% Map Rec Works like map, but gets a hint (second last argument) which
% configurable nodes have to be passed.
% The semantics of unmap are as follows: For an unmapped destination region,
% it will be unmapped in all of the listed vspaces. A mapping will be removed
% if no other allocation passes through that mapping.


%% Unmap Rec case 1: We can reach Src without passing any reconfigurable nodes.
%% -> No state change implied
%unmap_rec(S, Src, Dst, [], S) :-
%    decodes_region(S, Src, Dst).
%
%% Unmap Rec case 2: 
%% We can reach Src with passing a reconfigurable node, the supermapping for
%% this node can be removed, because there is no other mapping hitting that block.
%unmap_rec(S0, region(SrcId, SrcB), region(DstId, DstB), ConfNodes, NewS) :-
%    append(NextConfNodes, [LastConfNodeIn], ConfNodes),
%    configurable(LastConfNodeIn, Bits, LastConfNodeOut),
%
%    % Now, move Dst to the LastConfNodeOut NS
%    decodes_region(S0, region(LastConfNodeOut, ConfOutBlk), region(DstId,DstB)),
%
%    % Check if we have a matching dynamic translate in S
%    translate_region(S0, region(LastConfNodeIn, ConfInBlk),
%                     region(LastConfNodeOut, ConfOutBlk)),
%
%    % Get the bits aligned superblock
%    bits_aligned_superregion(region(LastConfNodeIn, ConfInBlk), Bits, InSuperReg),
%    bits_aligned_superregion(region(LastConfNodeOut, ConfOutBlk), Bits,
%        region(LastConfNodeOut, block(OutSuperBase, OutSuperLimit))),
%
%    % Remove the mapping
%    state_remove_mapping(S0, InSuperReg, name(LastConfNodeOut, OutSuperBase), S1),
%
%    not( (
%        state_has_mapping(S1
%
%
%    % Now recurse, since we reuse a mapping, nothing needs to be added to NewS.
%    map_rec(S, region(SrcId, SrcB), region(LastConfNodeIn, ConfInBlk), NextConfNodes, NewS).
%
%% Unmap Rec case 3: 
%% We can reach Src with passing a reconfigurable node, the supermapping for
%% this node can NOT be removed, because there are other mapping hitting that block.
%unmap_rec(S, region(SrcId, SrcB), region(DstId, DstB), ConfNodes, NewS) :-
%
%unmap(S, region(SrcId, SrcB), DstUnresolvedR, NewS) :-
%    TODO TODO TODO : First free the SrcRegion so it will not appear in the overlaps
%    check.
%    % Resolve the Destination Region first.
%    resolves_region(S, DstUnresolvedR, region(DstId, DstB)),
%    flat(region(SrcId, SrcBB), ConfNodes, region(DstId, DstBB)),
%    region_region_contains(region(SrcId, SrcB), region(SrcId, SrcBB)),
%    region_region_contains(region(DstId, DstB), region(DstId, DstBB)),
%    unmap_rec(S, region(SrcId, SrcB), region(DstId, DstB), ConfNodes, NewS).
