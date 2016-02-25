% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf
%                 Kish Shen
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Component:	ECLiPSe III compiler
% Version:	$Id: compiler_peephole.ecl,v 1.28 2015/05/27 16:48:51 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(compiler_peephole).

:- comment(summary, "ECLiPSe III compiler - peephole optimizer").
:- comment(copyright, "Cisco Technology Inc").
:- comment(author, "Joachim Schimpf, Kish Shen").
:- comment(date, "$Date: 2015/05/27 16:48:51 $").

:- comment(desc, ascii("
    This pass does simple code improvements like:

	 - eliminating some instructions (e.g. nop)
	 - moving branch targets (e.g. to skip unneeded type tests)
	 - merging instructions (e.g. call+ret -> jmp)
    Takes a list of register-allocated, annotated code  Code is simplified,
    and finally the annotations are stripped, and a plain list of 
    instructions is returned, which can be fed into the assembler.
")).

:- use_module(compiler_common).

:- import meta_index/2 from sepia_kernel.


:- local struct(chunk(
    	cont,	% index of continuation chunk
	len,	% length of code list
	code,	% code list
	done)).	% 'done' if chunk already in final code list, else uninstantiated

         
% maximum size of code chunks that should be duplicated to save a branch
max_joined_len(2).


:- comment(simplify_code/3, [
    summary:"Strip annotations and do peephole optimizations",
    amode:simplify_code(+,-,+),
    args:[
	"AnnotatedCodeIn":"A list of annotated WAM code (struct(code))",
	"WamCodeOut":"A list of WAM instructions in lib(asm) format",
	"Options":"struct(options)"
    ],
    see_also:[struct(code)]
]).

/* 
simplify_code(+CodeList, -WamList +Options)
    Performs peephole optimisation on annotated code list CodeList, and
    peoduces an (unannotated) WamList of abstract instruction.

    The code can either be broken into `chunks', to allow for inter-chunk
    optimisations (such as moving of jump targets, joining of short
    continuations, and dead code removal), and then peephole optimisation
    is performed on each chunk, or it can be peephole optimised as a unit.

    If broken into chunks, the chunks are rejoined without any
    `dead' chunks (i.e. chunks that cannot be reached). The chunks may be
    rejoined in a different order from the original. Because some
    originally adjacent chunks are best if adjucent in the final code,
    these are rejoined early (before peephole optimisation), to ensure that
    they stay adjacent.

    The joining of short continuations will duplicate code, but reduces 
    branching, and the joined code allows for last call optimisation,
    if the continuation exits from the predicate. Short continuations are
    only joined if there are more than one chunk that continues into it;
    this is to prevent duplication of the code -- one for the continuation,
    and one for any branch targets. This optimisation greatly reduces the
    code size expansion. The alternative is to put a label into the first
    joined short continuation to act as a branch target, but the label
    prevents important optimisations (such as the last call opt) in the
    `boundary' code that is joined.
*/
:- export simplify_code/3.
simplify_code(CodeList, WamList, options{opt_level:OptLevel}) :-
	( OptLevel > 0 ->
	    flat_code_to_basic_blocks(CodeList, BasicBlockArray, Rejoins),
            make_nonreplicate_array(BasicBlockArray, Rejoins, NonRepArray),
            interchunk_simplify(BasicBlockArray, Rejoins, NonRepArray,
                                ReachedArray, Targets),
            compute_chunk_connections(BasicBlockArray, ReachedArray, Targets,
                ContArray, RefedArray, JoinedArray, Branches, BranchesT),
            ( for(_,1,max_joined_len), 
              param(BasicBlockArray, NonRepArray, ReachedArray,
                    ContArray,RefedArray,JoinedArray) 
            do
		join_short_continuations(BasicBlockArray, ReachedArray,
                    NonRepArray, ContArray, RefedArray, JoinedArray)
	    ),
            % add marked chunks in JoinedArray to Branches
            ( foreacharg(J, JoinedArray, Idx), param(RefedArray),
              fromto(BranchesT, B1,B2, []) do
                ( nonvar(J),
                  arg(Idx, RefedArray, Refed),
                  nonvar(Refed)
                -> 
                    % add to branches that needs to be processed when
                    % joining branches back together for continuations
                    % that have been joined early,  and is ref'ed
                    B1 = [Idx|B2]
                ;
                    B1 = B2
                )
            ),
	    ( foreacharg(chunk{code:Chunk,cont:Cont,len:Len,done:Done},
                         BasicBlockArray,I), 
              param(BasicBlockArray) do
                ( var(Done) ->
                    simplify_chunk(Chunk, SimplChunk),
                    % Len is approximate after simplify! 
                    setarg(I, BasicBlockArray, chunk{code:SimplChunk,len:Len,cont:Cont})
                ;
                    true
                )
            ),
	    basic_blocks_to_flat_code(BasicBlockArray, Branches, JoinedArray, ReachedArray, CodeList1),
            simplify_chunk(CodeList1, SimpCodeList)  % run simplify again on entire code list
	;
	    simplify_chunk(CodeList, SimpCodeList)
        ),
        ( foreach(code{instr:Instr},SimpCodeList), foreach(Instr,WamList) do
            true
        ).

compute_chunk_connections(BasicBlockArray, ReachedArray, Targets, ContArray,
    RefedArray, JoinedArray, Branches, BranchesT) :-
        functor(BasicBlockArray, F, N),
        functor(ContArray, F, N),
        functor(RefedArray,F, N),
        functor(JoinedArray, F, N),
        arg(1, ContArray, r([])), % Chunk 1 marked as cont'ed into 
        ( foreacharg(chunk{cont:Cont}, BasicBlockArray, I),
         param(ContArray,ReachedArray) do
            % ContArray: determine chunks that are continuations
            % from 0  (ContArray[n] is var), 
            %      1  (ContArray[n] = r(M), M is var)
            %      1+ (ContArray[n] = r([])
            % other chunks
            ( arg(I, ReachedArray, Reached),
              nonvar(Reached), % is a reached chunk
              integer(Cont), Cont > 0
            ->
                arg(Cont, ContArray, CStatus),
                (var(CStatus) -> CStatus = r(_) ; CStatus = r([]))
            ;
                true
            )
        ),
        % RefedArray: 
        % RefedArray[n] is var if it is not the target of any ref()
        % RefedArray[n] = [] if it is the target of one or more ref()
        ( foreach(T, Targets), param(RefedArray) do
            arg(T, RefedArray, [])
        ),
        ( foreacharg(IsCont,ContArray, I), foreacharg(Refed,RefedArray), 
          fromto(Branches, Branches0,Branches1, BranchesT) do
            ( var(IsCont), nonvar(Refed) ->
                % chunk is not continued into, but is referenced, 
                % i.e. first chunk of a branch
                Branches0 = [I|Branches1]
            ;
                Branches0 = Branches1
            )
        ).

% Take a simple list of annotated code, and cut it up at the labels.
% The result are code chunks that correspond to basic blocks.
% Number each chunk and instantiate the corresponding Label to this number.
% Enter the chunk into an array indexed by the chunk number.
%
% Also determine if two consecutive chunks are `contiguous' chunks, i.e.
% the instructions at the splitting of the chunks should be contiguous in
% the final code if possible. These chunks will be rejoined as soon as 
% possible, unless the earlier chunk is unreachable. The first chunk numbers 
% for each of these contiguous chunks are collected in Rejoins
%
% 
% We already do some opportunistic simplification here:
% - removing the code{} wrapper
% - eliminating nops
% - eliminating redundant consecutive labels (unifying the Labels)
% - eliminating unreachable code between branch() and the next label()
% - make indirect branch() (branch() to another branch()) direct
%
% During code traversal, we maintain a State variable with the values:
%  labels:	the previous instruction was a label (not in rejoin state)
%  normal:	we are in the middle of a chunk
%  unreachable:	the current code is unreachable
%  rejoin:      the previous instruction was a `contiguous' instruction, i.e.
%               it should be contiguous with the following instruction
%  rejoinlabels: the previous instruction was a label, encountered while
%               state was rejoin

flat_code_to_basic_blocks(AnnCode, BasicBlockArray, Rejoins) :-
	(
	    fromto(AnnCode, [AnnInstr|Code1], Code1, []),
	    fromto(FirstChunk,Chunk0,Chunk1,LastChunk),
	    fromto(FirstChunk,Tail0,Tail1,[]),
            fromto(1,Label0,Label1,_),	% last label (while in
                                        % labels/rejoinlabels state)
	    fromto(Chunks,Chunks0,Chunks1,Chunks2),
	    fromto(0,N0,N1,N),			% chunk number
	    fromto(0,Len0,Len1,Len),		% chunk length
            fromto([],Rejoins0,Rejoins1,Rejoins), % rejoin chunk numbers
            fromto(labels,State,State1,EndState)
	do
            AnnInstr = code{instr:Instr},
            ( Instr = label(L) ->
		verify var(L),
                ( State == rejoin ->
                    State1 = rejoinlabels 
                ; State == rejoinlabels ->
                    State1 = rejoinlabels
                ; 
                    State1 = labels
                ),
                Label1 = L,
                Rejoins0 = Rejoins1,
                N1 = N0,
                ( (State == labels ; State == rejoinlabels) ->
                    Label1 = Label0,		% a redundant label
                    Len1 = Len0,
                    Chunk1 = Chunk0,
                    Tail0 = Tail1,
                    Chunks0 = Chunks1
                ; State == unreachable ->
                    Len1 = 0,
                    Chunk1 = Tail1,	% start a new chunk
                    Tail0 = [],		% terminate the previous chunk
                    Chunks0 = Chunks1	% dont't collect finished chunk
                ; % State == normal ; State == rejoin
                    Len1 = 0,
                    Chunk1 = Tail1,	% start a new chunk
                    Tail0 = [],		% terminate the previous chunk
                    % collect finished chunk (L is uninstantiated)
                    Chunks0 = [chunk{code:Chunk0,len:Len0,cont:L}|Chunks1]
                )

	    ; Instr = branch(ref(L)) ->
                N1 = N0,
                Label1 = none,
                Rejoins1 = Rejoins0,
                State1 = unreachable,
                ( (State == labels ; State == rejoinlabels) ->
                    % branch follows immediately from a label
                    Label0 = L,		% get rid of indirect label
                    Len0 = Len1,
		    Chunk0 = Chunk1,
		    Chunks0 = Chunks1,
		    Tail0 = Tail1
                ; State == unreachable ->
		    succ(Len0, Len1),
		    Chunk0 = Chunk1,
		    Chunks0 = Chunks1,
		    Tail0 = Tail1
		; atom(L)  ->
		    Len1 = 0,
		    succ(Len0, Len2),
		    Chunk1 = Tail1,		% start a new chunk
		    Tail0 = [AnnInstr],		% terminate the previous chunk
		    Chunks0 = [chunk{code:Chunk0,len:Len2,cont:0}|Chunks1] % collect finished chunk
		;
                    Len1 = 0,
		    Chunk1 = Tail1,		% start a new chunk
		    Tail0 = [],			% terminate the previous chunk
		    Chunks0 = [chunk{code:Chunk0,len:Len0,cont:L}|Chunks1]	% collect finished chunk
		)

            ; is_nop(Instr) ->
                Rejoins0 = Rejoins1,
                Label0 = Label1,
                N1 = N0,
                Len1 = Len0,
                Chunk1 = Chunk0,
		Chunks1 = Chunks0,
	    	Tail1 = Tail0,
                State = State1   % keep same state

            ; 
                Label1 = none,
                ( (State == labels ; State == rejoinlabels) ->
                    % init. current chunk -- we are in code following a label
                    % that we want to keep
                    Label0 = N1,	% instantiate the previous label
                    succ(N0, N1),	% current chunk number
                    (State == rejoinlabels ->
                        Rejoins1 = [N0|Rejoins0] % current is a rejoin chunk
                    ;
                        Rejoins1 = Rejoins0
                    )
                ;
                    N0 = N1,
                    Rejoins1 = Rejoins0
                ),
                ( unconditional_transfer(Instr) ->
                    State1 = unreachable,
                    ( State == unreachable ->
                        succ(Len0, Len1),
                        Chunk1 = Chunk0,
                        Chunks1 = Chunks0,
                        Tail1 = Tail0
                    ;
                        Len1 = 0,
                        succ(Len0, Len2),
                        Chunk1 = Tail1,		% start a new chunk
                        Tail0 = [AnnInstr],	% terminat current chunk
			% collect finished chunk
                        Chunks0 = [chunk{code:Chunk0,len:Len2,cont:0}|Chunks1]
                    )
                
                ; 
                    succ(Len0, Len1),
                    Chunk1 = Chunk0,
                    Chunks0 = Chunks1,
                    Tail0 = [AnnInstr|Tail1],	% append this instruction
                    next_state(Instr, State, State1)
                )
            )
        ),
	( EndState = unreachable ->
	    Chunks2 = []
	;
	    Chunks2 = [chunk{code:LastChunk,len:Len,cont:0}]
	),
	verify length(Chunks, N),
	BasicBlockArray =.. [[]|Chunks].

% determine the next state while in the middle of traversing code
next_state(Instr, State, NextState) :-
        ( State == unreachable -> 
            NextState = unreachable 
        ; 
            ( indexing_branch(Instr) -> 
                NextState = rejoin % following code should be contiguous
            ; 
                NextState = normal
            )
        ).

    % Unconditional control transfer instructions
    % Only needs to list instructions generated by the code generator
    unconditional_transfer(branch(_)).
    unconditional_transfer(exit).
    unconditional_transfer(exitd).
    unconditional_transfer(failure).
    unconditional_transfer(ret).
    unconditional_transfer(retd).
    unconditional_transfer(retn).
    unconditional_transfer(jmp(_)).
    unconditional_transfer(jmpd(_)).
    unconditional_transfer(chain(_)).
    unconditional_transfer(chaind(_)).
    unconditional_transfer(trust(_,_)).
    unconditional_transfer(trust_inline(_,_,_)).
    % generated instructions from peephole optimiser
    unconditional_transfer(move_chain(_,_,_)).
    unconditional_transfer(branchs(_,_)).

    % unconditional control transfer instruction to outside the predicate
    % Subset of unconditional_transfer/1, plus extra instr from peephole
    % optimisation.  Keep the two in sync!
    % Separate definitions for the two to avoid cuts in merged definition
    unconditional_transfer_out(exit).
    unconditional_transfer_out(exitd).
    unconditional_transfer_out(failure).
    unconditional_transfer_out(ret).
    unconditional_transfer_out(retd).
    unconditional_transfer_out(retn).
/*    unconditional_transfer_out(jmp(_)).
    unconditional_transfer_out(jmpd(_)).
    unconditional_transfer_out(chain(_)).
    unconditional_transfer_out(chaind(_)).
    % generated instr from peephole optimiser 
    unconditional_transfer_out(branchs(_,_)).
    unconditional_transfer_out(jmpd(_,_)).*/

    % these are indexing branch instructions with a default fall-through
    % case. It is desirable that the fall-through code is contiguous with
    % the instruction rather than a branch to somewhere else. However, if 
    % code following is split into a new chunk, the two chunks should be 
    % rejoined as soon as possible to ensure the final code is contiguous.
    indexing_branch(try_me_else(_,_,_)).
    indexing_branch(try(_,_,_)).
    indexing_branch(retry_me_else(_,_)).
    indexing_branch(retry_me_inline(_,_,_)).
    indexing_branch(retry(_,_)).
    indexing_branch(retry_inline(_,_,_)).
    indexing_branch(trust_me(_)).
    indexing_branch(trust_me_inline(_,_)).


%----------------------------------------------------------------------
% inter-chunk reachability and simplifications
%----------------------------------------------------------------------

% interchunk_simplify is intended to do peephole optimisations across
% different chunks, connected by refs.
% mark all reachable chunks by following the continuations and refs.
% Rejoin any contiguous chunks, unless its first chunk is unreachable

interchunk_simplify(BasicBlockArray, Rejoins, NonRepArray, ReachedArray, Targets) :-
        find_reached_chunks(BasicBlockArray, NonRepArray, ReachedArray, Targets),
        rejoin_contiguous_chunks(BasicBlockArray, ReachedArray, Rejoins).

find_reached_chunks(BasicBlockArray, NonRepArray, ReachedArray, Targets) :-
        functor(BasicBlockArray, F, N),
        functor(ReachedArray, F, N),
        functor(TargetArray, F, N), 
        N1 is N + 1,  % start of extra label id
        arg(1, ReachedArray, []), % first chunk
        arg(1, BasicBlockArray, Chunk),
        find_reached_chunks_(Chunk, BasicBlockArray, NonRepArray, ReachedArray, 
                             Targets, Targets, TargetArray, N1, _).

find_reached_chunks_(Chunk, BasicBlockArray, NonRepArray, ReachedArray, Targets,
                     TargetsT0, TargetArray, NL0, NL) :-
        Chunk = chunk{cont:Cont,code:Code},
        process_chunk_targets(Code, BasicBlockArray, Cont, NonRepArray, TargetArray, 
                              NL0, NL1, TargetsT0, TargetsT1, NewCode),
        setarg(code of chunk, Chunk, NewCode),
        ( integer(Cont), Cont > 0,    % continue to another chunk 
          arg(Cont, ReachedArray, ReachedCont), 
          var(ReachedCont)  % that chunk have not yet been reached
        -> 
            ReachedCont =  [],  % Mark as reached
            arg(Cont, BasicBlockArray, ContChunk)
        ; 
            true
        ),
        ( nonvar(ContChunk) ->
            find_reached_chunks_(ContChunk, BasicBlockArray, NonRepArray, ReachedArray, 
                                 Targets, TargetsT1, TargetArray, NL1, NL)
        ;
            find_chunks_in_branch(Targets, BasicBlockArray, NonRepArray, ReachedArray,
                                  TargetsT1, TargetArray, NL1, NL)
        ).

find_chunks_in_branch(Targets, BasicBlockArray, NonRepArray, ReachedArray, 
                      TargetsT, TargetArray, NL0, NL) :-
        ( var(Targets) ->
            true % queue empty, done
        ;
            Targets = [Target|Targets0],
            arg(Target, ReachedArray, RefStatus),
            ( var(RefStatus) ->  % not yet processed
                RefStatus = [], % process it now, and mark it as reached
                arg(Target, BasicBlockArray, Chunk),
                find_reached_chunks_(Chunk, BasicBlockArray, NonRepArray, ReachedArray,
                    Targets0, TargetsT, TargetArray, NL0, NL)
            ;
                find_chunks_in_branch(Targets0, BasicBlockArray, NonRepArray,
                    ReachedArray, TargetsT, TargetArray, NL0, NL)
            )
        ).

% Find all ref()s that refer to unprocessed chunks and queue the labels
% also perform inter-chunk optimisations by looking at the instructions
% in the original chunk and the chunks being ref'ed
process_chunk_targets([Code|Rest0], BasicBlockArray, Cont, NonRepArray, TargetArray, 
                      NL0, NL, TargetsT0, TargetsT, New) ?-
        Code = code{instr:I},        
        process_instr_targets(I, Code, BasicBlockArray, Cont, NonRepArray, TargetArray,
                              Rest0, Rest1, NL0, NL1, TargetsT0, TargetsT1, New, New1),
        process_chunk_targets(Rest1, BasicBlockArray, Cont, NonRepArray, TargetArray,
                              NL1, NL, TargetsT1, TargetsT, New1).
process_chunk_targets([], _, _, _, _, NL0, NL, TargetsT0, TargetsT, New) ?-
            NL0 = NL, TargetsT0 = TargetsT, New = [].


process_instr_targets(atom_switch(a(A),Table,ref(Def)), Code, BasicBlockArray, _, NonRepArray, TargetArray,
    Rest0, Rest, NL0, NL, TargetsT0, TargetsT, New, NewT) ?-
        !,
        Rest0 = Rest,
        mark_and_accumulate_targets(Def, TargetArray, TargetsT0, TargetsT1),
        update_struct(code, [instr:atom_switch(a(A),NewTable,ref(Def))], Code, NewCode),
        New = [NewCode|NewT],
        ( foreach(Atom-Ref, Table), 
          foreach(Atom-NewRef, NewTable),
          fromto(TargetsT1, TT2,TT3, TargetsT),
          fromto(NL0, NL1,NL2, NL),
          param(BasicBlockArray,NonRepArray,TargetArray,A)
        do
            skip_subsumed_instr([(get_atom(a(A),Atom),next), 
                                 (in_get_atom(a(A),Atom),next)], 
                                Ref, BasicBlockArray, NonRepArray, TargetArray, NL1, NL2, TT2, TT3, NewRef)
        ).
process_instr_targets(functor_switch(a(A),Table,ref(Def)), Code, BasicBlockArray, _, NonRepArray, 
    TargetArray, Rest0, Rest, NL0, NL, TargetsT0, TargetsT, New, NewT) ?- !,
        Rest0 = Rest,
        mark_and_accumulate_targets(Def, TargetArray, TargetsT0, TargetsT1),
        update_struct(code, [instr:functor_switch(a(A),NewTable,ref(Def))], Code, NewCode),
        New = [NewCode|NewT],
        ( foreach(Func-FRef, Table), 
          foreach(Func-NewFRef, NewTable),
          fromto(TargetsT1, TT2,TT3, TargetsT),
          fromto(NL0, NL1,NL2, NL),
          param(BasicBlockArray,NonRepArray,TargetArray,A)
        do
            skip_subsumed_instr([(get_structure(a(A),Func,ReadRef),ReadRef),
                                 (in_get_structure(a(A),Func,InRef),InRef)], 
                 FRef, BasicBlockArray, NonRepArray, TargetArray, NL1,NL2, TT2,TT3, NewFRef)
        ). 
process_instr_targets(integer_switch(a(A),Table,ref(Def)), Code, BasicBlockArray, _, NonRepArray,  
    TargetArray, Rest0, Rest, NL0, NL, TargetsT0, TargetsT, New, NewT) ?- !,
        Rest0 = Rest,
        mark_and_accumulate_targets(Def, TargetArray, TargetsT0, TargetsT1),
        update_struct(code, [instr:integer_switch(a(A),NewTable,ref(Def))], Code, NewCode),
        New = [NewCode|NewT],
        ( foreach(Int-Ref, Table), 
          foreach(Int-NewRef, NewTable),
          fromto(TargetsT1, TT2,TT3, TargetsT),
          fromto(NL0, NL1,NL2, NL),
          param(BasicBlockArray,NonRepArray,TargetArray,A)
        do
            skip_subsumed_instr([(get_integer(a(A),Int),next),
                                 (in_get_integer(a(A),Int),next)], 
                                Ref, BasicBlockArray, NonRepArray, TargetArray, NL1, NL2, TT2, TT3, NewRef)
                                
        ). 
process_instr_targets(list_switch(a(A),ListRef,NilRef,ref(VarLab)), Code, BasicBlockArray, _, NonRepArray, TargetArray, 
    Rest0, Rest, NL0, NL, TargetsT0, TargetsT, New, NewT) ?- !,
        Rest0 = Rest,
        mark_and_accumulate_targets(VarLab, TargetArray, TargetsT0, TargetsT1),
        update_struct(code, [instr:list_switch(a(A),NewListRef,NewNilRef,ref(VarLab))], Code, NewCode),
        New = [NewCode|NewT],
        skip_subsumed_instr([(get_list(a(A),ReadRef),ReadRef),
                            (in_get_list(a(A),InRef),InRef)], ListRef, 
                            BasicBlockArray, NonRepArray, TargetArray, NL0, NL1, TargetsT1, TargetsT2, NewListRef),
        skip_subsumed_instr([(get_nil(a(A)),next),
                             (in_get_nil(a(A)),next)], 
                            NilRef, BasicBlockArray,  NonRepArray, TargetArray, NL1, NL, TargetsT2, TargetsT, NewNilRef).
process_instr_targets(switch_on_type(a(A),SwitchList), Code, BasicBlockArray, Cont, NonRepArray, TargetArray, 
    Rest0, Rest, NL0, NL, TargetsT0, TargetsT, New, NewT) ?- !,
        update_struct(code, [instr:switch_on_type(a(A),NewSwitchList)], Code, NewCode),
        New = [NewCode|NewT1],
        ( Rest0 == [] ->
            % end of chunk, the fall through case (type = var) continues to 
            % Cont
            Rest0 = Rest,
            ContRef = ref(Cont),
            subsumed_type_instr(free, A, VSkipCands),
            skip_subsumed_instr(VSkipCands, ContRef, BasicBlockArray, NonRepArray,
                                TargetArray, NL0, NL1, TargetsT0, TargetsT1, NewVRef),
            ( ContRef == NewVRef ->
                % no subsumed instruction found, no change to following code
                NewT1 = NewT
            ;
                % add a branch to new label
                NewT1 = [code{instr:branch(NewVRef)}|NewT]
            )
        ;
            % code following switch_on_type in chunk, do nothing with it
            % for now (could check for subsumed type test that is skipped
            Rest0 = Rest,
            NewT1 = NewT,
            NL0  = NL1,
            TargetsT0 = TargetsT1
        ),
        (
            foreach(Type:Ref, SwitchList),
            foreach(Type:NewRef,NewSwitchList),
            fromto(TargetsT1, TT1,TT2, TargetsT),
            fromto(NL1, NL2,NL3, NL),
            param(BasicBlockArray,NonRepArray,TargetArray,A)
        do
            ( subsumed_type_instr(Type, A, SkipCandidates) -> 
                skip_subsumed_instr(SkipCandidates, Ref, BasicBlockArray,
                    NonRepArray, TargetArray, NL2,NL3, TT1,TT2, NewRef)
            ;
                Ref = ref(Label),
                NL2 = NL3,
                NewRef = Ref,
                mark_and_accumulate_targets(Label, TargetArray, TT1, TT2) 
            )
        ).
process_instr_targets(Xs, Code, _BasicBlockArray, _Cont, _NonRepArray, TargetArray, 
                      Rest, Rest, NL, NL, TargetsT0, TargetsT, New, NewT) :-
        New = [Code|NewT],
        find_targets(Xs, TargetArray, TargetsT0, TargetsT).


find_targets(ref(L), TargetArray, TargetsT0, TargetsT1) ?- !, 
        mark_and_accumulate_targets(L, TargetArray, TargetsT0, TargetsT1).
find_targets(Xs, TargetArray, TargetsT0, TargetsT) :- 
    	compound(Xs), !,
	(
	    foreacharg(X,Xs),
            fromto(TargetsT0, TargetsT1,TargetsT2, TargetsT),
	    param(TargetArray)
	do
            find_targets(X, TargetArray, TargetsT1, TargetsT2)
	). 
find_targets(_, _, TargetsT, TargetsT).

% mark_and_accumulate_targets checks if T is a new target, and mark it in
% TargetArray if it is new, and add it to the Targets list
mark_and_accumulate_targets(T, TargetArray, TargetsT0, TargetsT1) :-
        (
            integer(T),
            arg(T, TargetArray, IsNew),
	    var(IsNew)
	->
	    TargetsT0 = [T|TargetsT1],
            IsNew = []  % mark target chunk as reached
	;
	    TargetsT0 = TargetsT1
	).

% skip_subsumed_instr checks to see if the chunk referenced by BaseRef
% starts with SkipInstr, which is one of the Candidates of instructions 
% that is subsumed. If it does, change BaseRef to skip the 
% instruction, either to the following instruction, or to the target
% given in NewRef
skip_subsumed_instr(Candidates, BaseRef, BasicBlockArray, NonRepArray, 
                    TargetArray,  NL0, NL1, TargetsT0, TargetsT1, NewRef) :-
        BaseRef = ref(BaseTarget),
        ( integer(BaseTarget),
          arg(BaseTarget, BasicBlockArray, Chunk),
          Chunk = chunk{code:Code},
          match_skipped_instr(Candidates, SkipInstr, NewRefPos, Code, Rest)
          %Code = [SkipInstr|Rest] % Base chunk has skipped instr
        ->  
            ( NewRefPos == next ->  % new target follows skipped instr  
                ( Rest = [code{instr:label(NL)}|_] ->
                    NewCode = [code{instr:SkipInstr}|Rest], % has a label already
                    NL1 = NL0
                ;
                    NL = NL0,               % add a new label
                    NewCode = [code{instr:SkipInstr},code{instr:label(NL)}|Rest],
                    NL1 is NL0 + 1
                ),
                arg(BaseTarget, NonRepArray, []), % chunk now non-replicatable
                NewRef = ref(NL),  % move target to after skipped instr 
                setarg(code of chunk, Chunk, NewCode),
                % jumping into chunk BaseTarget, so mark it if needed
                mark_and_accumulate_targets(BaseTarget, TargetArray, TargetsT0, TargetsT1)
            ; NewRefPos = ref(NewTarget) -> % new target is an existing label
                NL1 = NL0,
                NewRef = NewRefPos,
                mark_and_accumulate_targets(NewTarget, TargetArray, TargetsT0, TargetsT1)
            ;   % don't know where new target is
                TargetsT0 = TargetsT1,
                NL0 = NL1,
                NewRef = BaseRef
            )
        ;   % SkipInstr not matched
            NL0 = NL1,
            NewRef = BaseRef,
            mark_and_accumulate_targets(BaseTarget, TargetArray, TargetsT0, TargetsT1)
        ).

match_skipped_instr([(Candidate,NewRef0)|Candidates], SkipInstr, NewRef, Code, Rest) :-
        ( Code = [code{instr:Candidate}|Rest] ->
            SkipInstr = Candidate,
            NewRef0 = NewRef
        ;
            match_skipped_instr(Candidates, SkipInstr, NewRef, Code, Rest)
        ).


% the (mainly) type test instructions that are subsumed by the type
% switches of switch_on_type
subsumed_type_instr(meta, A, [(bi_var(a(A)),next),(bi_meta(a(A)),next),(in_get_meta(a(A),_),next)]).
subsumed_type_instr([], A, [(get_nil(a(A)),next),(bi_atom(a(A)),next),
                            (bi_atomic(a(A)),next),(bi_callable(a(A)),next),
                            (bi_nonvar(a(A)),next),(in_get_nil(a(A)),next),
                            (bi_nonvar(a(A)),next)]).
subsumed_type_instr(atom, A, [(bi_atom(a(A)),next),(bi_atomic(a(A)),next),
                              (bi_callable(a(A)),next),(bi_nonvar(a(A)),next)]).
subsumed_type_instr(bignum, A, [(bi_number(a(A)),next),(bi_integer(a(A)),next),
                                (bi_bignum(a(A)),next),(bi_atomic(a(A)),next),
                                (bi_nonvar(a(A)),next)]).
subsumed_type_instr(integer, A, [(bi_number(a(A)),next),(bi_integer(a(A)),next),
                                (bi_atomic(a(A)),next),(bi_nonvar(a(A)),next)]).
subsumed_type_instr(breal, A, [(bi_number(a(A)),next),(bi_real(a(A)),next),
                               (bi_breal(a(A)),next),(bi_nonvar(a(A)),next),(bi_atomic(a(A)),next)]).
subsumed_type_instr(double, A, [(bi_number(a(A)),next),(bi_real(a(A)),next),
                               (bi_float(a(A)),next),(bi_nonvar(a(A)),next),(bi_atomic(a(A)),next)]).
subsumed_type_instr(goal, A, [(bi_atomic(a(A)),next),(bi_nonvar(a(A)),next)]).
subsumed_type_instr(handle, A, [(bi_is_handle(a(A)),next),(bi_nonvar(a(A)),next),(bi_atomic(a(A)),next)]).
subsumed_type_instr(list, A, [(bi_compound(a(A)),next),
                                (bi_callable(a(A)),next),(bi_nonvar(a(A)),next)]).
subsumed_type_instr(rational, A, [(bi_number(a(A)),next),(bi_rational(a(A)),next),(bi_nonvar(a(A)),next),
				(bi_atomic(a(A)),next)]).
subsumed_type_instr(string, A, [(bi_atomic(a(A)),next),(bi_string(a(A)),next),(bi_nonvar(a(A)),next)]).
subsumed_type_instr(structure, A, [(bi_compound(a(A)),next),
				(bi_callable(a(A)),next),(bi_nonvar(a(A)),next)]).
subsumed_type_instr(free, A, [(bi_var(a(A)),next),(bi_free(a(A)),next)]).

% rejoin adjacent chunks that should be contiguous if the first chunk
% is reached. Rejoins must have later chunks first in the list because more 
% after rejoining two chunks, the rejoined chunk can be rejoined with the
% previous chunk
rejoin_contiguous_chunks(BasicBlockArray, ReachedArray, Rejoins) :-
        (foreach(R, Rejoins), param(BasicBlockArray, ReachedArray) do
            arg(R, BasicBlockArray, chunk{len:Len,code:Code}),
            arg(R, ReachedArray, Reached),
            ( nonvar(Reached) -> % first chunk of rejoin chunks reached? 
                succ(R, NextC),  % yes, rejoin with succeeding chunk
                % succeeding chunk mark as processed
                arg(NextC, BasicBlockArray, NextChunk),
                NextChunk = chunk{len:NextLen,code:NextCode,cont:NextCont,done:done},
                append(Code, [code{instr:label(NextC)}|NextCode],NewCode),
                NewLen is Len + NextLen,
                setarg(R, BasicBlockArray, chunk{len:NewLen,code:NewCode, cont:NextCont}),
                setarg(cont of chunk, NextChunk, 0)  % get rid of the old continuation in the discarded chunk
            ;
                % first chunk not reached, so don't join
                true
            )
        ).

% NonRepArray indicates which chunks should not be replicated -- currently
% chunks that contains labels (i.e. rejoined chunks)
make_nonreplicate_array(BasicBlockArray, Rejoins, NonRepArray) :-
        functor(BasicBlockArray, F, A),
        functor(NonRepArray, F, A),
        ( foreach(R, Rejoins), param(NonRepArray) do
            R1 is R + 1,
            arg(R, NonRepArray, []), 
            arg(R1, NonRepArray, [])
        ).

% Joins a chunk to its continuation if the continuation is short, and can
% be replicated -- i.e. there are no labels inside the continuation chunk.
% An optimisation is that if the continuation immediately jumps elsewhere,
% the continuation of the chunk is simply updated.
join_short_continuations(BasicBlockArray, ReachedArray, NonRepArray, ContArray, RefedArray, JoinedArray) :-
        (
	    foreacharg(Chunk,BasicBlockArray,I),
	    param(BasicBlockArray,NonRepArray,ReachedArray,ContArray,RefedArray,JoinedArray)
	do
            Chunk = chunk{cont:Cont,len:Len,code:Code,done:Done},
            ( Cont == 0 ->
                true % no continuatipn to join
            ; nonvar(Done) ->
                true % nonvar if chunk discarded, don't join
            ;
                arg(Cont, BasicBlockArray, NextChunk),
                NextChunk = chunk{len:ContLen,code:ContCode,cont:ContCont},
                arg(I, ReachedArray, ReachedI),
                ( var(ReachedI) ->
                    true % chunk not reached, don't join
                ; arg(Cont, NonRepArray, NonRepC), nonvar(NonRepC)  ->
                    true  % next chunk should not be replicated -- don't join
                ; arg(Cont, ContArray, ContStatus), ContStatus \== r([]),
                  arg(Cont, RefedArray, Refed), nonvar(Refed) ->
                    true % cont chunk is continuation for one (i.e. this) chunk
                         % only, and is referenced, don't join now
                ;
                    arg(Cont, BasicBlockArray, NextChunk),
                    NextChunk = chunk{len:ContLen,code:ContCode,cont:ContCont},
                    ( ContLen > max_joined_len ->
                        true
                    ; 
                        % Join NextChunk to chunk I.
                        % mark NextChunk as joined, and update ContArray
                        % if this is not the first time NextChunk is joined
                        % because NextChunk is now replicated and so is its
                        % continuation (ContCont)
                        arg(Cont, JoinedArray, Joined),
                        ( var(Joined) ->
                            Joined = []
                        ;
                            (ContCont > 0 ->
                                % make sure ContCont is now marked as
                                % having multiple continuations
                                arg(ContCont, ContArray, r([]))
                            ;
                                true
                            )
                        ),

                        append(Code, ContCode, NewCode),
                        NewLen is Len+ContLen,
                        setarg(I, BasicBlockArray, chunk{code:NewCode,len:NewLen,cont:ContCont})
                    )
                )
            )
        ).


% Flatten the BasicBlockArray into a WAM code list.
% We emit only the reachable chunks, by collecting all ref()s in the code,
% and filter for those ref()s that are not continued into, i.e. start of
% branches, plus chunks that have been joined but are ref'ed as well..
% The done-flag in the array indicates whether the chunk has already been
% processed.

basic_blocks_to_flat_code(BasicBlockArray, Reached, JoinedArray, ReachedArray, Code) :-
	(
	    fromto(1,I,NextI,0),			% current chunk
	    fromto(1,PrevCont,Cont,_),			% prev. chunk's continuation
	    fromto(Reached,Reached1,Reached2,_),	% branches (queue)
	    fromto(Code,Code0,Code3,[]),		% result list
	    param(BasicBlockArray, JoinedArray, ReachedArray)
	do
	    arg(I, BasicBlockArray, Chunk),
	    Chunk = chunk{code:ChunkCode,done:Done,cont:Cont0},
	    ( var(Done) ->
		% process chunk: append code to final code list
		Done = done,
		Cont = Cont0,
                Code0 = [code{instr:label(I)}|Code1],
		append(ChunkCode, Code2, Code1)
	    ; PrevCont == I ->
		% previous chunk continues into this one, but it has already
		% been emitted, so we need a branch 
                % can't copy because 
                %  1) chunk may have labels
                %  2) no length info for chunk because of simplification
                Code0 = [code{instr:branch(ref(I))}|Code2],
		Cont = 0
	    ;
		Cont = 0,
		Code0 = Code2
	    ),
	    % Choose the next chunk to process: prefer the current chunk's
	    % continuation, otherwise pick one from the queue
	    ( Cont > 0 ->
               ( should_continue_branch(Cont, I, BasicBlockArray, JoinedArray, ReachedArray) ->
                   Code2 = [code{instr:branch(ref(Cont))}|Code3],   
                   Reached1 = [NextI|Reached2]		% don't use continuation
               ;
                   Code2 = Code3,
                   NextI = Cont, Reached1 = Reached2	% use continuation
               )
	    ; Reached1 == [] ->
                Code2 = Code3,
	    	NextI = 0				% queue empty, finished
	    ;
                Code2 = Code3,
		Reached1 = [NextI|Reached2]		% pick from queue
	    )
	).

/* should_continue_branch(Cont, Current, BasicBlockArray, JoinArraye, ReachedArray)
   determines if the continuation chunk should be appended to the 
   current one, or if a new branch started. The idea is to preserve
   the original branching if possible, to preserve any optimisation
   performed by the compiler. However, if a chunk is already joined
   (e.g. by joing short continuations), then do not try to preserve
   original branching as chunk may be replicated
*/
should_continue_branch(Cont, Current, BasicBlockArray, JoinedArray, ReachedArray) :-
        Cont =\= Current + 1, % Continuation is not next chunk
        BeforeCont is Cont - 1, 
        BeforeCont \== 0,
        arg(BeforeCont, BasicBlockArray, BeforeChunk),
        BeforeChunk = chunk{done:Done,cont:Cont}, 
        % BeforeChunk continues into Continue (i.e. original branching)
        var(Done),
        arg(BeforeCont, JoinedArray, BeforeJoined),
        var(BeforeJoined),    % BeforeChunk was not joined early
        arg(BeforeCont, ReachedArray, BeforeReached),
        nonvar(BeforeReached). % check that BeforeChunk is not dead code 

%----------------------------------------------------------------------
% simplify a basic block
%----------------------------------------------------------------------

% simplify_chunk leaves the annotations around instructions so that it can be
% run multiple times on a chunk.
% Every time we make a simplification, we back up 2 instructions to the
% left, and try to simplify again. These two instructions are in the two
% (possibly empty) difference lists Rescan1 and Rescan2.

simplify_chunk(Code, SimplifiedCode) :-
	simplify_chunk(Empty1, Empty1, Empty2, Empty2, Code, SimplifiedCode).

:- mode simplify_chunk(?,?,?,?,+,-).
simplify_chunk(Rescan1, Rescan2, Rescan2, [], [], Rescan1).
simplify_chunk(Rescan1, RescanT1, Rescan2, RescanT2, [AnnInstr|More], AllSimplified) :-
        AnnInstr = code{instr:Instr},
        ( simplify(Instr, AnnInstr, More, Simplified, MoreTail, SimplifiedTail) ->
%	    log(Instr, More, Simplified),
	    % We transformed Instr+More -> Simplified
	    % Now simplify Rescan+Simplified+Moretail
	    RescanT1 = Rescan2, RescanT2 = Simplified, SimplifiedTail = MoreTail,
	    simplify_chunk(Empty1, Empty1, Empty2, Empty2, Rescan1, AllSimplified)
	;
	    % Instr which couldn't be simplified goes into rescan2,
	    % and the old rescan1 goes into the final code.
	    AllSimplified = Rescan1,
	    simplify_chunk(Rescan2, RescanT2, [AnnInstr|Tail], Tail, More, RescanT1)

	    % Only 1 instruction back:
%	    AllSimplified = Rescan2,
%	    simplify_chunk(Rescan1, RescanT1, [AnnInstr|Tail], Tail, More, RescanT2)
	).


log(Instr, More, Simplified) :-
	code_instr(More, Next),
	code_instr(Simplified, Simp),
	writeln(Instr+Next->Simp).

code_instr(X, []) :- var(X), !.
code_instr([], []) :- !.
code_instr([code{instr:Instr}|_], Instr).


is_nop(nop) ?- true.
is_nop(move(X,X)) ?- true.
is_nop(gc_test(0)) ?- true.
is_nop(initialize([])) ?- true.


% simplify(+Instr, +Code, +Follow, -New, -FollowTail, -NewTail)
% New is where the simplified annotated instruction goes, with an 
% uninstantiated NewTail FollowTail is the tail of the existing following 
% instruction, with the head being the next instruction to simplified.
% Code is the annotated version of Instr. Instr is extracted to allow
% for indexing. This must fail if no simplification is done!

simplify(nop, _, More, New, MoreT, NewT) ?- !, 
        NewT = New,
        MoreT = More.

simplify(gc_test(N), _, More, New, MoreT, NewT) ?-
	( N==0 ->
	    true
	;
	    N =< #wam_max_global_push,
	    % The following test is necessary to retain small gc_tests in
	    % the (rare) case of initialisation code at the end of branches!
	    More = [code{instr:Instr}|_], Instr \= put_global_variable(y(_))
	),
	!, 
        NewT = New,
        MoreT = More.

simplify(move(X,X), _, More, New, MoreT, NewT) ?- !, 
        NewT = New,
        MoreT = More.

simplify(initialize(y([])), _, More, New, MoreT, NewT) ?- !, 
        NewT = New,
        MoreT = More.

simplify(deallocate, _Code, [code{instr:ret}|More], New, MoreT, NewT) ?- !,
        MoreT = More,
        New = [code{instr:exit}|NewT].

simplify(jmp(_), Code, [code{instr:ret}|More], New, MoreT, NewT) ?- !,
        MoreT = More,
        New = [Code|NewT].

simplify(chain(_), Code, [code{instr:ret}|More], New, MoreT, NewT) ?- !,
        MoreT = More,
        New = [Code|NewT].

simplify(move_chain(_,_,_), Code, [code{instr:ret}|More], New, MoreT, NewT) ?- !,
        MoreT = More,
        New = [Code|NewT].

simplify(callf(P,eam(0)), Code, [code{instr:Instr}|More], New, MoreT, NewT) ?- !,
        MoreT = More,
        New = [NewCode|NewT],
        % body goals order rearranged here to avoid old compiler bug
        update_struct(code, instr:NewInstr, Code, NewCode),
	simplify_call(P, Instr, NewInstr).

simplify(move_callf(Y,A,P,eam(0)), Code, [code{instr:exit}|More], New, MoreT, NewT) ?- !,
        MoreT = More,
        New = [NewCode|NewT],
	true,
        update_struct(code, instr:move_chain(Y,A,P), Code, NewCode).

simplify(call(P,eam(0)), Code, [code{instr:Instr}|More], New, MoreT, NewT) ?- !,
        MoreT = More,
	New = [NewCode|NewT],
        % body goals order rearranged here to avoid old compiler bug
        update_struct(code, instr:NewInstr, Code, NewCode),
	simplify_call(P, Instr, NewInstr).

/*simplify(cut(y(1),_N), [exit|More], New) ?- !,
        New = [exitc|More].
*/
simplify(savecut(AY), Code, [code{instr:cut(AY)}|More], New, MoreT, NewT) ?- !,
        % remove cut(..) and allow savecut(..) to be examined again for further simplifications
        MoreT = [Code|More],
        New = NewT.

simplify(savecut(AY), Code, [code{instr:cut(AY,_)}|More], New, MoreT, NewT) ?- !,
        % remove cut(..) and allow savecut(..) to be examined again for further simplifications
        MoreT = [Code|More],
        New = NewT.

simplify(savecut(_), _, More, New, MoreT, NewT) ?- !,
        More = [code{instr:Instr}|_],
        New = NewT,
        More = MoreT,
        unconditional_transfer_out(Instr).

simplify(cut(A), Code, [code{instr:cut(A)}|More], New, MoreT, NewT) ?- !,
        New = [Code|NewT],
        More = MoreT.

simplify(cut(AY,E), Code, [code{instr:cut(AY,E)}|More], New, MoreT, NewT) ?- !,
        New = [Code|NewT],
        More = MoreT.

/*simplify(push_structure(B), [write_did(F/A)|More], New) ?- !,
        B is A + 1,
        New = [write_structure(F/A)|More].
*/
simplify(allocate(N), _, [code{instr:move(a(I),y(J)),regs:Regs}|More], New, MoreT, NewT) ?- !,
        More = MoreT,
        New = [code{instr:get_variable(N, a(I), y(J)),regs:Regs}|NewT].

simplify(allocate(N), _, [code{instr:chain(P)}|Rest], New, RestT, NewT) ?- !,
	verify N==0,
        New = [code{instr:jmp(P)}|NewT],
        RestT = Rest.

simplify(space(N), _, [code{instr:branch(L)}|More], New, MoreT, NewT) ?- !,
        More = MoreT,
        New = [code{instr:branchs(N,L)}|NewT].

simplify(space(N), _, [code{instr:exit}|More], New, MoreT, NewT) ?- !,
        More = MoreT,
        New = [code{instr:exits(N)}|NewT].
/*        
simplify(space(N), _, [code{instr:jmpd(L)}|More], New, MoreT, NewT) ?- !,
        More = MoreT,
        New = [code{instr:jmpd(N,L)}|NewT].
*/
	% the code generator compiles attribute unification as if it were
	% unifying a meta/N structure. Since attribute_name->slot mapping
	% can change between sessions, we transform sequences like
	%	read_attribute suspend		(where suspend->1)
	%	read_void*			(N times)
	%	read_xxx			(match actual attribute)
	% into
	%	read_attribute name		(where name->N)
	%	read_xxx			(match actual attribute)
	% to make the code session-independent. Note that this cannot cope
	% with multiple attributes being matched at once. This restriction
	% also exists in the old compiler; lifting it requires a different
	% compilation scheme with probably new instructions.
simplify(read_attribute(FirstName), _, More0, New, MoreT, NewT) ?-
	meta_index(FirstName, I0),
	count_same_instr(More0, read_void, I0, I, MoreT),
	I > I0,
	!,
	( meta_index(Name, I) ->
	    New = [code{instr:read_attribute(Name)}|NewT]
	;
	    % as many or more read_voids than attributes
	    New = NewT
	).

simplify(read_void, _, [code{instr:read_void}|Rest0], New, RestT, NewT) ?- !,
	count_same_instr(Rest0, read_void, 2, N, RestT),
	New = [code{instr:read_void(N)}|NewT].

simplify(write_void, _, [code{instr:write_void}|Rest0], New, RestT, NewT) ?- !,
        count_same_instr(Rest0, write_void, 2, N, RestT),
        New = [code{instr:write_void(N)}|NewT]. 

simplify(push_void, _, [code{instr:push_void}|Rest0], New, RestT, NewT) ?- !,
        count_same_instr(Rest0, push_void, 2, N, RestT),
        New = [code{instr:push_void(N)}|NewT]. 

simplify(move(y(Y1),a(A1)), _, [AnnInstr0|Rest0], New, RestT, NewT) ?- 
        AnnInstr0 = code{instr:move(y(Y2),a(A2))}, !,
        ( A2 =:= A1 + 1, Y2 =:= Y1 + 1 ->
            % the arguments for the moves are consecutive
            extract_conargs_moves(Rest0, move(y(Y),a(A)), Y, A, Y1, A1, 2, N, RestT),
            New = [code{instr:move(N,y(Y1),a(A1))}|NewT]
        ;
            MoveInstrs = [move(y(Y1),a(A1))|MoveInstrs0],
            extract_nonconargs_moves(Rest0, move(y(_),a(_)), AnnInstr0, Y2, A2, MoveInstrs0, RestT),
            MoveInstrs0 \= [], % no compact possible with single move.
            compact_moves(MoveInstrs, New, NewT)
        ).

simplify(move(a(A1),y(Y1)), _, [AnnInstr0|Rest0], New, RestT, NewT) ?- 
        AnnInstr0 = code{instr:move(a(A2),y(Y2))}, !,
        ( A2 =:= A1 + 1, Y2 =:= Y1 + 1 ->
            % the arguments for the moves are consecutive
            extract_conargs_moves(Rest0, move(a(A),y(Y)), A, Y, A1, Y1, 2, N, RestT),
            New = [code{instr:move(N,a(A1),y(Y1))}|NewT]
        ;
            MoveInstrs = [move(a(A1),y(Y1))|MoveInstrs0],
            extract_nonconargs_moves(Rest0, move(a(_),y(_)), AnnInstr0, A2, Y2, MoveInstrs0, RestT),
            MoveInstrs0 \= [], % no compact possible with single move
            compact_moves(MoveInstrs, New, NewT)
        ).

simplify(move(y(Y),a(A)), _, [code{instr:callf(P,EAM)}|Rest0], New, RestT, NewT) ?- !,
        New = [code{instr:move_callf(y(Y),a(A),P,EAM)}|NewT],
        RestT = Rest0.

simplify(move(y(Y),a(A)), _, [code{instr:chain(P)}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:move_chain(y(Y),a(A),P)}|NewT],
        RestT = Rest.

simplify(put_global_variable(a(A),y(Y)), _, [code{instr:callf(P,EAM)}|Rest0], New, RestT, NewT) ?- !,
        New = [code{instr:put_global_variable_callf(a(A),y(Y),P,EAM)}|NewT],
        RestT = Rest0.

simplify(move(a(A1),a(A2)), Code, Rest, New, RestT, NewT) ?- !,
        Code = code{regs:Regs},
        extract_moveaas(Rest, Moves, RegInfos, RestT), 
	Moves \= [],
        simplify_moveaas([A1>A2|Moves], [Regs|RegInfos], New, NewT).

simplify(move(y(Y1),y(Y2)), _, [code{instr:move(y(Y3),y(Y4))}|Rest], New, RestT, NewT) ?- !,
        ( Rest = [code{instr:move(y(Y5),y(Y6))}|Rest0] ->
            New = [code{instr:move(y(Y1),y(Y2),y(Y3),y(Y4),y(Y5),y(Y6))}|NewT],
            Rest0 = RestT
        ;
            New = [code{instr:move(y(Y1),y(Y2),y(Y3),y(Y4))}|NewT],
            Rest = RestT
        ).

simplify(read_variable(a(A1)), _, [code{instr:read_variable(a(A2))}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:read_variable2(a(A1),a(A2))}|NewT],
        RestT = Rest.

simplify(read_variable(a(A1)), _, [code{instr:read_variable(y(Y2))}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:read_variable2(a(A1),y(Y2))}|NewT],
        RestT = Rest.

simplify(write_variable(a(A1)), _, [code{instr:write_variable(a(A2))}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_variable2(a(A1),a(A2))}|NewT],
        RestT = Rest.

simplify(push_variable(a(A1)), _, [code{instr:push_variable(a(A2))}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_variable2(a(A1),a(A2))}|NewT],
        RestT = Rest.

simplify(write_variable(a(A1)), _, [code{instr:write_variable(y(Y2))}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_variable2(a(A1),y(Y2))}|NewT],
        RestT = Rest.

simplify(read_variable(y(Y1)), _, [code{instr:read_variable(y(Y2))}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:read_variable2(y(Y1),y(Y2))}|NewT],
        RestT = Rest.

simplify(write_variable(y(Y1)), _, [code{instr:write_variable(y(Y2))}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_variable2(y(Y1),y(Y2))}|NewT],
        RestT = Rest.

simplify(push_variable(y(Y1)), _, [code{instr:push_variable(y(Y2))}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_variable2(y(Y1),y(Y2))}|NewT],
        RestT = Rest.

simplify(write_local_value(a(A1)), _, [code{instr:write_local_value(a(A2))}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_local_value2(a(A1),a(A2))}|NewT],
        RestT = Rest.

simplify(write_local_value(y(Y1)), _, [code{instr:write_local_value(y(Y2))}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_local_value2(y(Y1),y(Y2))}|NewT],
        RestT = Rest.

simplify(push_local_value(a(A1)), _, [code{instr:push_local_value(a(A2))}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:push_local_value2(a(A1),a(A2))}|NewT],
        RestT = Rest.

simplify(push_local_value(y(Y1)), _, [code{instr:push_local_value(y(Y2))}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:push_local_value2(y(Y1),y(Y2))}|NewT],
        RestT = Rest.

simplify(put_global_variable(a(A1),y(Y1)), _, [code{instr:put_global_variable(a(A2),y(Y2))}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:put_global_variable2(a(A1),y(Y1),a(A2),y(Y2))}|NewT],
        RestT = Rest.

simplify(put_variable(a(A1)), _, [code{instr:put_variable(a(A2))}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:put_variable2(a(A1),a(A2))}|NewT],
        RestT = Rest.

simplify(write_integer(C1), _, [code{instr:write_integer(C2)}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_integer2(C1,C2)}|NewT],
        RestT = Rest.

/* push_integer = write_integer in emu.c */
simplify(push_integer(C1), _, [code{instr:push_integer(C2)}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_integer2(C1,C2)}|NewT],
        RestT = Rest.

simplify(write_atom(C1), _, [code{instr:write_atom(C2)}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_atom2(C1,C2)}|NewT],
        RestT = Rest.

simplify(write_atom(C1), _, [code{instr:write_did(C2)}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_atomdid(C1,C2)}|NewT],
        RestT = Rest.

simplify(write_did(C1), _, [code{instr:write_did(C2)}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_did2(C1,C2)}|NewT],
        RestT = Rest.

simplify(write_did(C1), _, [code{instr:write_atom(C2)}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_didatom(C1,C2)}|NewT],
        RestT = Rest.

simplify(write_atom(C1), _, [code{instr:write_integer(C2)}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_atominteger(C1,C2)}|NewT],
        RestT = Rest.

simplify(write_did(C1), _, [code{instr:write_integer(C2)}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_didinteger(C1,C2)}|NewT],
        RestT = Rest.

% broken instruction
%simplify(read_atom(C1), _, [code{instr:read_integer(C2)}|Rest], New, RestT, NewT) ?- !,
%        New = [code{instr:read_atominteger(C1,C2)}|NewT],
%        RestT = Rest.

simplify(write_integer(C1), _, [code{instr:write_atom(C2)}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_integeratom(C1,C2)}|NewT],
        RestT = Rest.

simplify(write_integer(C1), _, [code{instr:write_did(C2)}|Rest], New, RestT, NewT) ?- !,
        New = [code{instr:write_integerdid(C1,C2)}|NewT],
        RestT = Rest.

% broken instruction
%simplify(read_integer(C1), _, [code{instr:read_atom(C2)}|Rest], New, RestT, NewT) ?- !,
%        New = [code{instr:read_integeratom(C1,C2)}|NewT],
%        RestT = Rest.


/* extract consecutive move a(_) a(_) insturctions for further optimisation 
   MoveRegs is a list of the arg register pairs for each move instruction,
   and RegInfos is a list of the corresponding regs field for the instruction 
*/ 
:- mode extract_moveaas(+, -, -, -).
extract_moveaas([code{instr:move(a(A1),a(A2)),regs:RegI}|Rest], MoveRegs, RegInfos, RestT) ?- !,
        MoveRegs = [A1>A2|MoveRegs1], % use '>' to suggest move direction
        RegInfos = [RegI|RegInfos1],
        extract_moveaas(Rest, MoveRegs1, RegInfos1, RestT).
extract_moveaas(Rest, [], [], Rest).

/* simplify the move a(_) a(_) sequence by
   1. extracting sequences of move a(_) a(_) which are shifting the value between a
      chain of registers (e.g A1<-A2...<-An). The move instr may be non-consecutive, 
      effectively rearranging the order of the moves: move a(Source) a(Dest) can be
      done earlier as long as the intervening moves does not:
         a) overwrite Source (i.e. Source register is not a destination for intervening moves)
         b) use the contents of Dest (i.e. Dest register is not a source for intervening moves)
      A chain is represented as a list [a(A1),a(A2)...] which represents the chain A1<-A2...
   2. convert these chains to the following instructions, in order of preference:
         a) rotate type instruction  A1<-A2...<-A1
         b) shift type instruction A1<-A2 ...<-An
         c) multiple move instruction (non-chained moves) 
*/
simplify_moveaas(Regs, RegInfos, New, NewT) :-
        extract_reg_chains(Regs, RegInfos, Chains, [], ChainInfos, []),
        convert_chains_to_instrs(Chains, ChainInfos, 0, [], New, NewT).

extract_reg_chains([], [], Chains, ChainsT, ChainInfos, ChainInfosT) ?- 
        Chains = ChainsT,
        ChainInfos = ChainInfosT.
extract_reg_chains([A1>A2|Regs0], [[A1Info,A2Info]|RegInfos0], Chains, ChainsT, ChainInfos, ChainInfosT) :-
        ( find_reg_chain(A1, A1Info, Regs0, RegInfos0, [], [], Regs1, RegInfos1, Chained1, CInfo1),
          Chained1 \= [_] % no chain found if there is only  one element 
        ->
        
            Chains =[[a(A2)|Chained1]|Chains1],
            ChainInfos = [[A2Info|CInfo1]|ChainInfos1]
            
        ;
            Chains = [[a(A1),a(A2)]|Chains1],
            ChainInfos = [[A1Info,A2Info]|ChainInfos1],
            RegInfos0 = RegInfos1,
            Regs0 = Regs1
        ),
        % try find more chains in remaining move instructions
        extract_reg_chains(Regs1, RegInfos1, Chains1, ChainsT, ChainInfos1, ChainInfosT).

find_reg_chain(S, SInfo, [], [], _UnmovedSs, _UnmovedDs, RegsOut, RInfosOut, Chained, CInfo) ?- !,
        RegsOut = [],
        RInfosOut = [],
        Chained = [a(S)],
        CInfo = [SInfo].
find_reg_chain(S0, S0Info, [RegPair|Regs1], [RPairInfo|RInfos1], UnmovedSs0, UnmovedDs0, 
               RegsOut, RInfosOut, Chained, CInfo) :-
        RegPair = (S1>D1),
        RPairInfo = [S1Info,D1Info],
        (  D1 == S0  ->
            % S1>D1 match for chain, can it be added to chain?
            ( nonmember(D1, UnmovedSs0), % not a source in intervening moves
              nonmember(S1, UnmovedDs0),  % not a destination in intervening moves
              check_source_reg_info(S1, S1Info, UnmovedSs0)
            ->
                % add to chain
                Chained = [a(D1)|Chained1],
                CInfo = [D1Info|CInfo1],
                find_reg_chain(S1, S1Info, Regs1, RInfos1, UnmovedSs0,
                               UnmovedDs0, RegsOut, RInfosOut, Chained1, CInfo1)
            ;
                % intervening moves prevent S1>D1 to be part of the chain,
                % stop now
                RegsOut = [],
                RInfosOut = [],
                Chained = [a(S0)],
                CInfo = [S0Info]
                
            )
        ;
            % S1>D1 does not match for current chain. Try matching with
            % subsequent moves
            RegsOut = [RegPair|RegsOut1],
            RInfosOut = [RPairInfo|RInfosOut1],
            find_reg_chain(S0, S0Info, Regs1, RInfos1, [S1|UnmovedSs0], [D1|UnmovedDs0], 
                           RegsOut1, RInfosOut1, Chained, CInfo)
        ).
        
    % make sure that the source register information is still correct if
    % the move instruction is added to a chain: if it is the last use of
    % the register, it should not be moved before an earlier use of the
    % register as a source. [alternative: update the reg info instead] 
    check_source_reg_info(S, SInfo, UnmovedSs) :-
        ( SInfo = r(_,_,_,IsLast), IsLast == last ->
            nonmember(S, UnmovedSs) 
         ; 
            true
        ).
       


convert_chains_to_instrs([], [], NMoves, MoveRegs, New, NewT) ?- 
        (NMoves > 0 ->
            combine_moves(MoveRegs, Instr),
            New = [code{instr:Instr}|NewT]
        ;
            New = NewT
        ).
convert_chains_to_instrs([Chain|Rest], [CInfo|ChainsInfo], NMoves, MoveRegs, New, NewT) ?-
        length(Chain, L),
        ( L == 2 ->
            /* a move instr */
            ( NMoves =:= maxmoveaas -> /* maxmoveaas must be > 0! */
                combine_moves(MoveRegs, MoveInstr),
                New = [code{instr:MoveInstr}|New1],
                NMoves1 = 1,
                MoveRegs1 = Chain
            ;
                New1 = New,
                NMoves1 is NMoves + 1,
                append(MoveRegs, Chain, MoveRegs1)
            ),
            convert_chains_to_instrs(Rest, ChainsInfo, NMoves1, MoveRegs1, New1, NewT)
        
        ;   /* a shift instruction */
            ( NMoves > 0 ->
                /* generate previously accumulated move instr */
                combine_moves(MoveRegs, MoveInstr),
                New = [code{instr:MoveInstr}|New1]
            ;
                New1 = New
            ),
            ( L =< maxshift -> /* maxshift must be > 2 */
                ( L == 4, 
                  Chain = [T,A1,A2,T],
                  CInfo = [_,_,_,TInfo],
                  TInfo = r(_,_,_,IsLast),
                  IsLast == last
                ->
                   New1 = [code{instr:swap(A1,A2)}|New2]
                ; L == 5, 
                  Chain = [T,A1,A2,A3,T],
                  CInfo = [_,_,_,_,TInfo],
                  TInfo = r(_,_,_,IsLast),
                  IsLast == last
                ->
                   New1 = [code{instr:rot(A1,A2,A3)}|New2]
                ;
                   ShiftInstr =.. [shift|Chain],
                   New1 = [code{instr:ShiftInstr}|New2]
                )

            ;
                split_shift_instrs(Chain, L, New1, New2)
            ),
            convert_chains_to_instrs(Rest, ChainsInfo, 0, [], New2, NewT)
        ).

combine_moves(MoveRegs, Instr) :-
        Instr =.. [move|MoveRegs].

split_shift_instrs(Chain, L, New, NewT) :-
        maxshift(Max),
        split_chain(L, Max, Chain, New, NewT).

split_chain(Len, Max, Chain, New, NewT) :-
        ( Len == 2 ->
            /* 2 arguments - move instr, argument order reversed */
            Chain = [A1,A2],
            New = [code{instr:move(A2,A1)}|NewT]
        ; Len =< Max ->
            Instr =.. [shift|Chain],
            New = [code{instr:Instr}|NewT]
        ; 
            get_subchain(Max, Chain, SubChain, RestChain),
            Instr =.. [shift|SubChain],
            New = [code{instr:Instr}|New1],
            Len1 is Len - Max + 1,
            split_chain(Len1, Max, RestChain, New1, NewT)
        ).

get_subchain(1, List0, SubT, RestList) :- !, 
        List0 = RestList,
        List0 = [E|_],
        SubT = [E].
get_subchain(N, [E|List0], SubT, RestList) :-
        SubT = [E|SubT0],
        N0 is N - 1,
        get_subchain(N0, List0, SubT0, RestList).

maxmoveaas(3).  /* maximum number of non-related move a(_) a(_) than can be combined */
maxshift(5).    /* maximum number of arguments in a shift instruction */

% extract a sequence of move instructions of the same type whose argument
% refers to consecutive registers. The number of such move instructions, N,
% is returned
extract_conargs_moves(Codes, Instr, X, Y, X0, Y0, N0, N, Rest) :-
        ( \+ \+ (Codes = [code{instr:Instr}|_], X0 + N0 =:= X, Y0 + N0 =:= Y) 
        ->
            Codes = [_|Codes1],
            N1 is N0+1,
            extract_conargs_moves(Codes1, Instr, X, Y, X0, Y0, N1, N, Rest)
        ;
            N = N0, 
            Rest = Codes
        ).

% extract a sequence of move instructions of type Template whose arguments
% are not consectuve, starting with AnnInstr0. Can return an empty sequence
extract_nonconargs_moves(Codes0, Template, AnnInstr0, X0, Y0, MoveInstrs1, Codes) :-
        AnnInstr0 = code{instr:Instr0},
        ( Codes0 = [AnnInstr1|Codes1],
          AnnInstr1 = code{instr:Instr1},
          \+ \+ Instr1 = Template ->
            arg([1,1], Instr1, X1),
            arg([2,1], Instr1, Y1),
            ( X1 =:= X0 + 1,
              Y1 =:= Y0 + 1 ->
                MoveInstrs1 = [],
                Codes = [AnnInstr0|Codes0]
            ;
                MoveInstrs1 = [Instr0|MoveInstrs2], 
                extract_nonconargs_moves(Codes1, Template, AnnInstr1, X1, Y1,
                                         MoveInstrs2, Codes)
            )
        ;
            MoveInstrs1 = [Instr0],
            Codes = Codes0
        ).

:- mode compact_moves(+,-,-).
compact_moves([], Tail, Tail).
compact_moves([Instr1,Instr2,Instr3|Rest], [code{instr:move(X1,Y1,X2,Y2,X3,Y3)}|CRest1],
              CRest) :-
        !,
        Instr1 =.. [_,X1,Y1],
        Instr2 =.. [_,X2,Y2],
        Instr3 =.. [_,X3,Y3],
        compact_moves(Rest, CRest1, CRest).
compact_moves([Instr1,Instr2], [code{instr:move(X1,Y1,X2,Y2)}|CRest], CRest) :-
        !,
        Instr1 =.. [_,X1,Y1],
        Instr2 =.. [_,X2,Y2].
compact_moves([Instr], [code{instr:Instr}|CRest], CRest).



count_same_instr(Codes, Instr, N0, N, Rest) :-
    	( Codes = [code{instr:Instr}|Codes1] ->
	    N1 is N0+1,
	    count_same_instr(Codes1, Instr, N1, N, Rest) 
	;
	    Rest = Codes, N = N0
	).

:- mode simplify_call(+,+,-).
simplify_call(P, ret, jmp(P)).
simplify_call(P, exit, chain(P)).


%----------------------------------------------------------------------
end_of_file.
%----------------------------------------------------------------------




Requirements
------------

Process and simplify a WAM code list.  The main problems are:

    - how to substitute patterns that are not consecutive,
	i.e. contain jumps

    - how to make sure that all new substitutions opportunities arising
    	from performed substitutions are found

    - how to detect unreachable labels

It might be useful to transform the code sequence into a graph and work on
that. Read up on some implementation techniques.



Sample substitution patterns:
-----------------------------

Pattern 1:	(eliminate instr)

	nop

    -> replace with nothing


Pattern 1a:

    	move X X

    -> replace with nothing

Pattern 1b:

    	branch lab
    otherlab:
    lab:
	...

    ->
    otherlab:
    lab:
	...


Pattern 2:	(merge instr sequence)

	move(B,A)
	move(C,B)
    ->
    	shift(A,B,C)

	move(Yi,Aj)
	move(Yk,Al)
    ->
    	move(Yi,Aj,Yk,Al)

Pattern 2a:

	call	P N
	ret
    ->
    	jmp 	P


Pattern 3:	(merge broken instr sequence)

	call	P N
	branch	l1
	...
    l1:
	ret
    ->
	jmp	P
	...
    l1:			(might now be unreachable)
	ret


Pattern 4:	(eliminate unreachable code)

	...a...
	branch/jmp
    l1:			(not a jump target)
	...b...
    l2:
	...c...
    ->
	...a...
	branch/jmp
    l2:
	...c...


Pattern 5:	(skip subsumed instruction)

	Atom_switch A1 [a->alab, b->blab]

	...
    alab:
	Get_atom A1 a
	...
    blab:
	Get_atom A1 b
	...

    -> change the Atom_switch to jump beyond the Get_atom instruction directly.


Pattern 5a:	(skip subsumed instruction)

	List_switch A1 llab ...
	...
    llab:
	Get_list A1 rlab
	...
    rlab:
	Read_xxx

    -> Here the List_switch should be changed to jump directly to rlab.

Pattern 5a:    (skip subsumed instruction)

	get_variable n An Ym
	switch_on_type Ym meta:mlab 

     mlab:
        move Ym An
        ...

     -> change the meta:mlab to meta:lab where lab is after move Ym An

        get_variable n An Ym
	list_switch Ym ref(llab) ref(nlab) ...

     nlab:
        move Ym An
        in_get_nil An
        ...

     -> change to:

        get_variable n An Ym
	list_switch An ref(lab) ref(nlab) ...

     nlab:
        move Ym An
        in_get_nil An
     lab:
        ...

Pattern 5a:	(redirect to shared code)

	List_switch A1 llab ...
	...
    llab:
	Failure

    -> Here the List_switch should be changed to jump directly to the
    	global fail label.





Remove Res instruction when an event-triggering instruction follows
before failure can occur (but probably better done earlier):

    Res,...,Call
    Res,...,Metacall
    Res,...,Jmp
    Res,...,Chain
    Res,...,Ret
    Res,...,Exit


Various Patterns:


    savecut(a(A)),cut(a(A))	-->	savecut(a(A))
    savecut(..), <transfer out> -->     <transfer out> unsafe for calls
                                        
    read_void,read_void+	-->	read_void N

    write_void,write_void+	-->	write_void N
                                        
    allocate n, move Ai,Yj      -->     get_variable(n,Ai,Yj)
                                        
    space n, branch L           -->     branchs n,L
    space n, jmpd L             -->     jmpd n, L

Patterns that are not safe to optimise:
                                      
    push_structure(N+1),write_did(F/N)  --> write_structure(F/N)
    because the push_structure and write_did may refer to different structs                                        
    cut(y(1),N), exit		-->	exitc 
    because cut(...) may be local cut (not the whole cluase)                                  
