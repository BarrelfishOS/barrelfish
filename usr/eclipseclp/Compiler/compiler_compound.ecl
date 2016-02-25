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
% Contributor(s): Joachim Schimpf.
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Component:	ECLiPSe III compiler
% Version:	$Id: compiler_compound.ecl,v 1.13 2015/01/14 01:31:08 jschimpf Exp $
%
% This code is based on the paper
%
%	Micha Meier, "Compilation of Compound Terms in Prolog"
%
% Head unification:
%    The basic idea is to unify nested compound terms top-down and
%    left-to-right. Unlike the WAM scheme, this method does not require
%    temporaries to hold structure arguments, but needs a stack instead.
%    However, since the depth of the nested term in the head is known
%    at compile time, this stack can be built from temporaries (every
%    nesting level is assigned one temporary, except the bottom level).
%    These temporaries contain a read/write mode flag and a copy of the
%    S register, indicating how and where to continue after having
%    finished the unification of a compound subterm.
%    This method is better than the WAM scheme especially for wide,
%    flat structures and for right-balanced structures like lists.
%
%    Read and write-mode are in separate code sequences, and there
%    are conditional jumps back and forth between the sequences.
%    If a read-instruction discovers a variable in the input, it
%    creates a structure frame and jumps into the write-sequence to
%    construct the structure arguments. The 'return address' in form
%    of a read-flag and the next value of S is saved in a temporary.
%    At the end of a write sequence for all arguments of a subterm,
%    the temporary is tested and possibly control is transferred back
%    to the read mode. This is all further complicated by a 'last-call'
%    optimization, ie. dropping the temporary before the last subterm.
%
%    Compared to the presentation in Micha's paper, in the actual
%    implementation instructions are merged and specialised:
%
%    Write mode:				Read mode:
%
% (part	of Write_first_struct/list)          (part of Read_structure WLabel)
%		allocate Ti			allocate Ti
%		down (save S+1|WRITE)		down (save S+1|READ)
%						possibly goto write mode
%
% (part of Write_next_struct/list Ti RLabel) (part of Read_next_struct Ti WLabel)
%		possibly goto read mode		up (restore S)
%		up (restore S)			down (save S+1)
%		down (save S+1)			possibly goto write mode
%				
%	Mode Ti RLabel			     Mode Ti
%		up (restore S)			up (restore S)
%		possibly goto read mode
%
% (part of Write_next_struct/list Ti)	     (part of Read_structure Ti WLabel)
%		down (save S+1)			down (save S+1)
%						possibly goto write mode
%
% Support for head matching (matching clauses)
%    This is partly done with special code, and partly by reusing the
%    unify-code and throwing away the write-mode code. The generic code
%    uses a Dir flag (inout/in) to distinguish unification/matching mode.
%
% Support for attributed variable matching
%    This is similar to matching a variable followed by a structure.
%    TODO: The current scheme is not suitable for creating .eco files
%    because the generated code will only work when the same meta_attribute
%    declarations are in place at compile time and at eco-load time.
%
% Possible improvements
%    It would be advantageous to reorder subterm unification rather than
%    going left-to-right. E.g. in order to exploit the last-subterm
%    optimization better one should do the shallow subterms first, then
%    the more deeply nested ones. This shouldn't be difficult, but it
%    requires all instructions that save S+1 to save S+-offset instead,
%    so they all need an extra offset parameter.
%
% CAUTION:
%    The Ti used here are still allocated on the local stack (identical
%    to ECLiPSe I).
%----------------------------------------------------------------------


:- import
	bignum/1,
	meta_index/2
    from sepia_kernel.

%:- export head/6.

% generate head unification for VarId I and Term
head(I, Term, ChunkData0, ChunkData, Code, Code0) :-
	head1(I, Term, ChunkData0, ChunkData, Code1, Code0),
	env_allocate_delta(ChunkData0, ChunkData, Code, Code1).

head1(I, Term, ChunkData0, ChunkData, Code, Code0) :-
	Term = [_|_],
	Code = [code{instr:get_list(RI, ref(LR)),regs:[r(I,RI,use_a,_),r(_,_,split(State),_)]}|WCode],
	alloc_term(Term, ChunkData0, ChunkData1),
	unify_args(Term, ChunkData1, ChunkData, 0, Reg, WCode, WCode0, RCode, RCode0, inout),
	WCode0 = [code{instr:branch(ref(LE))},code{instr:label(LR),regs:[r(_,_,restore(State),_)]}|RCode],
	RCode0 = [code{instr:label(LE),regs:[r(_,_,join(State),_)]}|Code1],
	emit_pop_temp(Reg, Code1, Code0).
head1(I, Term, ChunkData0, ChunkData, Code, Code0) :-
	Term = structure{name:F,arity:A},
	Code = [code{instr:get_structure(RI, F/A, ref(LR)),regs:[r(I,RI,use_a,_),r(_,_,split(State),_)]}|WCode],
	alloc_term(Term, ChunkData0, ChunkData1),
	unify_args(Term, ChunkData1, ChunkData, 0, Reg, WCode, WCode0, RCode, RCode0, inout),
	WCode0 = [code{instr:branch(ref(LE))},code{instr:label(LR),regs:[r(_,_,restore(State),_)]}|RCode],
	RCode0 = [code{instr:label(LE),regs:[r(_,_,join(State),_)]}|Code1],
	emit_pop_temp(Reg, Code1, Code0).
head1(I, Term, ChunkData, ChunkData, Code, Code0) :-
	atomic(Term),
	Code = [code{instr:Instr,regs:[r(I,RI,use_a,_)]}|Code0],
	get_const(RI, Term, Instr).


% generate head matching for VarId I and Term
in_head(I, Term, ChunkData0, ChunkData, Code, Code0) :-
	in_head1(I, Term, ChunkData0, ChunkData, Code1, Code0),
	env_allocate_delta(ChunkData0, ChunkData, Code, Code1).

in_head1(I, Term, ChunkData0, ChunkData, Code, Code0) :-
	Term = [_|_],
	Code = [code{instr:in_get_list(RI, ref(LR)),regs:[r(I,RI,use_a,_)]},
		code{instr:label(LR)}|RCode],
	unify_args(Term, ChunkData0, ChunkData, 0, Reg, WCode, [], RCode, Code1, in),
	replace_lost_labels(WCode),	% and discard the WCode sequence
	emit_pop_temp(Reg, Code1, Code0).
in_head1(I, Term, ChunkData0, ChunkData, Code, Code0) :-
	Term = structure{name:F,arity:A},
	Code = [code{instr:in_get_structure(RI, F/A, ref(LR)),regs:[r(I,RI,use_a,_)]},
		code{instr:label(LR)}|RCode],
	unify_args(Term, ChunkData0, ChunkData, 0, Reg, WCode, [], RCode, Code1, in),
	replace_lost_labels(WCode),	% and discard the WCode sequence
	emit_pop_temp(Reg, Code1, Code0).
in_head1(I, Term, ChunkData0, ChunkData, Code, Code0) :-
	Term = attrvar{meta:Meta},
	Code = [code{instr:in_get_meta(RI, ref(fail)),regs:[r(I,RI,use_a,_)]},
		code{instr:read_void},
		code{instr:read_attribute(FirstAttr)}
		|RCode],
	meta_index(FirstAttr, 1),
	unify_args(Meta, ChunkData0, ChunkData, 0, Reg, WCode, [], RCode, Code1, in),
	replace_lost_labels(WCode),	% and discard the WCode sequence
	emit_pop_temp(Reg, Code1, Code0).
in_head1(I, Term, ChunkData, ChunkData, Code, Code0) :-
	atomic(Term),
	Code = [code{instr:Instr,regs:[r(I,RI,use_a,_)]}|Code0],
	in_get_const(RI, Term, Instr).

    % generate code to pop any used stack temporaries
    emit_pop_temp(Reg, Code, Code0) :-
	( Reg > 0 ->
	    MReg is -Reg,
	    Code = [code{instr:space(MReg)}|Code0]
	;
	    Code = Code0
	).

    % Unify all the labels in WCode with the fail-label, because we are
    % about to throw away the WCode sequence. This redirects the references
    % from RCode to WCode to fail, effectively turning the
    % read_*_[list|structure] instructions into in_read_*_[list|structure]
    replace_lost_labels(WCode) :-
    	( foreach(Instr,WCode) do
	    ( Instr = code{instr:label(Label)} -> Label=fail ; true )
	).
    	


    % Generate the code for unifying all subterms of Term
    % The last argument is treated specially
    %
    % ?Term	Term whose arguments are to be unified
    % +Reg-	counts the number of temporaries used so far
    % -WCode+	write mode sequence
    % -RCode+	read mode sequence
    % +Dir	'inout' or 'in' (for matching)

unify_args(Compound, ChunkData0, ChunkData, Reg1, Reg5, WCode, WCode5, RCode, RCode6, Dir) :-
	( Compound = structure{args:Args} -> true
	; Compound = [H|T] -> Args = [H,T]
	; verify false
	),
	(
	    fromto(Args, [Arg|Args1], Args1, [ArgN]),
	    fromto(simple, Prev2, Prev3, Prev4),
	    fromto(ChunkData0,ChunkData1,ChunkData2,ChunkData3),
	    fromto(WCode, WCode2, WCode3, WCode4),
	    fromto(RCode, RCode2, RCode3, RCode4),
	    fromto(RV0, RVH2, RVH3, RVH4),	% Difflist of trailing read_var* instructions
	    fromto(RV0, RVT2, RVT3, RVT4),	%   (appended/cleaned up lazily)
	    fromto(Reg1, Reg2, Reg3, Reg4),
	    param(Tmp,Dir)
	do
	    connect_readvar_code(Arg, RCode2, RCode3, RVH2, RVH3, RVT2, RVT3, NewH, NewT),
	    unify_next_arg(Arg, Prev2, Prev3, Tmp, ChunkData1, ChunkData2, Reg2, Reg3, WCode2, WCode3, NewH, NewT, Dir)
	),
	connect_readvar_code(ArgN, RCode4, RCode5, RVH4, RVH5, RVT4, [], LastH, LastT),
	unify_last_arg(ArgN, Prev4, Tmp, ChunkData3, ChunkData, Reg4, Reg5, WCode4, WCode5, LastH, LastT, Dir),
	remove_trailing_read_voids(RVH5, RCode5, RCode6, Empty, Empty).


    % We initially put read_var-style instructions into a separate RVH-RVT list.
    % Gets appended unchanged to RCode when a non-read_var follows.
    % Gets cleaned up (trailing read_void removed) and appened at the very end.
    % This optimization would be fragile with peephole.
    connect_readvar_code(variable{}, RCode2, RCode3, RVH2, RVH3, RVT2, RVT3, NewH, NewT) ?- !,
	RCode2 = RCode3,	% new code not committed yet
	RVH2 = RVH3,		% start of read_var sequence stays the same
	NewH = RVT2,		% new code goes into read_var sequence first
	NewT = RVT3.
    connect_readvar_code(_, RCode2, RCode3, RVH2, RVH3, RVT2, RVT3, NewH, NewT) :-
	RCode2 = RVH2,		% delayed append of read_var sequence
	NewH = RVT2,		% new code will be directly appended
	NewT = RCode3,
	RVH3 = RVT3.		% new empty read_var sequence
    	
    remove_trailing_read_voids([], Out, Out, _RVs, _RVs0).
    remove_trailing_read_voids([H|T], Out, Out0, RVs, RVs0) :-
    	( H = code{instr:read_void} ->
	    RVs0 = [H|RVs1],	% collect read_voids in RVs
	    remove_trailing_read_voids(T, Out, Out0, RVs, RVs1)
	;
	    Out = RVs,		% emit collected read_voids
	    RVs0 = [H|Out1],	% and the following non-read_void
	    remove_trailing_read_voids(T, Out1, Out0, Empty, Empty)
	).



    % Generate the code for a non-last subterm
    %
    % ?SubTerm	subterm to be unified
    % +Prev	type of previous argument ('simple'|'compound')
    % -This	type of this argument ('simple'|'compound')
    % ?Tmp	the temporary for this level (var|integer)
    % +Reg-	counts the number of temporaries used so far
    % -WCode+	write mode sequence
    % -RCode+	read mode sequence
    % +Dir	'inout' or 'in' (for matching)

:- mode unify_next_arg(+,+,-,?, +,-, +,-, -,?, -,?, +).
unify_next_arg(List, Prev, compound, Tmp, ChunkData0, ChunkData, Reg0, Reg2, WCode, WCode0, RCode, RCode0, Dir) :-
	List = [_|_],
	( var(Tmp) ->		% first compound subterm in this level
	    Reg1 is Reg0 + 1,
	    Tmp = Reg1,
	    WCode = [code{instr:write_first_list},code{instr:label(WL)}|WCode1],
	    RCode = [code{instr:read_list(ref(WL))}|RCode1]

	; Prev = compound ->	% immediately following a compound subterm
	    Reg1 = Reg0,
	    Off is Reg0-Tmp,
	    WCode = [code{instr:write_next_list(t(Off),ref(RL))},code{instr:label(WL)}|WCode1],
	    RCode = [code{instr:label(RL)},code{instr:read_next_list(t(Off),ref(WL))}|RCode1]
	; % Prev = simple ->	% following a simple term
	    Reg1 = Reg0,
	    Off is Reg0-Tmp,
	    WCode = [code{instr:write_next_list(t(Off))},code{instr:label(WL)}|WCode1],
            RCode = [code{instr:read_list(t(Off),ref(WL))}|RCode1]
	),
	alloc_term(List, ChunkData0, ChunkData1, Dir),
	unify_args(List, ChunkData1, ChunkData, Reg1, Reg2, WCode1, WCode0, RCode1, RCode0, Dir).
unify_next_arg(Struct, Prev, compound, Tmp, ChunkData0, ChunkData, Reg0, Reg2, WCode, WCode0, RCode, RCode0, Dir) :-
	Struct = structure{name:F,arity:A},
	( var(Tmp) ->		% first compound subterm in this level
	    Reg1 is Reg0 + 1,
	    Tmp = Reg1,
	    WCode = [code{instr:write_first_structure(F/A)},code{instr:label(WL)}|WCode1],
            RCode = [code{instr:read_structure(F/A,ref(WL))}|RCode1]
	; Prev = compound ->	% immediately following a compound subterm
	    Reg1 = Reg0,
	    Off is Reg0-Tmp,
	    WCode = [code{instr:write_next_structure(F/A,t(Off),ref(RL))},code{instr:label(WL)}|WCode1],
	    RCode = [code{instr:label(RL)},code{instr:read_next_structure(F/A,t(Off),ref(WL))}|RCode1]
	; % Prev = simple ->	% following a simple term
	    Reg1 = Reg0,
	    Off is Reg0-Tmp,
	    WCode = [code{instr:write_next_structure(F/A,t(Off))},code{instr:label(WL)}|WCode1],
	    RCode = [code{instr:read_structure(F/A,t(Off),ref(WL))}|RCode1]
	),
	alloc_term(Struct, ChunkData0, ChunkData1, Dir),
	unify_args(Struct, ChunkData1, ChunkData, Reg1, Reg2, WCode1, WCode0, RCode1, RCode0, Dir).
unify_next_arg(Avar, Prev, compound, Tmp, ChunkData0, ChunkData, Reg0, Reg2, WCode, WCode, RCode, RCode0, Dir) :-
	Avar = attrvar{variable:Var,meta:Struct},
	verify Dir == in,
	( var(Tmp) ->		% first compound subterm in this level
	    Reg1 is Reg0 + 1,
	    Tmp = Reg1,
	    RCode = [code{instr:match_meta}|RCode1]
	; Prev = compound ->	% immediately following a compound subterm
	    Reg1 = Reg0,
	    Off is Reg0-Tmp,
	    RCode = [code{instr:match_next_meta(t(Off))}|RCode1]
	; % Prev = simple ->	% following a simple term
	    Reg1 = Reg0,
	    Off is Reg0-Tmp,
	    RCode = [code{instr:match_meta(t(Off))}|RCode1]
	),
	unify_va(Var, ChunkData0, ChunkData1, _, [], RCode1, RCode2, Dir),
	RCode2 = [code{instr:read_attribute(FirstAttr)}|RCode3],
	meta_index(FirstAttr, 1),
	unify_args(Struct, ChunkData1, ChunkData, Reg1, Reg2, WCode1, [], RCode3, RCode0, Dir),
	replace_lost_labels(WCode1).	% and discard the WCode sequence
unify_next_arg(Var, Prev, simple, Tmp, ChunkData0, ChunkData, Reg, Reg, WCode, WCode0, RCode, RCode0, Dir) :-
	Var = variable{},
	up(Prev, Tmp, Reg, WCode, WCode1, RCode, RCode1),
	unify_va(Var, ChunkData0, ChunkData, WCode1, WCode0, RCode1, RCode0, Dir).
unify_next_arg(Const, Prev, simple, Tmp, ChunkData, ChunkData, Reg, Reg, WCode, WCode0, RCode, RCode0, Dir) :-
	atomic(Const),
	up(Prev, Tmp, Reg, WCode, WCode1, RCode, RCode2),
	matching_test(Dir, RCode2, RCode1),
	WCode1 = [code{instr:WInstr}|WCode0],
	RCode1 = [code{instr:RInstr}|RCode0],
	unify_const(Const, WInstr, RInstr).


    % Generate the code for the last subterm
    % Arguments as above

:- mode unify_last_arg(+,+,+, +,-, +,-, -,?, -,?, +).
unify_last_arg(List, Prev, Tmp, ChunkData0, ChunkData, Reg0, Reg1, WCode, WCode0, RCode, RCode0, Dir) :-
	List = [_|_],
	up(Prev, Tmp, Reg0, WCode, WCode1, RCode, RCode1),
	WCode1 = [code{instr:write_list},code{instr:label(WL)}|WCode2],
	RCode1 = [code{instr:read_last_list(ref(WL))}|RCode2],
	alloc_term(List, ChunkData0, ChunkData1, Dir),
	unify_args(List, ChunkData1, ChunkData, Reg0, Reg1, WCode2, WCode0, RCode2, RCode0, Dir).
unify_last_arg(Struct, Prev, Tmp, ChunkData0, ChunkData, Reg0, Reg1, WCode, WCode0, RCode, RCode0, Dir) :-
	Struct = structure{name:F,arity:A},
	up(Prev, Tmp, Reg0, WCode, WCode1, RCode, RCode1),
	WCode1 = [code{instr:write_structure(F/A)},code{instr:label(WL)}|WCode2],
	RCode1 = [code{instr:read_last_structure(F/A,ref(WL))}|RCode2],
	alloc_term(Struct, ChunkData0, ChunkData1, Dir),
	unify_args(Struct, ChunkData1, ChunkData, Reg0, Reg1, WCode2, WCode0, RCode2, RCode0, Dir).
unify_last_arg(Avar, Prev, Tmp, ChunkData0, ChunkData, Reg0, Reg1, WCode, WCode, RCode, RCode0, Dir) :-
	Avar = attrvar{variable:Var,meta:Struct},
	verify Dir == in,
	up(Prev, Tmp, Reg0, _, [], RCode, RCode1),
	RCode1 = [code{instr:match_last_meta}|RCode2],
	unify_va(Var, ChunkData0, ChunkData1, _, [], RCode2, RCode3, Dir),
	RCode3 = [code{instr:read_attribute(FirstAttr)}|RCode4],
	meta_index(FirstAttr, 1),
	unify_args(Struct, ChunkData1, ChunkData, Reg0, Reg1, WCode1, [], RCode4, RCode0, Dir),
	replace_lost_labels(WCode1).	% and discard the WCode sequence
unify_last_arg(Var, Prev, Tmp, ChunkData0, ChunkData, Reg, Reg, WCode, WCode0, RCode, RCode0, Dir) :-
	Var = variable{},
	up(Prev, Tmp, Reg, WCode, WCode1, RCode, RCode1),
	unify_va(Var, ChunkData0, ChunkData, WCode1, WCode0, RCode1, RCode0, Dir).
unify_last_arg(Const, Prev, Tmp, ChunkData, ChunkData, Reg, Reg, WCode, WCode0, RCode, RCode0, Dir) :-
	atomic(Const),
	up(Prev, Tmp, Reg, WCode, WCode1, RCode, RCode2),
	matching_test(Dir, RCode2, RCode1),
	WCode1 = [code{instr:WInstr}|WCode0],
	RCode1 = [code{instr:RInstr}|RCode0],
	unify_const(Const, WInstr, RInstr).


    up(compound, Tmp, Reg, WCode, WCode0, RCode, RCode0) :-
	Off is Reg-Tmp,
	WCode = [code{instr:mode(t(Off),ref(RL))}|WCode0],
	RCode = [code{instr:mode(t(Off))},code{instr:label(RL)}|RCode0].
    up(simple, _Tmp, _Reg, WCode, WCode, RCode, RCode).

    % when compiling a matching clause: insert nonvar test into read mode
    matching_test(inout, Code, Code).
    matching_test(in, [code{instr:read_test_var}|Code0], Code0).


%----------------------------------------------------------------------
%
% Body subgoal arguments
%	The terms are built breadth-first, top-down, using two pointers.
%	TG is the allocation pointer and S is the write-pointer, lagging
%	behind and filling the allocated space. The code below uses a
%	queue for the breadth-first traversal.
%
% Improvements:
%	Sequence push_structure A, write_did F/A can be replaced
%		by write_structure F/A (peephole)
%
%	We don't really need to increment TG on every push_structure/list.
%	We could just allocate all the space in advance and then use
%	normal write-instructions to fill in the data.
%----------------------------------------------------------------------

%:- export body/6.

body(ArgId, Term, State, State, [code{instr:Instr,regs:[r(ArgId,R,def,_)]}|Code0], Code0, Module) :-
	atomic(Term),
	put_const(R, Term, Module, Instr).
body(ArgId, Term, State0, State, Code, Code0, _Module) :- Term = [_|_],
	Code = [code{instr:put_list(R),regs:[r(ArgId,R,def,_)]}|Code1],
	alloc_term(Term, State0, State1),
	push_args(Term, QueueHead, QueueTail, State1, State2, Code1, Code2),
	push_next_in_queue(QueueHead, QueueTail, State2, State, Code2, Code0).
body(ArgId, Term, State0, State, Code, Code0, _Module) :-
	Term = structure{name:F,arity:A},
	Code = [code{instr:put_structure(R, F/A),regs:[r(ArgId,R,def,_)]}|Code1],
	alloc_term(Term, State0, State1),
	push_args(Term, QueueHead, QueueTail, State1, State2, Code1, Code2),
	push_next_in_queue(QueueHead, QueueTail, State2, State, Code2, Code0).


push_args([H|T], QueueTail0, QueueTail, State0, State, Code, Code0) ?-
	push_arg(H, QueueTail0, QueueTail1, State0, State1, Code, Code1),
	push_arg(T, QueueTail1, QueueTail, State1, State, Code1, Code0).
push_args(structure{args:Args}, QueueTail0, QueueTail, State0, State, Code, Code0) ?-
	(
	    foreach(Arg, Args),
	    fromto(Code, Code1, Code2, Code0),
	    fromto(State0, State1, State2, State),
	    fromto(QueueTail0, QueueTail1, QueueTail2, QueueTail)
	do
	    push_arg(Arg, QueueTail1, QueueTail2, State1, State2, Code1, Code2)
	).


push_next_in_queue([], [], State0, State, Code, Code0) :- !,
	State = State0,
	Code = Code0.
push_next_in_queue([Term|QueueRest], QueueTail0, State0, State, Code, Code0) :-
	push_functor(Term, Code, Code1),
	push_args(Term, QueueTail0, QueueTail1, State0, State1, Code1, Code2),
	push_next_in_queue(QueueRest, QueueTail1, State1, State, Code2, Code0).

    :- mode push_functor(+,-,?).
    push_functor([_|_], Code, Code) :- !.
    push_functor(structure{name:F,arity:A}, [code{instr:write_did(F/A)}|Code], Code).

push_arg(Arg, Queue, Queue, State0, State, Code, Code0) :-
	Arg = variable{}, !,
	push_va(Arg, State0, State, Code, Code0).
push_arg(Arg, [Arg|Queue], Queue, State0, State, Code, Code0) :- Arg = [_|_],
	Code = [code{instr:push_list}|Code0],
	alloc_term(Arg, State0, State).
push_arg(Arg, [Arg|Queue], Queue, State0, State, Code, Code0) :-
	Arg = structure{arity:A},
	N is A+1,
	Code = [code{instr:push_structure(N)}|Code0],
	alloc_term(Arg, State0, State).
push_arg(Arg, Queue, Queue, State, State, [code{instr:Instr}|Code0], Code0) :-
	atomic(Arg),
	push_const(Arg, Instr).



%----------------------------------------------------------------------
% Code generation for unifying/constructing constants
% We could generate only read/write/push_constant
% instructions here and specialize them later
%----------------------------------------------------------------------

:- mode unify_const(+,-,-).
unify_const([],   write_nil,		read_nil) :- !.
unify_const(Term, write_atom(Term),	read_atom(Term)) :- atom(Term), !.
unify_const(Term, write_string(Term),   read_string(Term)) :- string(Term), !.
%unify_const(Term, write_float(Term),    read_float(Term)) :- float(Term), !.
unify_const(Term, write_constant(Term), read_constant(Term)) :- bignum(Term), !.
unify_const(Term, write_integer(Term),  read_integer(Term)) :- integer(Term), !.
unify_const(Term, write_constant(Term), read_constant(Term)).


:- mode push_const(+,-).
push_const([],   push_nil) :- !.
push_const(Term, write_atom(Term)) :- atom(Term), !.
push_const(Term, push_string(Term)) :- string(Term), !.
%push_const(Term, push_float(Term)) :- float(Term), !.
push_const(Term, push_constant(Term)) :- bignum(Term), !.
push_const(Term, push_integer(Term)) :- integer(Term), !.
push_const(Term, push_constant(Term)).


:- mode put_const(?,+,+,-).
put_const(R, [],   _M, put_nil(R)) :- !.
put_const(R, Mod, Mod, put_module(R,Mod)) :- atom(Mod), !.
put_const(R, Term, _M, put_atom(R,Term)) :- atom(Term), !.
put_const(R, Term, _M, put_string(R,Term)) :- string(Term), !.
%put_const(R, Term, _M, put_float(R,Term)) :- float(Term), !.
put_const(R, Term, _M, put_constant(R,Term)) :- bignum(Term), !.
put_const(R, Term, _M, put_integer(R,Term)) :- integer(Term), !.
put_const(R, Term, _M, put_constant(R,Term)).


:- mode get_const(?,+,-).
get_const(R, [],   get_nil(R)) :- !.
get_const(R, Term, get_atom(R,Term)) :- atom(Term), !.
get_const(R, Term, get_string(R,Term)) :- string(Term), !.
%get_const(R, Term, get_float(R,Term)) :- float(Term), !.
get_const(R, Term, get_constant(R,Term)) :- bignum(Term), !.
get_const(R, Term, get_integer(R,Term)) :- integer(Term), !.
get_const(R, Term, get_constant(R,Term)).

:- mode in_get_const(?,+,-).
in_get_const(R, [],   in_get_nil(R)) :- !.
in_get_const(R, Term, in_get_atom(R,Term)) :- atom(Term), !.
in_get_const(R, Term, in_get_string(R,Term)) :- string(Term), !.
%in_get_const(R, Term, in_get_float(R,Term)) :- float(Term), !.
in_get_const(R, Term, in_get_constant(R,Term)) :- bignum(Term), !.
in_get_const(R, Term, in_get_integer(R,Term)) :- integer(Term), !.
in_get_const(R, Term, in_get_constant(R,Term)).



%----------------------------------------------------------------------
% Code generation for unifying/constructing variables inside compounds.
% See put_variable and unify_variable for details of what to put into
% the register annotations of the instructions.
% An extra complication here arises from the two code sequences (read
% and write code): The register allocator in principle works only with
% a simple linear code sequence, not with general spaghetti code.
% Fortunately, the code here is special in that the read and write
% sequence are isomorphic wrt register usage, and the branches are
% structured hierarchically.  What we do is to tell the allocator
% (via the split/restore/join annotations in head/6) to save its state
% when the code forks, and to reset its state when the write sequence
% starts, so the write-sequence reg allocation will be identical to
% the one for the read-sequence.
%----------------------------------------------------------------------

:- mode push_va(+,+,-,-,?).

push_va(Var, ChunkData0, ChunkData, Code0, Code) :-
	Var = variable{varid:VarId},
	variable_occurrence(Var, ChunkData0, ChunkData, Code0, Code1, VarOccDesc),
	push_va_code(VarOccDesc, VarId, Code1, Code).

    push_va_code(void, _VarId, Code, Code0) :-
	Code = [code{instr:push_void}|Code0].

    push_va_code(tmp_first, VarId, Code, Code0) :-
	Code = [code{instr:push_variable(R),regs:[r(VarId,R,def,_)]}|Code0].

    push_va_code(tmp, VarId, Code, Code0) :-
	Code = [code{instr:push_value(R),regs:[r(VarId,R,use,_)]}|Code0].
%	Code = [code{instr:push_local_value(R),regs:[r(VarId,R,use,_)]}|Code0].		%%% WHEN VARS IN ENVIRONMENT

    push_va_code(perm_first(Y), VarId, Code, Code0) :-
	Code = [code{instr:push_variable(Y),regs:[r(VarId,Y,perm,_)]}|Code0].

    push_va_code(perm_first_in_chunk(Y), VarId, Code, Code0) :-
	Code = [code{instr:push_value(Y),regs:[r(VarId,Y,perm,_)]}|Code0].
%	Code = [code{instr:push_local_value(Y),regs:[r(VarId,Y,perm,_)]}|Code0].		%%% WHEN VARS IN ENVIRONMENT

    push_va_code(perm(_Y), VarId, Code, Code0) :-
	Code = [code{instr:push_value(RY),regs:[r(VarId,RY,use,_)]}|Code0].
%	Code = [code{instr:push_local_value(RY),regs:[r(VarId,RY,use,_)]}|Code0].		%%% WHEN VARS IN ENVIRONMENT



unify_va(Var, ChunkData0, ChunkData, WCode, WCode0, RCode, RCode0, inout) :-
	unify_va(Var, ChunkData0, ChunkData, WCode, WCode0, RCode, RCode0).
unify_va(Var, ChunkData0, ChunkData, WCode, WCode, RCode, RCode0, in) :-
	in_unify_va(Var, ChunkData0, ChunkData, RCode, RCode0).


:- mode unify_va(+,+,-,-,?,-,?).

unify_va(Var, ChunkData0, ChunkData, WCode, WCode0, RCode, RCode0) :-
	Var = variable{varid:VarId},
	variable_occurrence(Var, ChunkData0, ChunkData1, _AllocateCode, [], VarOccDesc),
	% AllocateCode is empty or consists of one allocate(N) instruction.
	% It is discarded here, and later reconstructed and prefixed to the
	% whole compound unification, to avoid interference with stack temps.
	unify_va_code(VarOccDesc, VarId, WCode, WCode0, RCode, RCode1, GAlloc),
	alloc_check_after(GAlloc, ChunkData1, ChunkData, RCode1, RCode0).

    unify_va_code(void, _VarId, WCode, WCode0, RCode, RCode0, 0) :-
	WCode = [code{instr:write_void}|WCode0],
	RCode = [code{instr:read_void} |RCode0].

    unify_va_code(tmp_first, VarId, WCode, WCode0, RCode, RCode0, 0) :-
	WCode = [code{instr:write_variable(R),regs:[r(VarId,R,def,_)]}|WCode0],
	RCode = [code{instr:read_variable(R),regs:[r(VarId,R,def,_)]} |RCode0].

    unify_va_code(tmp, VarId, WCode, WCode0, RCode, RCode0, unbounded_maybe) :-
	WCode = [code{instr:write_value(R),regs:[r(VarId,R,use,_)]}|WCode0],
%	WCode = [code{instr:write_local_value(R),regs:[r(VarId,R,use,_)]}|WCode0],		%%% WHEN VARS IN ENVIRONMENT
	RCode = [code{instr:read_value(R),regs:[r(VarId,R,use,_)]} |RCode0].

    unify_va_code(perm_first(Y), VarId, WCode, WCode0, RCode, RCode0, 0) :-
	WCode = [code{instr:write_variable(Y),regs:[r(VarId,Y,perm,_)]}|WCode0],
	RCode = [code{instr:read_variable(Y),regs:[r(VarId,Y,perm,_)]} |RCode0].

    unify_va_code(perm_first_in_chunk(Y), VarId, WCode, WCode0, RCode, RCode0, unbounded_maybe) :-
	WCode = [code{instr:write_value(Y),regs:[r(VarId,Y,perm,_)]}|WCode0],
%	WCode = [code{instr:write_local_value(Y),regs:[r(VarId,Y,perm,_)]}|WCode0],		%%% WHEN VARS IN ENVIRONMENT
	RCode = [code{instr:read_value(Y),regs:[r(VarId,Y,perm,_)]} |RCode0].

    unify_va_code(perm(_Y), VarId, WCode, WCode0, RCode, RCode0, unbounded_maybe) :-
	WCode = [code{instr:write_value(RY),regs:[r(VarId,RY,use,_)]}|WCode0],
%	WCode = [code{instr:write_local_value(RY),regs:[r(VarId,RY,use,_)]}|WCode0],		%%% WHEN VARS IN ENVIRONMENT
	RCode = [code{instr:read_value(RY),regs:[r(VarId,RY,use,_)]} |RCode0].


% Matching only

:- mode in_unify_va(+,+,-,-,?).

in_unify_va(Var, ChunkData0, ChunkData, RCode, RCode0) :-
	Var = variable{varid:VarId},
	variable_occurrence(Var, ChunkData0, ChunkData, _AllocateCode, [], VarOccDesc),
	% AllocateCode is empty or consists of one allocate(N) instruction.
	% It is discarded here, and later reconstructed and prefixed to the
	% whole compound unification, to avoid interference with stack temps.
	in_unify_va_code(VarOccDesc, VarId, RCode, RCode0).

    in_unify_va_code(void, _VarId, RCode, RCode0) :-
	RCode = [code{instr:read_void} |RCode0].

    in_unify_va_code(tmp_first, VarId, RCode, RCode0) :-
	RCode = [code{instr:read_variable(R),regs:[r(VarId,R,def,_)]} |RCode0].

    in_unify_va_code(tmp, VarId, RCode, RCode0) :-
	RCode = [code{instr:read_matched_value(R),regs:[r(VarId,R,use,_)]} |RCode0].

    in_unify_va_code(perm_first(Y), VarId, RCode, RCode0) :-
	RCode = [code{instr:read_variable(Y),regs:[r(VarId,Y,perm,_)]} |RCode0].

    	% does not occur as long as matching only in head
    in_unify_va_code(perm_first_in_chunk(Y), VarId, RCode, RCode0) :-
	RCode = [code{instr:read_matched_value(Y),regs:[r(VarId,Y,perm,_)]} |RCode0].

    in_unify_va_code(perm(_Y), VarId, RCode, RCode0) :-
	RCode = [code{instr:read_matched_value(RY),regs:[r(VarId,RY,use,_)]} |RCode0].


%----------------------------------------------------------------------
% Track global stack space needed for constructing terms
%----------------------------------------------------------------------

alloc_term(_, ChunkData, ChunkData, in).
alloc_term(Term, ChunkData0, ChunkData, inout) :-
	alloc_term(Term, ChunkData0, ChunkData).

alloc_term(structure{arity:A}, ChunkData0, ChunkData) :- !,
	N is A+1,
	alloc_check_pwords(N, ChunkData0, ChunkData).
alloc_term([_|_], ChunkData0, ChunkData) :- !,
	alloc_check_pwords(2, ChunkData0, ChunkData).
%alloc_term(X, ChunkData0, ChunkData) :- float(X), !,
%	alloc_check_pwords(2, ChunkData0, ChunkData).
%alloc_term(X, ChunkData0, ChunkData) :- integer(X), !,
%	( X =< 16'7fffffff, X+1 >= -16'7fffffff ->
%	    % 32-bit integer
%	    ChunkData = ChunkData0
%	;
%	    % potential bignum
%	    ( count(_,2,N), fromto(X,X1,X2,0) do X2 is X1>>30>>30>>4 ),
%	    alloc_check_pwords(N, ChunkData0, ChunkData)
%	).
%alloc_term(X, ChunkData0, ChunkData) :- string(X), !,
%	N is (string_length(X)+16)//8,
%	alloc_check_pwords(N, ChunkData0, ChunkData)
alloc_term(_, ChunkData, ChunkData) :-
	unreachable("alloc_term").

