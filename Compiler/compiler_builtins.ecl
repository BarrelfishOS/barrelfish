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
% Version:	$Id: compiler_builtins.ecl,v 1.5 2015/01/14 01:31:08 jschimpf Exp $
%
% Part of module(compiler_codegen)
% ----------------------------------------------------------------------

%----------------------------------------------------------------------
% Generate code for "simple" built-ins with flexible argument registers.
% These are compiled using dedicated instructions of the form
%	instr Ai ... Ak
%----------------------------------------------------------------------

generate_simple_goal(Goal, ChunkData0, ChunkData, Code0, Code, Options, Module) :-
	Goal = goal{functor: (=)/2, args:[Arg1,Arg2],definition_module:sepia_kernel},
	generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code1, Code2, Module), % may fail
	emit_debug_noarg(Goal, Code0, Code1, Code2, Code, Options, Module),
	!.

generate_simple_goal(Goal, ChunkData0, ChunkData, Code0, Code, Options, Module) :-
	Goal = goal{functor: (==)/2, args:[Arg1,Arg2],definition_module:sepia_kernel},
	generate_identity(Arg1, Arg2, ChunkData0, ChunkData, Code1, Code2), % may fail
	emit_debug_noarg(Goal, Code0, Code1, Code2, Code, Options, Module),
	!.

generate_simple_goal(Goal, ChunkData0, ChunkData, Code0, Code, _Options, _Module) :-
	Goal = goal{functor: (?=)/2, args:[Arg1,Arg2],definition_module:sepia_kernel},
	!,
	generate_in_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code).

generate_simple_goal(goal{functor: get_cut/1, args:[Arg],definition_module:sepia_kernel}, ChunkData0, ChunkData, Code0, Code, _Options, _Module) ?- !,
	Arg = variable{varid:VarId},
	variable_occurrence(Arg, ChunkData0, ChunkData, Code0, Code1, VarOccDesc),
	( VarOccDesc = void ->
	    Code1 = Code
	; VarOccDesc = tmp_first ->
	    Code1 = [code{instr:savecut(R),regs:[r(VarId,R,def,_)]}|Code]
	; VarOccDesc = perm_first(Y) ->
	    Code1 = [code{instr:savecut(Y),regs:[r(VarId,Y,perm,_)]}|Code]
	;
	    verify false	% require a first occurrence!
	).

generate_simple_goal(goal{functor: cut_to/1, args:[Arg],definition_module:sepia_kernel,envsize:ESize,state:State}, ChunkData0, ChunkData, Code0, Code, _Options, _Module) ?- !,
	Arg = variable{varid:VarId},
	variable_occurrence(Arg, ChunkData0, ChunkData1, Code0, Code1, VarOccDesc),
	( compiler_analysis:state_lookup_binding(State, VarId, ++(cutpoint(_))) ->
	    ChunkData1 = chunk_data{allocated:ExistingESize},
	    ( ESize >= 0 ->
		% cut (and trim if necessary)
		( nonvar(ExistingESize) -> true ;
		    ExistingESize = ESize	% fill in previous alloc
		),
		( ExistingESize >= 0 ->
		    % cut and trim existing environment
		    verify ExistingESize >= ESize,
		    update_struct(chunk_data, [allocated:ESize], ChunkData1, ChunkData),
		    ( VarOccDesc = perm_first_in_chunk(Y) ->
			Code1 = [code{instr:cut(Y,ESize),regs:[r(VarId,Y,perm,_)]}|Code]
		    ; VarOccDesc = perm(_Y) ->
			Code1 = [code{instr:cut(RY,ESize),regs:[r(VarId,RY,use,_)]}|Code]
		    ; verify VarOccDesc = tmp,
			Code1 = [code{instr:cut(RY,ESize),regs:[r(VarId,RY,use,_)]}|Code]
		    )
		;
		    % no environment, just cut (allocation expected later)
		    verify VarOccDesc == tmp,
		    Code1 = [code{instr:cut(R),regs:[r(VarId,R,use,_)]}|Code],
		    ChunkData1 = ChunkData
		)
	    ;
		% deallocation request
		( nonvar(ExistingESize) -> true ;
		    unreachable("unexpected allocate..cut..deallocate sequence"),
		    ExistingESize = 0	% fill in previous alloc
		),
		( ExistingESize >= 0 ->
		    % cut and deallocate existing environment
		    update_struct(chunk_data, [allocated: -1], ChunkData1, ChunkData),
		    ( VarOccDesc = perm_first_in_chunk(Y) ->
			Code1 = [code{instr:cut(Y,0),regs:[r(VarId,Y,perm,_)]}, code{instr:deallocate}|Code]
		    ; VarOccDesc = perm(_Y) ->
			Code1 = [code{instr:cut(RY,0),regs:[r(VarId,RY,use,_)]}, code{instr:deallocate}|Code]
		    ; verify VarOccDesc = tmp,
			Code1 = [code{instr:cut(RY,0),regs:[r(VarId,RY,use,_)]}, code{instr:deallocate}|Code]
		    )
		; 
		    % no environment anyway, just cut
		    verify VarOccDesc == tmp,
		    Code1 = [code{instr:cut(R),regs:[r(VarId,R,use,_)]}|Code],
		    ChunkData1 = ChunkData
		)
	    )
	;
	    % If the binding analyser couldn't verify that the argument holds
	    % a cutpoint, assume it is a non-local cut_to:
	    % Use cut(R) instruction and don't trim.
	    ( VarOccDesc = perm_first_in_chunk(Y) ->
		Code1 = [code{instr:cut(R),regs:[r(VarId,Y,perm,_),r(VarId,R,use_a,_)]}|Code]
	    ; VarOccDesc = perm(_Y) ->
		Code1 = [code{instr:cut(R),regs:[r(VarId,R,use_a,_)]}|Code]
	    ; verify VarOccDesc = tmp,
		Code1 = [code{instr:cut(R),regs:[r(VarId,R,use_a,_)]}|Code]
	    ),
	    ChunkData1 = ChunkData
	).

generate_simple_goal(Goal, ChunkData0, ChunkData, Code0, Code, Options, Module) ?-
	Goal = goal{functor: (-)/3, args:[A1,A2in,A3], definition_module:sepia_kernel},
	integer(A2in), A2 is -A2in, smallint(A2),
	!,
	update_struct(goal, [functor:(+)/3,args:[A1,A2,A3]], Goal, Goal1),
	generate_simple_goal(Goal1, ChunkData0, ChunkData, Code0, Code, Options, Module).

generate_simple_goal(Goal, ChunkData0, ChunkData, Code0, Code, Options, Module) ?-
	Goal = goal{functor: (+)/3, args:[A1,A2,A3], definition_module:sepia_kernel},
	smallint(A1), \+smallint(A2),
	!,
	update_struct(goal, [args:[A2,A1,A3]], Goal, Goal1),
	generate_simple_goal(Goal1, ChunkData0, ChunkData, Code0, Code, Options, Module).

generate_simple_goal(Goal, ChunkData0, ChunkData, Code0, Code, Options, Module) ?-
	Goal = goal{functor:Name/Arity, args:Args,definition_module:sepia_kernel},
	inlined_builtin(Name, Arity, GlobalAlloc, InstrTemplate),	% nondet
	functor(InstrTemplate, InstrName, N),
	functor(Instr, InstrName, N),
	% We have to generate the code in the correct order because of the
	% occurrence information in ChunkData. First the input arguments:
	(
	    foreach(Arg,Args),
	    count(I,1,_),
	    fromto(RegDescs,RegDescs1,RegDescs2,RegDescs3),
	    fromto(ChunkData0, ChunkData1, ChunkData2, ChunkData3),
	    fromto(Code0, Code1, Code2, Code3),
	    fromto(0,ArgDesc1,ArgDesc2,ArgDesc),
	    param(Instr,InstrTemplate,Module)
	do
	    arg(I, Instr, Reg),
	    arg(I, InstrTemplate, Expect),
	    add_arg_desc(I, Expect, ArgDesc1, ArgDesc2),
	    ( Expect == int ->
	    	smallint(Arg),			% may fail
		Reg=Arg, RegDescs1=RegDescs2, ChunkData1=ChunkData2, Code1=Code2
	    ; Expect == mod ->
	    	atom(Arg),			% may fail
		Reg=Arg, RegDescs1=RegDescs2, ChunkData1=ChunkData2, Code1=Code2
	    ; Expect == arg ->
		RegDescs1 = [r(ValId,Reg,use_a,_)|RegDescs2],
		put_term(Arg, ChunkData1, ChunkData2, Code1, Code2, ValId, Module)
	    ;
		% ignore here - treat in second pass
		RegDescs1=RegDescs2, ChunkData1=ChunkData2, Code1=Code2
	
	    )
	),
	!,	% Commit to this instruction variant and emit it
	( N>0, arg(N, InstrTemplate, desc) ->
	    arg(N, Instr, ArgDesc), DbgArgDesc = -1, NArgs is N-1
	;
	    DbgArgDesc = ArgDesc, NArgs = N
	),
	emit_call_simple(Instr, NArgs, DbgArgDesc, RegDescs, Goal, Code3, Code4, DbgLabel, Options, Module),
	alloc_check_after(GlobalAlloc, ChunkData3, ChunkData4, Code4, Code5),
	% Now generate code for result unification, if necessary
	(
	    foreach(Arg,Args),
	    count(I,1,_),
	    fromto(RegDescs3,RegDescs4,RegDescs5,[]),
	    fromto(ChunkData4, ChunkData5, ChunkData7, ChunkData),
	    fromto(Code5, Code6, Code8, Code9),
	    param(Instr,InstrTemplate)
	do
	    arg(I, Instr, Reg),
	    arg(I, InstrTemplate, Expect),
	    ( Expect == uarg ->
		RegDescs4 = [RegDesc|RegDescs5],
		unify_result(Arg, Reg, RegDesc, ChunkData5, ChunkData6, Code6, Code7, GlobalAlloc2),
		alloc_check_after(GlobalAlloc2, ChunkData6, ChunkData7, Code7, Code8)
	    ;
		% these were treated in the first pass
		RegDescs4=RegDescs5, ChunkData5=ChunkData7, Code6=Code8
	    )
	),
	emit_exit_simple(InstrTemplate, DbgLabel, Code9, Code, Options).

generate_simple_goal(goal{functor: P, args:Args}, ChunkData0, ChunkData, Code0, Code, _Options, Module) ?-
	P = _/Arity,
	dim(RegArr, [Arity]),
	dim(RegDescs, [Arity]),
	heuristic_put_order(Args, ChunkData0, OrderedPuts),
	(
	    foreach(put(_,I,Arg),OrderedPuts),
	    fromto(ChunkData0, ChunkData2, ChunkData3, ChunkData4),
	    fromto(Code0, Code1, Code2, Code3),
	    param(RegArr,RegDescArr,Module)
	do
	    arg(I, RegArr, Reg),
	    arg(I, RegDescArr, r(ValId,Reg,use_a,_)),
	    put_term(Arg, ChunkData2, ChunkData3, Code1, Code2, ValId, Module)
	),
	RegArr =.. [_|Regs],
	RegDescArr =.. [_|RegDescs],
	Code3 = [code{instr:escape(P,Regs),regs:RegDescs}|Code4],
	alloc_check_after(unbounded, ChunkData4, ChunkData, Code4, Code).


% All the builtins that can be implemented by an instruction like
%	bi_inst Ai ... Ak [desc]

% inlined_builtin(+Name, +Arity, -MaxGlobalAlloc, -InstructionTemplate)
%
% MaxGlobalAlloc could be made more precise:
%	arithmetics	can create bignums, therefore unbounded
%	others		can only delay or make exception -> const+arity
%
% InstructionTemplate has the instruction name as its functor, and its
% arguments describe how the instruction interprets its arguments.
%	arg	pointer to argument register
%	uarg	pointer to uninitialised argument register
%	int	32-bit integer
%	mod	module did
%	desc	runtime descriptor for the preceding arguments
%
% If a builtin has special instruction templates for special cases,
% they must be listed before the general template (see e.g. +/3).
%
inlined_builtin(fail,		0,	0,		failure).
inlined_builtin(false,		0,	0,		failure).
inlined_builtin(free,		1,	0,		bi_free(arg)).
inlined_builtin(is_suspension,	1,	0,		bi_is_suspension(arg)).
inlined_builtin(is_event,	1,	0,		bi_is_event(arg)).
inlined_builtin(is_handle,	1,	0,		bi_is_handle(arg)).
inlined_builtin(nonvar,		1,	0,		bi_nonvar(arg)).
inlined_builtin(var,		1,	0,		bi_var(arg)).
inlined_builtin(meta,		1,	0,		bi_meta(arg)).
inlined_builtin(atom,		1,	0,		bi_atom(arg)).
inlined_builtin(integer,	1,	0,		bi_integer(arg)).
inlined_builtin(bignum,		1,	0,		bi_bignum(arg)).
inlined_builtin(rational,	1,	0,		bi_rational(arg)).
inlined_builtin(real,		1,	0,		bi_real(arg)).
inlined_builtin(float,		1,	0,		bi_float(arg)).
inlined_builtin(breal,		1,	0,		bi_breal(arg)).
inlined_builtin(string,		1,	0,		bi_string(arg)).
inlined_builtin(number,		1,	0,		bi_number(arg)).
inlined_builtin(atomic,		1,	0,		bi_atomic(arg)).
inlined_builtin(callable,	1,	0,		bi_callable(arg)).
inlined_builtin(compound,	1,	0,		bi_compound(arg)).
inlined_builtin(is_list,	1,	0,		bi_is_list(arg)).
inlined_builtin(==,		2,	0,		get_matched_value(arg,arg)).
inlined_builtin(\==,		2,	0,		bi_not_identical(arg,arg)).
inlined_builtin(set_bip_error,	1,	0,		bi_set_bip_error(arg)).
inlined_builtin(cont_debug,	0,	0,		bi_cont_debug).
inlined_builtin(sys_return,	1,	0,		bi_exit(arg)).
inlined_builtin(\==,		3,	unbounded,	bi_not_ident_list(arg,arg,arg)).
inlined_builtin(~=,		2,	unbounded,	bi_inequality(arg,arg)).
inlined_builtin(make_suspension, 4,	unbounded,	bi_make_suspension(arg,arg,arg,arg,desc)).
inlined_builtin(=:=,		3,	unbounded,	bi_eq(arg,arg,mod,desc)).
inlined_builtin(=:=,		3,	unbounded,	bi_eq(arg,arg,arg,desc)).
inlined_builtin(=\=,		3,	unbounded,	bi_ne(arg,arg,mod,desc)).
inlined_builtin(=\=,		3,	unbounded,	bi_ne(arg,arg,arg,desc)).
inlined_builtin(<,		3,	unbounded,	bi_lt(arg,arg,mod,desc)).
inlined_builtin(<,		3,	unbounded,	bi_lt(arg,arg,arg,desc)).
inlined_builtin(>,		3,	unbounded,	bi_gt(arg,arg,mod,desc)).
inlined_builtin(>,		3,	unbounded,	bi_gt(arg,arg,arg,desc)).
inlined_builtin(=<,		3,	unbounded,	bi_le(arg,arg,mod,desc)).
inlined_builtin(=<,		3,	unbounded,	bi_le(arg,arg,arg,desc)).
inlined_builtin(>=,		3,	unbounded,	bi_ge(arg,arg,mod,desc)).
inlined_builtin(>=,		3,	unbounded,	bi_ge(arg,arg,arg,desc)).
inlined_builtin(-,		2,	unbounded,	bi_minus(arg,uarg,desc)).
inlined_builtin(+,		3,	unbounded,	bi_addi(arg,int,uarg,desc)).
inlined_builtin(+,		3,	unbounded,	bi_add(arg,arg,uarg,desc)).
inlined_builtin(-,		3,	unbounded,	bi_sub(arg,arg,uarg,desc)).
inlined_builtin(*,		3,	unbounded,	bi_mul(arg,arg,uarg,desc)).
inlined_builtin(/,		3,	unbounded,	bi_quot(arg,arg,uarg,desc)).
inlined_builtin(//,		3,	unbounded,	bi_div(arg,arg,uarg,desc)).
inlined_builtin(rem,		3,	unbounded,	bi_rem(arg,arg,uarg,desc)).
inlined_builtin(div,		3,	unbounded,	bi_fdiv(arg,arg,uarg,desc)).
inlined_builtin(mod,		3,	unbounded,	bi_mod(arg,arg,uarg,desc)).
inlined_builtin(/\,		3,	unbounded,	bi_and(arg,arg,uarg,desc)).
inlined_builtin(\/,		3,	unbounded,	bi_or(arg,arg,uarg,desc)).
inlined_builtin(xor,		3,	unbounded,	bi_xor(arg,arg,uarg,desc)).
inlined_builtin(><,		3,	unbounded,	bi_xor(arg,arg,uarg,desc)).
inlined_builtin(\,		2,	unbounded,	bi_bitnot(arg,uarg,desc)).
inlined_builtin(arg,		3,	unbounded,	bi_arg(int,arg,uarg,desc)).
inlined_builtin(arg,		3,	unbounded,	bi_arg(arg,arg,uarg,desc)).
inlined_builtin(arity,		2,	unbounded,	bi_arity(arg,uarg,desc)).
inlined_builtin(get_bip_error,	1,	0,		bi_get_bip_error(uarg)).
inlined_builtin(compare,	3,	0,		bi_compare(uarg,arg,arg)).
inlined_builtin(list_end,	2,	0,		bi_list_end(arg,uarg)).
inlined_builtin(qualify_,	3,	3,		bi_qualify(arg,uarg,arg)).

% Encode argument descriptors in bitmask, 2 bits per argument
add_arg_desc(_,  arg, Desc, Desc).
add_arg_desc(I, uarg, Desc0, Desc) :- Desc is Desc0 + 1 << (2*(I-1)).
add_arg_desc(I,  int, Desc0, Desc) :- Desc is Desc0 + 2 << (2*(I-1)).
add_arg_desc(I,  mod, Desc0, Desc) :- Desc is Desc0 + 3 << (2*(I-1)).


% Tracing inlined builtins:
% A fundamental problem with generating a trace that is close to the source code
% is that for inlined predicates the arguments (especially output) do not exist
% at call time, and may well never exist (e.g. when the result unification is
% compiled into a get_structure-sequence). We could have a debug-compilation
% mode that generates completely different unoptimized code, but we are concerned
% here with generating a usable trace for optimized code, by just adding extra
% debug information.  Because of the output argument problem, we display those
% arguments at call ports as '...', but show them at exit ports.
% To generate the ports, we rely on the argument positions of the main instruction
% implementing the builtin. The debug_call/exit instructions have a reference
% that points *behind* this argument block, possibly terminated by an argument
% descriptor (desc).  If the instruction has no argument descriptor itself,
% we supply one in the debug instruction itself.
%
% Builtins without output arguments:
% 
% 	debug_call_simple ... -1 NArgs
%	bi_xxx  a1 ... an desc			e.g. >/2
% 	debug_exit_simple
% 
% 	debug_call_simple ... desc NArgs
%	bi_xxx  a1 ... an			e.g. atom/1
% 	debug_exit_simple
%
% Instructions with uninitialised output registers: the debug_exit instruction
% receives additional parameters, used to display output values at exit ports.
%
% L:	debug_call_simple ... -1 NArgs
%	bi_xxx  a1 ... uan desc			e.g. +/3, arg/3
% 	<possible output unifications>
%	debug_exit_simple ref(L)
%
% L:	debug_call_simple ... desc NArgs
%	bi_xxx  a1 ... uan			e.g. get_bip_error/1
% 	<possible output unifications>
%	debug_exit_simple ref(L)
%
% where NArgs indicates the number of arguments to the bi_xxx instruction,
% and a desc of -1 means that the bi_xxx instruction has its own desc.

emit_call_simple(BiInstr, NArgs, DbgArgDesc, RegDescs, Goal, Code, Code0, DbgLabel, options{debug:Debug}, Module) :-
	( Debug == off ->
	    Code = [code{instr:BiInstr,regs:RegDescs}|Code0]
	;
	    % CAUTION: the RegDescs must be given to the label instruction instead
	    % of the bi_xxx instructions to make sure that the register allocator
	    % (which may need to insert moves) puts all values in place *before*
	    % the debug instruction is executed, and to make sure nothing goes
	    % between the label and the actual debug_call_simple instruction.
	    Goal = goal{functor:Pred,lookup_module:LM,path:Path,line:Line,from:From,to:To},
	    ( LM\==Module -> QPred = LM:Pred ; QPred = Pred ),
	    Code = [
		    code{instr:label(DbgLabel),regs:RegDescs},
		    code{instr:debug_call_simple(QPred,#call_port,Path,Line,From,To,DbgArgDesc,NArgs)},
		    code{instr:BiInstr}|Code0]
	).


emit_exit_simple(InstrTmpl, DbgLabel, Code, Code0, options{debug:Debug}) :-
	( Debug == off ->
	    Code = Code0
	; (foreacharg(Arg,InstrTmpl) do Arg \= uarg) ->
	    Code = [code{instr:debug_exit_simple}|Code0]
	;
	    % The instruction has uninitialised output arguments: Improve the
	    % trace for this special case by giving extra parameters to
	    % debug_exit_simple that allow "patching" the debug-exit frame.
	    Code = [code{instr:debug_exit_simple(0/*unused*/,ref(DbgLabel))}|Code0]
	).

emit_debug_noarg(Goal, CallCode, CallCode0, ExitCode, ExitCode0, options{debug:Debug}, Module) :-
	( Debug == off ->
	    CallCode = CallCode0, ExitCode = ExitCode0
	;
	    Goal = goal{functor:Pred,lookup_module:LM,path:Path,line:Line,from:From,to:To},
	    ( LM == sepia_kernel ->
		% If the LM is sepia_kernel, it is most likely a goal that was
		% inserted by the normalisation phase - don't trace it!
		CallCode = CallCode0, ExitCode = ExitCode0
	    ;
		% Create debug instructions with all arguments hidden (by
		% pretending they are all uargs - hack)
		( LM\==Module -> QPred = LM:Pred ; QPred = Pred ),
		Pred = _/N,
		( for(I,1,N), fromto(0,ArgDesc1,ArgDesc2,ArgDesc) do
		    add_arg_desc(I, uarg, ArgDesc1, ArgDesc2)
		),
		CallCode = [code{instr:debug_call_simple(QPred,#call_port,Path,Line,From,To,ArgDesc,0),regs:[]}|CallCode0],
		ExitCode = [code{instr:debug_exit_simple}|ExitCode0]
	    )
	).


%----------------------------------------------------------------------
% The special case of =/2 goal
% Due to normalisation, all should be in the form Var=Term!
% Fail if no special treatment possible (shouldn't happen)

generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code, _Module) :-
	Arg1 = variable{varid:VarId1},
	Arg2 = variable{varid:VarId2},
	!,
	( VarId1 = VarId2 ->
	    % pretend X=X was X=_
	    ChunkData0 = ChunkData1, Code0 = Code1, VarOccDesc1 = void
	;
	    variable_occurrence(Arg1, ChunkData0, ChunkData1, Code0, Code1, VarOccDesc1)
	),
	variable_occurrence(Arg2, ChunkData1, ChunkData2, Code1, Code2, VarOccDesc2),
	unify_variables(VarOccDesc1, VarId1, VarOccDesc2, VarId2, Code2, Code3, GAlloc),
	alloc_check_after(GAlloc, ChunkData2, ChunkData, Code3, Code).
generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code, Module) :-
	Arg1 = variable{varid:VarId},
	!,
	variable_occurrence(Arg1, ChunkData0, ChunkData1, Code0, Code1, VarOccDesc),
	bind_variable(VarOccDesc, VarId, Arg2, ChunkData1, ChunkData, Code1, Code, Module).
generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code, Module) :-
	Arg2 = variable{},
	!,
	generate_unify(Arg2, Arg1, ChunkData0, ChunkData, Code0, Code, Module).
generate_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code, _Module) :-
	atomic(Arg1), atomic(Arg2), !,
	ChunkData0 = ChunkData,
	( Arg1 = Arg2 ->
	    Code0 = Code
	;
	    Code0 = [code{instr:failure,regs:[]}|Code]
	).
generate_unify(_Arg1, _Arg2, _ChunkData0, _ChunkData, _Code0, _Code, _Module) :-
	writeln(warning_output,
	    "WARNING: nonvar = nonvar unification should be unwrapped by preprocessing"),
	fail.

%
% Generate code for the unification of a variable and a nonvariable.
%
% bind_variable(VarOccDesc, VarId, Term, ChunkData0, ChunkData, Code, Code0, Module)
%

bind_variable(void, _VarId, Term, ChunkData0, ChunkData, Code, Code0, _Module) :-
	void_term(Term, ChunkData0, ChunkData, Code, Code0).
bind_variable(tmp_first, VarId, Term, ChunkData0, ChunkData, Code, Code0, Module) :-
	body(VarId, Term, ChunkData0, ChunkData, Code, Code0, Module).
bind_variable(tmp, VarId, Term, ChunkData0, ChunkData, Code, Code0, _Module) :-
	head(VarId, Term, ChunkData0, ChunkData, Code, Code0).
bind_variable(perm_first(Y), VarId, Term, ChunkData0, ChunkData, Code, Code0, Module) :-
	Code1 = [code{instr:move(R,Y),regs:[r(VarId,R,use_a,_),r(VarId,Y,perm,_)]}|Code0],
	body(VarId, Term, ChunkData0, ChunkData, Code, Code1, Module).
bind_variable(perm_first_in_chunk(Y), VarId, Term, ChunkData0, ChunkData, Code, Code0, _Module) :-
	Code = [code{instr:nop,regs:[r(VarId,Y,perm,_)]}|Code1],
	head(VarId, Term, ChunkData0, ChunkData, Code1, Code0).
bind_variable(perm(_Y), VarId, Term, ChunkData0, ChunkData, Code, Code0, _Module) :-
	head(VarId, Term, ChunkData0, ChunkData, Code, Code0).


%
% Code generation for: _void = Term
% We only need to take care of side effects of variable occurrences
%

void_term(Term, State, State, Code, Code) :-
	atomic(Term).
void_term([T1|T2], State0, State, Code, Code0) :-
	void_term(T1, State0, State1, Code, Code1),
	void_term(T2, State1, State, Code1, Code0).
void_term(structure{args:Args}, State0, State, Code, Code0) :-
	void_term(Args, State0, State, Code, Code0).
void_term(Var, State0, State, Code, Code0) :-
	Var = variable{varid:VarId},
	variable_occurrence(Var, State0, State1, Code, Code1, VarOccDesc),
	unify_variables_ord(void, 0, VarOccDesc, VarId, Code1, Code0, GAlloc),
	alloc_check_pwords(GAlloc, State1, State).


%
% Generate code for unifying the value in register R with Arg
%

unify_result(Arg, R, RegDesc, ChunkData0, ChunkData, Code0, Code, GAlloc) :-
	Arg = variable{varid:VarId}, !,
	variable_occurrence(Arg, ChunkData0, ChunkData1, Code0, Code1, VarOccDesc),
	( VarOccDesc = void ->
	    RegDesc = r(VarId,R,def,_last),
	    ChunkData1 = ChunkData, Code1 = Code, GAlloc = 0
	; VarOccDesc = tmp_first ->
	    RegDesc = r(VarId,R,def,_),
	    ChunkData1 = ChunkData, Code1 = Code, GAlloc = 0
	; VarOccDesc = tmp ->
	    new_aux_temp(ChunkData1, ChunkData, TVarId),
	    RegDesc = r(TVarId,R,def,_), GAlloc = unbounded,
	    Code1 = [code{instr:get_value(R1,R2), regs:[r(TVarId,R1,use,_),r(VarId,R2,use,_)]}|Code]
	; VarOccDesc = perm(_Y) ->
	    new_aux_temp(ChunkData1, ChunkData, TVarId),
	    RegDesc = r(TVarId,R,def,_), GAlloc = unbounded,
	    Code1 = [code{instr:get_value(R1,RY2), regs:[r(TVarId,R1,use,_),r(VarId,RY2,use,_)]}|Code]
	; VarOccDesc = perm_first(Y) ->
	    new_aux_temp(ChunkData1, ChunkData, TVarId),
	    RegDesc = r(TVarId,R,def,_), GAlloc = 0,
	    Code1 = [code{instr:move(R1,Y), regs:[r(TVarId,R1,use,_),r(VarId,Y,perm,_)]}|Code]
	; VarOccDesc = perm_first_in_chunk(Y) ->
	    new_aux_temp(ChunkData1, ChunkData, TVarId),
	    RegDesc = r(TVarId,R,def,_), GAlloc = unbounded,
	    Code1 = [code{instr:get_value(R1,Y), regs:[r(TVarId,R1,use,_),r(VarId,Y,perm,_)]}|Code]
	;
	    unreachable("unify_result")
	).
unify_result(Arg, R, RegDesc, ChunkData0, ChunkData, Code0, Code, 0) :-
	new_aux_temp(ChunkData0, ChunkData1, TVarId),
	RegDesc = r(TVarId,R,def,_),
	head(TVarId, Arg, ChunkData1, ChunkData, Code0, Code).


%
% Generate code for the unification of two variables. Each variable is in one of
% the following states, according to its Variable Occurrence Descriptor:
%
%	tmp
%	tmp_first
%	void
%	perm(y(Y))
%	perm_first(y(Y))
%	perm_first_in_chunk(y(Y))
%

unify_variables(VarOccDesc1, VarId1, VarOccDesc2, VarId2, Code, Code0, GAlloc) :-
	% order the two descriptors, so we need only half a matrix below
	( VarOccDesc1 @> VarOccDesc2 ->
	    unify_variables_ord(VarOccDesc2, VarId2, VarOccDesc1, VarId1, Code, Code0, GAlloc)
	;
	    unify_variables_ord(VarOccDesc1, VarId1, VarOccDesc2, VarId2, Code, Code0, GAlloc)
	).

% PRE: VarOccDesc1 @=< VarOccDesc2
:- mode unify_variables_ord(+,+,+,+,-,?,-).
unify_variables_ord(tmp, VarId1, tmp, VarId2, Code, Code0, unbounded) :- !,
	Code = [code{instr:get_value(R1,R2), regs:[r(VarId1,R1,use,_),r(VarId2,R2,use,_)]}
		|Code0].
unify_variables_ord(tmp, VarId1, tmp_first, VarId2, Code, Code0, 0) :- !,
	Code = [code{instr:move(R1,R2), regs:[r(VarId1,R1,use,_),r(VarId2,R2,def,_)]}
		|Code0].
unify_variables_ord(tmp, _VarId1, void, _VarId2, Code, Code, 0) :- !.
unify_variables_ord(tmp, VarId1, perm(_), VarId2, Code, Code0, unbounded) :- !,
	Code = [code{instr:get_value(R1,RY2), regs:[r(VarId1,R1,use,_),r(VarId2,RY2,use,_)]}
		|Code0].
unify_variables_ord(tmp, VarId1, perm_first(Y2), VarId2, Code, Code0, 0) :- !,
	Code = [code{instr:move(R1,Y2), regs:[r(VarId1,R1,use,_),r(VarId2,Y2,perm,_)]}
		|Code0].
unify_variables_ord(tmp, VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0, unbounded) :- !,
	Code = [code{instr:get_value(R1,Y2), regs:[r(VarId1,R1,use,_),r(VarId2,Y2,perm,_)]}
		|Code0].

unify_variables_ord(tmp_first, VarId1, tmp_first, VarId2, Code, Code0, 1) :- !,
	Code = [code{instr:put_variable(R1), regs:[r(VarId1,R1,def,_)]},
		code{instr:move(R11,R2), regs:[r(VarId1,R11,use_a,_),r(VarId2,R2,def,_)]}
		|Code0].
unify_variables_ord(tmp_first, VarId1, void, _VarId2, Code, Code0, 1) :- !,
	Code = [code{instr:put_variable(R1), regs:[r(VarId1,R1,def,_)]}
		|Code0].
unify_variables_ord(tmp_first, VarId1, perm(Y2), _VarId2, Code, Code0, 0) :- !,
%	Code = [code{instr:move(RY2,R1), regs:[r(_VarId2,RY2,use,_),r(VarId1,R1,def,_)]}|Code0].
	Code = [code{instr:nop, regs:[r(VarId1,Y2,perm,_)]} |Code0].
unify_variables_ord(tmp_first, VarId1, perm_first(Y2), VarId2, Code, Code0, 1) :- !,
	Code = [code{instr:put_global_variable(R1,Y2), regs:[r(VarId2,Y2,perm,_),r(VarId1,R1,def,_)]}
		|Code0].
unify_variables_ord(tmp_first, VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0, 0) :- !,
%	Code = [code{instr:move(Y2,R1), regs:[r(VarId2,Y2,perm,_),r(VarId1,R1,def,_)]}|Code0].
	Code = [code{instr:nop, regs:[r(VarId1,Y2,perm,_),r(VarId2,Y2,perm,_)]} |Code0].

unify_variables_ord(void, _VarId1, void, _VarId2, Code, Code, 0) :- !.
unify_variables_ord(void, _VarId1, tmp, _VarId2, Code, Code, 0) :- !.
unify_variables_ord(void, _VarId1, tmp_first, VarId2, Code, Code0, 1) :- !,
	Code = [code{instr:put_variable(R2), regs:[r(VarId2,R2,def,_)]}|Code0].
unify_variables_ord(void, _VarId1, perm(_), _VarId2, Code, Code, 0) :- !.
unify_variables_ord(void, _VarId1, perm_first(Y2), VarId2, Code, Code0, 1) :- !,
	Code = [code{instr:put_global_variable(Y2), regs:[r(VarId2,Y2,perm,_)]}
		|Code0].
unify_variables_ord(void, _VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0, 0) :- !,
	Code = [code{instr:nop, regs:[r(VarId2,Y2,perm,_)]} |Code0].

unify_variables_ord(perm(_Y1), VarId1, perm(_Y2), VarId2, Code, Code0, unbounded) :- !,
	Code = [code{instr:get_value(RY1,RY2), regs:[r(VarId1,RY1,use,_),r(VarId2,RY2,use,_)]}
		|Code0].
unify_variables_ord(perm(_Y1), VarId1, perm_first(Y2), VarId2, Code, Code0, 0) :- !,
	Code = [code{instr:move(RY1,Y2), regs:[r(VarId1,RY1,use,_),r(VarId2,Y2,perm,_)]}
		|Code0].
unify_variables_ord(perm(_Y1), VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0, unbounded) :- !,
	Code = [code{instr:get_value(RY1,Y2), regs:[r(VarId1,RY1,use,_),r(VarId2,Y2,perm,_)]}
		|Code0].

unify_variables_ord(perm_first(Y1), VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0, 0) :- !,
	Code = [code{instr:move(Y2,Y1), regs:[r(VarId2,Y2,perm,_),r(VarId1,Y1,perm,_)]}
		|Code0].
unify_variables_ord(perm_first(Y1), VarId1, perm_first(Y2), VarId2, Code, Code0, 1) :- !,
	Code = [code{instr:put_global_variable(Y1), regs:[r(VarId1,Y1,perm,_)]},
		code{instr:move(Y1,Y2), regs:[r(VarId2,Y2,perm,_)]}
		|Code0].

unify_variables_ord(perm_first_in_chunk(Y1), VarId1, perm_first_in_chunk(Y2), VarId2, Code, Code0, unbounded) :- !,
	Code = [code{instr:get_value(Y1,Y2), regs:[r(VarId1,Y1,perm,_),r(VarId2,Y2,perm,_)]}
		|Code0].



%
% Implementation of one way unification (head matching)
% The Arg1 ?= Arg2 goal is only created during head normalisation,
% so only certain special cases occur.
%
% p(nonvar) ?- ...	was normalised into p(T) :- T ?= nonvar, ...
%	and T ?= nonvar implemented via specialised head unification code
%
% p(X,X) ?- ...		was normalised into p(X,T) :- X=X, X==T, ...
%
% p(X{A}) ?- ...	was normalised into p(X) :- X?=X{A}, ...
% p(X{A},X{A}) ?- ...	was normalised into p(X,T) :- X?=X{A}, T==X, ...
%
generate_in_unify(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code) :-
	Arg1 = variable{varid:VarId},
	( Arg2 = variable{} ->
	    verify false
    	;
	    variable_occurrence(Arg1, ChunkData0, ChunkData1, Code0, Code1, VarOccDesc),
	    verify VarOccDesc == tmp,
	    in_head(VarId, Arg2, ChunkData1, ChunkData, Code1, Code)
	).


%
% Identity ==/2
% implement some cases via in_get_xxx instructions, otherwise fail
%

generate_identity(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code) :-
	atomic(Arg1),
	Arg2 = variable{},
	!,
	generate_identity(Arg2, Arg1, ChunkData0, ChunkData, Code0, Code).
generate_identity(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code) :-
	Arg1 = variable{varid:VarId},
	atomic(Arg2),
	!,
	put_variable(Arg1, ChunkData0, ChunkData, Code0, Code1),
	Code1 = [code{instr:Instr,regs:[r(VarId,RI,use_a,_)]}|Code],
	in_get_const(RI, Arg2, Instr).
generate_identity(Arg1, Arg2, ChunkData0, ChunkData, Code0, Code) :-
	atomic(Arg1), atomic(Arg2),
	ChunkData0 = ChunkData,
	( Arg1 = Arg2 ->
	    Code0 = Code
	;
	    Code0 = [code{instr:failure,regs:[]}|Code]
	).


