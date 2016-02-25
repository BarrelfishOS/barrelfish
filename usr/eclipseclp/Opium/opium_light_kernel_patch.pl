/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1990,2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Mireille Ducasse, ECRC.
 * 
 * END LICENSE BLOCK
 */
%----------------------------------------------------------------------
%:- module_interface(opium_kernel).
%----------------------------------------------------------------------

/*
:- export
	trace_first_line/1,
	curr_line_Op/5,
	curr_chrono_Op/1,
	curr_port_Op/1,
	curr_call_Op/1,
	curr_depth_Op/1,
	curr_pred_Op/1,
	curr_arg_Op/1,
	curr_arg_Op/2,
	f_get_bare/5,
	no_trace_Op/0.
*/
:- local

	struct(trace_line(port,frame)),
	struct(tf(invoc,goal,depth,chp,parent,proc,module)),

	reference(current),

	variable(autoprint).


:- import
	configure_prefilter/5
   from sepia_kernel.

%:- begin_module(opium_light_kernel).

/* 
	TRACE_FIRST_LINE(+Bool)

	Tells whether the first trace line upon returning to
   opium_light ought to be traced.
   0 means do not trace it
   1 means do trace it

	This is needed, as there is no corouting. Queries such as
   "next,print_line." cannot be executed as opium_light returns to the
   traced execution after fget.

	The autoprint global variable allows the equivalent
   functionality for this particular kind of queries.

*/
trace_first_line(Int) :- 
	setval(autoprint, Int).

/*
	In Opium the very first line of each execution is always
   printed. It is an effective way to tell people that a tracing
   session is starting. 
*/ 
:- trace_first_line(1).


%----------------------------------------------------------------------
% The trace line event handler
%----------------------------------------------------------------------

opium_light(_252, TraceLine) :-
	setval(current, TraceLine),
	( getval(autoprint, 1) ->
	    print_line, 
            /* nl(debug_output), */
	    setval(autoprint, 0)
	;
	    true
	),
	get_error_handler(153, H, M),
	set_error_handler(153, opium_toplevel_prompt/2),
	get_flag(toplevel_module, TM),
	set_flag(toplevel_module, opium),
	break,				% run a nested toplevel
	set_flag(toplevel_module, TM),
	set_error_handler(153, H)@M.

opium_toplevel_prompt(_153, Module) :-
	get_prompt(toplevel_input, _, PromptStream),
	printf(PromptStream, "  *%w*: %b", [Module]).


%----------------------------------------------------------------------
% The curr_... primitives
%----------------------------------------------------------------------

curr_line_Op(_, Call, Depth, Port, M:N/A) :-
	getval(current, TraceLine),
	TraceLine = trace_line with [ port:Port, frame:Frame ],
	Frame = tf with [invoc:Call,goal:Goal,depth:Depth,module:M],
	functor(Goal, N, A).

curr_chrono_Op(_).

curr_port_Op(Port) :-
	getval(current, TraceLine),
	TraceLine = trace_line with port:Port.

curr_call_Op(Call) :-
	getval(current, TraceLine),
	TraceLine = trace_line with frame:(tf with invoc:Call).

curr_depth_Op(Depth) :-
	getval(current, TraceLine),
	TraceLine = trace_line with frame:(tf with depth:Depth).

curr_pred_Op(M:N/A) :-
	getval(current, TraceLine),
	TraceLine = trace_line with frame:Frame,
	Frame = tf with [goal:Goal,module:M],
	functor(Goal, N, A).

curr_arg_Op(ArgList) :-
	getval(current, TraceLine),
	TraceLine = trace_line with frame:(tf with goal:Goal),
	Goal =.. [_|ArgList].

curr_arg_Op(N, Arg) :-
	getval(current, TraceLine),
	TraceLine = trace_line with frame:(tf with goal:Goal),
	arg(N, Goal, Arg).


%----------------------------------------------------------------------
% Some basics
%----------------------------------------------------------------------

%:- tool(f_get_bare_Op/5, f_get_bare_Op_body/6).
f_get_bare_Op(_, Call, Depth, Port, Pred) :-
        curr_pred(Module:_/_),
	configure_prefilter(Call, Depth, Port, Pred, Module),
	exit_block(end).

no_trace_Op :-
	trace_first_line(1),
	f_get_bare_Op(_, _, _,[],_).


%----------------------------------------------------------------------
% Install Opium as the trace event handler
%----------------------------------------------------------------------

:- set_error_handler(252, opium_light/2).

% Suppress the messages when entering and leaving a break level
:- set_error_handler(158, true/0).
:- set_error_handler(159, true/0).

