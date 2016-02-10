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
% Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH.
% 
% END LICENSE BLOCK

:- module_interface(wake).

:- export
	default_wake/0,
	get_first_suspension/1,
	get_suspension_counter/1,
	set_first_suspension/1,
	set_suspension_counter/1,
	suspension_mark/3,
	trace_wake/0.
    
:- begin_module(wake).

:- import
	trace_propagation/4,
	trace_suspension/4,
	get_parent/1,
	set_parent/1
    from grace.

:- import
	call_suspension/1,
	first_woken/2,
	get_flag_body/4,
	get_priority/1,
	get_suspension_number/2,
	last_scheduled/1,
	last_suspension/1,
	new_scheduled/2,
	new_suspensions/2,
	set_priority/1,
	set_suspension_number/2
    from sepia_kernel.

:- local
	wake/0.

:- make_local_array(suspension_counter),
   make_local_array(first_suspension).

trace_wake :-
    get_flag(wake/0, visibility, local) ->
	call(export(wake/0), sepia_kernel),
	global(wake/0)
    ;
	true.

default_wake :-
    get_flag_body(wake/0, visibility, exported, sepia_kernel) ->
	local(wake/0),
	call(global(wake/0), sepia_kernel)
    ;
	true.

wake :-
    get_priority(Prio),
    wake_loop(Prio).

wake_loop(Prio) :-
    (first_woken(Prio, Susp) ->
	trace_and_call(Susp),
	wake_goals(Prio)
    ;
	set_priority(Prio)
    ).

wake_goals(Prio) :-
    (first_woken(Prio, Susp) ->
	trace_and_call(Susp),
	wake_goals(Prio)
    ;
	set_priority(Prio)
    ).

trace_and_call(Susp) :-
    suspension_to_goal(Susp, Goal, Module),
    (Module = grace ->
	% our goal
	call_suspension(Susp)
    ;
	trace_suspension('CALL', Goal, Mark, Module),
	(call_suspension(Susp) ->
	    trace_suspension('EXIT', Goal, Mark, Module)
	;
	    trace_suspension('**FAIL', Goal, Mark, Module),
	    fail
	)
    ).

set_first_suspension(_).
set_suspension_counter(_).
