/*
 * BEGIN LICENSE BLOCK
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
 *
 *	$Id: opium.pl,v 1.1 2006/09/23 01:54:34 snovello Exp $
 *
 */


/*  
    !!!!!!!!!!!!!!!!!

    THIS FILE SHOULD ACTUALLY BE SPLIT INTO TWO PIECES
    - ONE WITH THE COMMON PARTS BETWEEN OPIUM AND OPIUM LIGHT
    - ONE WITH THE PARTICULARITIES OF EACH
*/



/*
 *   everything which has to be defined in Opium but which is
 *   not part of an Opium scenario
 */

%:- import opium_level/1  from sepia_kernel.
:- import fd_open/3 	 from sepia_kernel.

/* set print_depth */
:- set_flag(print_depth, 1000).  % for writing lists and terms


/*  -----------------------
 *   init system variables
 *  -----------------------
 */

:- make_local_array(opium_loop_running).
% :- setval(opium_loop_running, 0).

:- make_local_array(tracing).
% :- setval(tracing, 0).

%% when opium_light is called there is necessarily an opium_loop_running !
:- setval(opium_loop_running, 1), setval(tracing, 1).


/*  ----------------------------------
 *   initialize toplevel module opium
 *  ----------------------------------
 */
% :- get_opium_file("opium_module", File), 
%    compile(File, opium).



/*  ---------------------------------------------------------
 *   definition of MU-Prolog builtins not available in Sepia
 *  ---------------------------------------------------------
 */

:- global sprintf/3.
:- global namevar/2.

% :- skipped sprintf/3.
% :- skipped namevar/2.

/*
 *  sprintf/3 
 *  the formatted string is converted to an atom an 
 *  instantiated to the first parameter
 */
sprintf(Atom, Format, List) :-
	open(_, string, Stream),
	printf(Stream, Format, List),
	current_stream(String, _, Stream),
	atom_string(Atom, String),
	close(Stream).

/*
 *  namevar/2
 *  returns the name of a sepia variable as atom
 */
namevar(V, VN) :-
	var(V),
	open(_, string, Stream),
	printf(Stream, "%QDw", [V]),
	current_stream(S, _, Stream),
	atom_string(VN, S),
	close(Stream).


/*  -----------------------------------------------------
 *   opium_error_handler/3
 *   common declaration for all the error handlers which
 *   are modified in Opium
 *  -----------------------------------------------------
 */

opium_error_handler(68, Goal, Module) :-	% undefined procedure called
	to_be_autoloaded(Goal, Module, NewGoal),
	!,
	call(NewGoal, Module).	
opium_error_handler(68, Goal, Module) :-
	error(default(68), Goal, Module).
opium_error_handler(134, Goal, Module) :-	% not consecutive clauses
	is_opium_obj(Goal),
	!,
	fail.
opium_error_handler(134, Goal, Module) :-
	error(default(134), Goal, Module).

is_opium_obj(opium_scenario/4).
is_opium_obj(opium_command/9).
is_opium_obj(opium_parameter/7).
is_opium_obj(opium_primitive/6).
is_opium_obj(opium_procedure/5).
is_opium_obj(opium_type/3).
is_opium_obj(opium_demo/4).

:- set_error_handler(134, opium_error_handler/3).
%:- set_error_handler(68,  opium_error_handler/3).  	% Opium's autoload 


/*  -------------------------------
 *   re-definition of Sepia events
 *  -------------------------------
 */

/*
 *  banner printed in Opium session
 */
% :- dynamic boot_date/1.
% :- date(D), assert(boot_date(D)).
% 
% opium_banner(_, Sepiabanner) :-
% 	write(toplevel_output, "SEPIA + Opium 3.1  "),
% 	boot_date(D),
% 	substring(D, 1, 10, D1),
% 	substring(D, 20, 5, D2),
% 	concat_strings(D1, D2, DD),
% 	write(toplevel_output, DD),
 *
% 	flush(toplevel_output).
% 
% :- set_error_handler(164, opium_banner/2).


/* 
 *  re-definition of opium_answer/2
 *  defined in ~/sepia/workdir/sepia/pl/boot_bips.pl
 */
opium_answer(_, yes).
opium_answer(_, no) :-
	write(toplevel_output, 'no.\n').
opium_answer(_, no_answer) :-
	write(toplevel_output, 'no (more) solution.\n').
opium_answer(_, last_yes).
opium_answer(_, last_answer) :-
	write(toplevel_output, '\n').
opium_answer(_, more_answers) :-
	write(toplevel_output, '     More? (;) '),
	flush(toplevel_output),
	tyi(toplevel_input, C),
	(C == 0'; ->
		write(toplevel_output, '\n'),
		fail
	;
		write(toplevel_output, '\n')
	).

:- set_error_handler(156, opium_answer/2).


/*
 *  to get number of break level in toplevel prompt
 */
% opium_toplevel_prompt(_, Module) :-
% 	get_prompt(toplevel_input, _, Out),
% 	write(Out, "["),
% 	write(Out, Module),
% 	get_flag(break_level, L),
% 	(L > 0 ->
% 		write(Out, " "),
% 		write(Out, L)
% 	;
% 		true
% 	),
% 	write(Out, "]: "),
% 	flush(Out).		
% 
% :- set_error_handler(153, opium_toplevel_prompt/2).




/*  --------------------------------------
 *   init_opium
 *   to be executed when Opium is started
 *  --------------------------------------
 */

%%% REPLACEMENT FOR OPIUM_LIGHT

:- setval(opium_level, 1).

% init_opium(X1, X2) :-
% 	set_interrupt_handler(18, true/0),	% disable ^Z in Opium
% 	set_interrupt_handler(2,  true/0),	% disable ^C
% 	argc(NArgs),
% 	NInitGoal is NArgs - 6,
% 	argv(NInitGoal, SInitGoal),
% 	term_string(InitGoal, SInitGoal),
% 	NP is NArgs - 5,
% 	argv(NP, PIDS),
% 	term_string(PID, PIDS),
% 	NL is NArgs - 4,
% 	argv(NL, LevS),
% 	term_string(Lev, LevS),
% 	set_opium_level(Lev),		% set level of current session
% 	NI is NArgs - 3,
% 	argv(NI, FD_Input), 
% 	term_string(I, FD_Input),
% 	fd_open(I, read, from_prolog),	% open pipe to read from Prolog
% 	NO is NArgs - 2,
% 	argv(NO, FD_Output),
% 	term_string(O, FD_Output),
% 	fd_open(O, write, to_prolog),	% open pipe to write to Prolog
% 	NIAsy is NArgs - 1,
% 	argv(NIAsy, FD_InputAsy),
% 	term_string(IAsy, FD_InputAsy),
% 	fd_open(IAsy, read, from_prolog_asynch),
% 	setval(prolog_pid, PID),
% 	set_opium_pid_in_prolog,
% 	initialize_c_parameters,	% system parameters
% 	set_opium(X1, X2),		% opium is toplevel module
% 	call(InitGoal, X2),		% call the init goal
% 	opium_init_file,		% compile ".opiumrc"
% 	set_error_handler(150, true/0),	% no opium_init on re-start
% 	reset_interrupt_handler(2).	% enable ^C
% 
% set_opium_pid_in_prolog :-
% 	get_flag(pid, OpiumPid),
% 	remote_once(setval(opium_pid, OpiumPid), sepia_kernel).

/*
 *  initialize parameters of type 'c'  (cannot be done without traced session)
 */
% initialize_c_parameters :-
% 	set_default_in_module(limit_depth,  opium_kernel),
% 	set_default_in_module(limit_call,   opium_kernel),
% 	set_default_in_module(record_trace, opium_kernel).

/*
 *  get opium as toplevel module
 */
% set_opium(_, opium).

/*
 *  load ./.opiumrc or ~/.opiumrc if you are an Opium session
 */
opium_init_file :-
	opium_level(0),
	!.
opium_init_file :-
	exists('.opiumrc'),
	!,
	compile('.opiumrc', opium).
opium_init_file :- 
	getenv('HOME', X),
	append_strings(X, "/.opiumrc", M),
	atom_string(F, M),
	exists(F),
	!,
	compile(F, opium).
opium_init_file.


% :- set_error_handler(150, init_opium/2).




/*
 * Put the demo directory inside the library path.
 * This is duplicated in the opium_slave.pl as for the development
 *   version of sepium opium_slave.p is not loaded in opium-1 unless a
 *   second opium is explicitely called.
 */
:-	get_opiumdir(OD),
  	concat_strings(OD, "demo", OpiumDemo),
	get_flag(library_path, PathList),
	( member(OpiumDemo, PathList)
	-> true
	; append(PathList,[OpiumDemo], NewPathList), 
	  set_flag(library_path, NewPathList)
	).
