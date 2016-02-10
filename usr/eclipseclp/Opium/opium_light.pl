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
    Currently the scenarios only work in opium_kernel whose module name
    is hard-coded in a number of places that I have no time to update
    properly. [MD]

    This will have to be fixed at some point !

*/
:- module(opium_kernel).


/* to avoid seeing the singleton variable checkings */
:- set_flag(variable_names, on).
/* suppress compiler warnings used-undefined and declared-undefined */
:- set_error_handler(76, true/0).
:- set_error_handler(77, true/0).

:- import set_opium_level/1 from sepia_kernel. 

:- get_flag(prolog_suffix, S), set_flag(prolog_suffix, [".op" | S]).

/* to initialize module opium_kernel (not quite the same as opium_module.pl) */
:- op(400, xfy, ':').
:- op(500, fx, =).
:- op(500, fx, <).
:- op(500, fx, =<).
:- op(500, fx, >).
:- op(500, fx, >=).

:- dynamic
        opium_command/10,
        opium_parameter/8,
        opium_primitive/7,
        opium_procedure/6,
        opium_scenario/6,
        opium_type/4,
        opium_demo/5,
        autoload_command/2,
        autoload_scenario/4.


:- dynamic
        opium_command/9,
        opium_parameter/6,
        opium_primitive/6,
        opium_procedure/5,
        opium_scenario/4,
        opium_type/3,
        opium_demo/3,
        opium_demo/4.

opium_module.


/* mandatory for bootstrapping */

/*  to avoid that file is dumped in compiled query
 */
mycompile(F) :-
        compile(F).



/*
 *  link commands/procedures to implementations to enable the bootstrapping 
 *  before the scenario handler links together commands/procedures and their
 *  implementations
 */

make(S, MOD, OL, SD, OD) :- make_scenario_Op(S, MOD, OL, SD, OD).

opium_scenario_in_module(S, M) :- opium_scenario_in_module_Op(S, M).

set_default_parameters_in_module(S, Mod) :- set_default_parameters_in_module_Op(S, Mod).

check_arg_type(X, Y, Z, T, M) :- check_arg_type_Op(X, Y, Z, T, M).

check_arg(X, Y, Z, T, M) :- check_arg_Op(X, Y, Z, T, M).

modify_time(F, T) :- modify_time_Op(F, T).

is_list(X) :- is_list_Op(X).
is_list_of_atoms(X) :- is_list_of_atoms_Op(X).
is_list_of_atoms_or_empty_list(X) :- is_list_of_atoms_or_empty_list_Op(X).
is_list_of_vars_or_empty_list(X) :- is_list_of_vars_or_empty_list_Op(X).
is_list_of_ports(X) :- is_list_of_ports_Op(X).
is_opium_declaration(P/A) :- is_opium_declaration_Op(P/A).
is_opium_module(M) :- is_opium_module_Op(M).
opium_module(M) :- opium_module_Op(M).

interface_status(X) :- interface_status_Op(X).

opium_write(V, M) :- opium_write_Op(V, M).
opium_printf(V, F, A) :- opium_printf_Op(V, F, A).
opium_printf(V, F, A, S) :- opium_printf_Op(V, F, A, S).
opium_nl(V) :- opium_nl_Op(V).


/* 
 *  get_opium_file(+File, -OpFile)
 *  determines full name of File according to opiumdir
 */
get_opium_file(File, OpFile) :-
	get_opiumdir(Dir),
	get_op_file(File, Dir, OpFile).

get_op_file(File, Dir, OpFile) :-
	concat_string([Dir, "/", File], OpFile),
	exists(OpFile),
	!.
get_op_file(File, Dir, OpFile) :-
	get_flag(prolog_suffix, Suff),
	member(S, Suff),
	concat_string([Dir, "/", File, S], OpFile),
	exists(OpFile),
	!.


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

opium_level(0).

make_here(Scenario, Module, OptionList) :-
	getcwd(CWDS),
        concat_strings(CWDS, "opiumfiles/", ODS),
        atom_string(OD, ODS),
        atom_string(CWD, CWDS),
	make(Scenario, opium_kernel, OptionList, CWD, OD).


/*
:- compile('/homes/js10/public/Opium/opium.sd', opium).
*/

:- compile('error.op',                 opium_kernel),
   compile('scenario_handler.op',      opium_kernel),
   compile('make.op',                  opium_kernel),
   compile('parameter.op',             opium_kernel),
   compile('scenario.op',              opium_kernel),
   compile('translate.op',             opium_kernel),
   compile('types.op',                 opium_kernel),
   compile('interface.op',             opium_kernel).



/* The translate.op file has been changed 
   The other files from the same scenario have not.

   They are currently symbolically linked to Joachim's public/Opium
   directory (scenario_handler.op, make.op, parameter.op, scenario.op,
   autoload.op, error.op, translate.op, types.op.)  
*/

:- make_here(scenario_handler, opium_kernel, [active, untraceable, global]).

/* 
	Patch for opium_light (overwrites part of translate.op). 
	Print_line cannot be executed after a trace
	command which actually quits opium_light.

  */

build_tracing_command(Load, Name, Name_np) :- 
	opium_assert(Load, (Name :- trace_first_line(1), Name_np)).

build_tracing_command_with_arguments(Load, Cmd1, ArgList, ArgNameList, TypeList, Name_np) :-
	opium_assert(Load,
		(Cmd1 :- 
			check_arg_type(ArgList, ArgNameList, TypeList, NewList),
			Cmd2 =.. [Name_np | NewList],
			trace_first_line(1),
			Cmd2)).




:-  make_here(help, opium_kernel, [active, traceable, global]).

:- make_here(display, opium_kernel, [active, traceable, global]).


:- compile('opium_light_kernel.op', opium_light_kernel).



% needed at least to run demos
:- make_here(interface, opium_kernel, [active, traceable, global]).

% this scenario is not entirely working yet because of remote calls
:- make_here(source, opium_kernel, [active, traceable, global]).

:- make_here(lint, opium_kernel, [active, traceable, global]).

:- make_here(step_by_step, opium_kernel, [active, traceable, global]).

:- make_here(opium_light_kernel, opium_kernel, [active, untraceable, global]).


/*	

	Patch for Opium_light.

	Redefines a number of implementations (*_Op predicates) of the kernel

*/

:- compile('opium_light_kernel_patch.pl', opium_kernel).

:- make_local_array(opiumdir).

:-	getcwd(D),
	setval(opiumdir, D).

% :- (extension(development) ->
%	getcwd(D),
%	setval(opiumdir, D)
%   ;
%	get_flag(installation_directory, SD),
%	concat_strings(SD, "/opium/", OD),
%	setval(opiumdir, OD)
%   ).

get_opiumdir(D) :-
	getval(opiumdir, D).

:- compile('opium.pl', opium_kernel).


remote_call(G) :- call(G).
remote_call(G,M) :- call(G,M).

remote_once(G) :- call(G), !.
remote_once(G,M) :- call(G,M), !.

remote_call_all(G) :- findall(G,G, L), !, member(G,L).

remote_call_all(G,M) :- call((findall(G,G, L), !, member(G,L)), M).
