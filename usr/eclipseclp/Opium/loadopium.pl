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
 *	$Id: loadopium.pl,v 1.1 2006/09/23 01:54:33 snovello Exp $
 *
 */

/*
 *  boot file for Opium
 */

/* to avoid seeing the singleton variable checkings */
:- set_flag(variable_names, on).

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

opium_module.


/* mandatory for bootstrapping */

/*  to avoid that file is dumped in compiled query
 */
mycompile(F) :-
	compile(F).

:- get_opium_file("opium", F), mycompile(F).
:- get_opium_file("error", F), mycompile(F). 
:- get_opium_file("interface", F), mycompile(F).
:- get_flag(extension,kegi_xview) ->
	get_opium_file("interface_pce", F), mycompile(F) ; true.
:- get_opium_file("kernel", F), mycompile(F).
:- get_opium_file("scenario_handler", F), mycompile(F).
:- get_opium_file("make", F), mycompile(F).	
:- get_opium_file("parameter", F), mycompile(F). 
:- get_opium_file("scenario", F), mycompile(F). 
:- get_opium_file("translate", F), mycompile(F).
:- get_opium_file("types", F), mycompile(F).	


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


/*  --------------------------------------------------------
 *   boot opium
 *   make all the scenarios which should be loaded in Opium
 *  --------------------------------------------------------
 */

boot_opium(WUI) :-
	get_opiumdir(SDS),
	append_strings(SDS, "opiumfiles/", ODS),
	atom_string(SD, SDS),
	atom_string(OD, ODS),
	TracGlobal = [active, traceable, global],
	UntrGlobal = [active, untraceable, global],
	TracLocal  = [active, traceable, local],
	UntrLocal  = [active, untraceable, local],
	/* scenario_handler has to be laoded first */
	make(scenario_handler,	opium_kernel, 	UntrGlobal, SD, OD),
	make(kernel,		opium_kernel, 	UntrGlobal, SD, OD),
	make(source, 		opium_kernel, 	UntrGlobal, SD, OD),
	make(display, 		opium_kernel, 	UntrGlobal, SD, OD),
	/* interface scenario requires help scenario */
	make(help, 		opium,		UntrGlobal, SD, OD),
	make(interface,		opium_kernel, 	UntrGlobal, SD, OD),
	(  WUI == pce
	-> /* libraries required to compile the pce files */
	   (  get_flag(extension, development) 
	   -> lib(kegi, kegi),
              lib(k_pce, kegi)
	   ;  true),
	   compile(pce_new, kegi),
	   make(interface_pce,	opium_kernel, 	UntrGlobal, SD, OD)
	;  
	   true
	),
	make(step_by_step, 	opium,		UntrGlobal, SD, OD),
	make(zooming, 		opium, 		UntrGlobal, SD, OD),
	make(ancestor,		opium,		UntrGlobal, SD, OD),
	make(conditional_spypoints, opium,	UntrGlobal, SD, OD),
	make(lint,		opium,		UntrGlobal, SD, OD),
	make(loop,		loop,		UntrGlobal, SD, OD),
	make(abstracts,		abstracts,	UntrGlobal, SD, OD),
	make(failure,		abstracts,	UntrGlobal, SD, OD),
	make(lo,		lo,		UntrGlobal, SD, OD).

:- get_flag(extension,kegi_xview) -> boot_opium(pce) ; boot_opium(tty).
