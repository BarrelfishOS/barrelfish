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
 *	$Id: opium_module.pl,v 1.1 2006/09/23 01:54:35 snovello Exp $
 *
 */

/*
 *  predicates which have to be compiled into each new opium module
 */

opium_module.	% to announce that the current module is an opium module

% for Mod:Pred/Arity
:- op(400 , xfy, ':').

% for f_get and b_get
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

/* the following predicates are system predicates in Sepia, so  
 * they cannot be redefined globally but they have to be imported
 * explicitely by every opium module
 */
:- import
	(traceable)/1,
	(untraceable)/1,
	(skipped)/1,
	(unskipped)/1,
	(spy)/1,
	(nospy)/1,
	no_trace/0,
	(listing)/1,
	(ls)/1
   from opium_kernel.



