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
 *	$Id: opiumtop.pl,v 1.1 2006/09/23 01:54:34 snovello Exp $
 *
 */

/* 
 *   bootstrapping of Opium scenarios in module opium
 *   (loadfile is loadopium.pl)
 */

:- module(opium_kernel).

:- import extension/1 from sepia_kernel.
:- global get_opium_file/2.	/* for manual/1 */

:- (extension(development) ->
	make_local_array(opiumdir),
	getcwd(D),
	setval(opiumdir, D),
	compile_term([
	    get_opiumdir(D) :-
		    getval(opiumdir, D)
	])
   ;
	(import get_opiumdir/1 from sepia_kernel)
   ).

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
 *  compile Opium loadfile
 */

/*  to avoid that file is dumped in compiled query
 */
mycompile(F) :-
	compile(F).

:- argv(5, LoadF),
   get_opium_file(LoadF, LoadFile), 
   mycompile(LoadFile).










	


