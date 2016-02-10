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
 *	$Id: pce_new.pl,v 1.1 2006/09/23 01:54:35 snovello Exp $
 *
 */

/*
	redefinition of kegi primitives for more general use

	When it is working try to have them included inside kegi ?
*/

pce_look:-
	is_running("PCE"),		%to avoid 2d conflicts
	block(pce_collect, Tag, pce_catch(Tag)).

pce_catch(send_error) :-
	opium_write(error, 'error in sending something to pce'),
	reset_pce.
pce_catch(read_error) :-
	reset_pce.
pce_catch(Tag) :-
	reset_pce,
	exit_block(Tag).

pce_collect:-
	pce_has_data, !,
	pce_read(message(Object, Behaviour, Value)),
	untraced_call( handle_pce_message(Object, Behaviour, Value), kegi),
	pce_collect.
pce_collect:-
	reset_pce.


/*
	This is a patch to enable reading (most of) the syntax of PCE
which is different from Sepia's.
	
	The '\' character is not a quote character in PCE. If set to a
normal chracter then the inserted quotes have to be doubled as it is
sent by PCE.

	Normally there should not be other characters causing problem
here (?!).

*/

pce_read(X) :-
	get_chtab(0'\, CurrentSlash),
	set_chtab(0'\, symbol),
	(   read(X, pce_msg) 
	->  set_chtab(0'\, CurrentSlash),
	    !
	;   set_chtab(0'\, CurrentSlash),
	    exit_block(read_error)).
	    

/*
	This is a wrapper for send/2 and send/3 defined by Phil
	So that included quotes aren't backslashed

	### The exit_blocks may be too strong.
*/
:- global 
	send_to_pce/2, 
	send_to_pce/3, 
	get_from_pce/3.

send_to_pce(X,Y) :-
	get_chtab(0'\, CurrentSlash),
	set_chtab(0'\, symbol),
	(   send(X, Y) 
	->  set_chtab(0'\, CurrentSlash),
	    !
	;   set_chtab(0'\, CurrentSlash),
	    !,
	    fail).


send_to_pce(X,Y, Z) :-
	get_chtab(0'\, CurrentSlash),
	set_chtab(0'\, symbol),
	(   send(X, Y, Z) 
	->  set_chtab(0'\, CurrentSlash),
	    !
	;   set_chtab(0'\, CurrentSlash),
	    !,
	    fail).

	

get_from_pce(X,Y, Z) :-
	get_chtab(0'\, CurrentSlash),
	set_chtab(0'\, symbol),
	(   get(X, Y, Z) 
	->  set_chtab(0'\, CurrentSlash),
	    !
	;   set_chtab(0'\, CurrentSlash),
	    exit_block(send_error)).

	

