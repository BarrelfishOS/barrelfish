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
 * Copyright (C) 1997-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */


/*
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: eg_cc_main.cc,v 1.2 2012/02/25 13:47:56 jschimpf Exp $
 *
 *
 * IDENTIFICATION:	eg.cc
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 */

#include	"eclipseclass.h"


double my_array[5] = {1.1, 2.2, 3.3, 4.4, 5.5};

int
main()
{
	ec_init();

	EC_ref R;
	EC_functor writeln_1 = EC_functor("writeln",1);
	EC_functor read_1 = EC_functor("read",1);
	EC_word w;

	post_goal(term(writeln_1,"hello world"));
	R= newvar();
	post_goal(term(read_1,R));
	post_goal(term(writeln_1,"hi again"));
	post_goal(term(writeln_1,R));
	w = list(0,list(1,3.0));
	post_goal(term(writeln_1,w));
	post_goal(term(writeln_1,R));
	post_goal(term(writeln_1,w*3+4*R));
	EC_resume();
}


