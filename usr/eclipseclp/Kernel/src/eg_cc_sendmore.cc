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
 * $Id: eg_cc_sendmore.cc,v 1.2 2012/02/25 13:47:56 jschimpf Exp $
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

enum {S,E,N,D,M,O,R,Y};

int
main()
{
	ec_init();
	post_goal("lib(fd)");

	EC_refs X(8);
	EC_ref DigitList;
	DigitList = list(X);

	post_goal(term(EC_functor("::",2),DigitList,
		term( EC_functor("..",2),0,9)));

	post_goal(term(EC_functor("alldistinct",1),DigitList));
	post_goal(term(EC_functor("##",2),0,X[S]));
	post_goal(term(EC_functor("##",2),0,X[M]));
	post_goal(term(EC_functor("#=",2),
			    1000 * X[S] + 100 * X[E] + 10 * X[N] + X[D]
			  + 1000 * X[M] + 100 * X[O] + 10 * X[R] + X[E] ,
	     10000 * X[M] + 1000 * X[O] + 100 * X[N] + 10 * X[E] + X[Y] ));
	post_goal(term(EC_functor("labeling",1),DigitList));
	post_goal(term(EC_functor("writeln",1),DigitList));
	EC_resume();
}


