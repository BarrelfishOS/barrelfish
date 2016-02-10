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
 * $Id: eg_cc_fail_loop.cc,v 1.2 2012/02/25 13:47:56 jschimpf Exp $
 *
 *
 * IDENTIFICATION:	fail_loop.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *	Example of minimal main useing external embed interface.
 */

#include	"eclipseclass.h"
#include	<iostream>


int
main(int argc,char ** argv)
{
    ec_init();
    EC_atom fail = EC_atom("fail");
    EC_ref X;
    EC_ref Start;
    long num;


    /* p(X) has infinite solutions, each solution gives a different
     * value of X
     */
    ec_exec_string("compile_term([p(1),(p(N) :- N is p+1)])",0);

    /* Fail loop */
    post_goal(term(EC_functor("p",1),X));
    while(EC_succeed == EC_resume(Start))
    {
	/* on each iteration X is instantiated to a different number */
    	if (EC_succeed == ((EC_word)X).is_long(&num))
	{
	    std::cout << "p(" << num << ")\n";

	    /* at 10 we want to exit the loop, cutting away other choices */
	    if(num == 10)
		Start.cut_to();
	}

	post_goal(fail);
    }

    ec_cleanup();
    exit(0);
}

