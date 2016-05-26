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
 * ECLiPSe Application Example
 *
 * $Id: eg_cc_mip.cc,v 1.2 2012/02/25 13:47:56 jschimpf Exp $
 *
 * IDENTIFICATION:	eg_sendmore.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * DESCRIPTION:
 *	Example of solving a constraint problem from C
 */

#include	"eclipseclass.h"
#include	<iostream>


#define NCOLS	3
#define NROWS	2

double req[NROWS][NCOLS] = {
    2.2, 1.8, 1.9,
    2.4, 2.0, 2.1
};

double pc[NCOLS] = {
    24.7, 22.4, 19.7
};




static void bounds(EC_word vars, double lb, double ub)
{
    post_goal(term(EC_functor("::",2),
	    vars,
	    term(EC_functor("..",2), lb, ub)));
}

static void eq(EC_word lhs, EC_word rhs)
{
    post_goal(term(EC_functor("$=",2), lhs, rhs));
}

static void geq(EC_word lhs, EC_word rhs)
{
    post_goal(term(EC_functor("$>=",2), lhs, rhs));
}

static void leq(EC_word lhs, EC_word rhs)
{
    post_goal(term(EC_functor("$=<",2), lhs, rhs));
}

static void maximize(EC_word obj, EC_word objval)
{
    post_goal(term(EC_functor("optimize",2),
	term(EC_functor("max",1), obj), objval));
}


int
main()
{
    ec_init();
    {

	EC_ref	Profit;
	EC_refs	Vars(NCOLS);
	EC_ref	VarList;

	post_goal("lib(eplex)");

	VarList = list(Vars);

	bounds(VarList, 0.0, 1e20);

	leq( VarList * list(NCOLS,req[0]),	8.0);
	leq( VarList * list(NCOLS,req[1]),	10.0);

	maximize(VarList * list(NCOLS,pc), Profit);

	if (EC_resume() == EC_succeed)		/* solve */
	{
	    double d;
	    int i;

	    if (EC_word(Profit).is_double(&d) == EC_succeed)
		std::cout << "Profit is " << d << "\n";
	    else
		std::cout << "Profit is ?\n";

	    for (i=0; i<NCOLS; i++)
	    {
		if (Vars[i].is_double(&d) == EC_succeed)
		    std::cout << "X" << i << " = " << d << "\n";
		else
		    std::cout << "X" << i << " = ?\n";
	    }
	}
	else std::cout << "No solution\n";
    }
    ec_cleanup();
}

