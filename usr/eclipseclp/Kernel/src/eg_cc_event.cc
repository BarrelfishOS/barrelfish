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
 * DESCRIPTION:
 *	Example of posting events to ECLiPSe
 */

#include	"eclipseclass.h"
#include	<iostream>
#include	<signal.h>
#include	<unistd.h>

void
handle_alarm(int i)
{
    post_event(EC_atom("abort"));
}


int
main(int argc,char ** argv)
{
    ec_init();
    int res;

    signal(SIGALRM, handle_alarm);

    alarm(1);	/* or setitimer(...) */

    post_goal("between(1,99999,1,X), writeln(X), fail");
    switch (EC_resume())
    {
    case EC_succeed:	std::cout << "Succeeded\n"; break;
    case EC_fail:	std::cout << "Failed\n"; break;
    case EC_throw:	std::cout << "Aborted\n"; break;
    default:		std::cout << "???\n"; break;
    }

    ec_cleanup();
    exit(0);
}

