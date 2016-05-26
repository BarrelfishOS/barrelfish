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
 * $Id: eg_cc_basic.cc,v 1.2 2012/02/25 13:47:56 jschimpf Exp $
 *
 *
 * IDENTIFICATION:	eg_cc_basic.cc
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *	Example of minimal main using string-based embed interface.
 */

#include	"eclipseclass.h"
#include <iostream>

int
main()
{
    char	buf[1024];
    int		n;

    ec_set_option_int(EC_OPTION_IO, MEMORY_IO);
    ec_init();

    ec_post_string("write(hello)");
    ec_resume();

    n = ec_queue_read(1, buf, 1024);
    buf[n] = 0;
    std::cout << "eclipse returned: " << buf << ".\n";

    n = ec_queue_read(2, buf, 1024);
    buf[n] = 0;
    std::cout << "eclipse error returned: " << buf << ".\n";

    ec_cleanup();
    exit(0);
}

