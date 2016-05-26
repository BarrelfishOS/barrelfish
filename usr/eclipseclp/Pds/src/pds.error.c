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
 * Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Kees Schuerman, ECRC
 * 
 * END LICENSE BLOCK */
/**********************************************************************
**      System: Parallel Distributed System
**        File: pds.error.c
**      Author: Kees Schuerman
**      SccsId: "@(#)pds.error.c	1.4 14 Nov 1995"
** Description: Error Reporting System
***********************************************************************/

#include "machine.h"    /* architecture specific constant definitions */

#include <stdio.h>

#include "pds.types.h"
#include "pds.error.h"


static char * pds_errlist[PDS_NOF_RETS] = {
	"Success !",
	"Not yet implemented !",
	"Warning !",
	"Error !",
	"Implementation limit !",
	"Invalid argument !",
	"Not enough resources !",
	"Not enough memory !",
	"Not ready !"};


void
pds_perror(pret, s)
    pds_ret_t pret;
    char * s;
{
    if ((pret > 0) && (pret < PDS_NOF_RETS)) {
        if (s && s[0] != '\0')
	    fprintf(stderr,"%s: ",s);
        fprintf(stderr, "%s\n", pds_errlist[pret]);
    }
}

