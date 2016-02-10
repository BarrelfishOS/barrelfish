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
**        File: pds.mem.c
**      Author: Kees Schuerman
**      SccsId: "@(#)pds.mem.c	1.5 24 Nov 1995"
** Description: Memory and Interrupt System
***********************************************************************/

#include "machine.h"    /* architecture specific constant definitions */

#include "pds.types.h"
#include "pds.mem.h"

#define PDS_MEM_SIZE_MIN 	0x100000
#define PDS_MEM_SIZE_INCR 	(PDS_MEM_SIZE_MIN / 8)


void
pds_int_init(handler)
    void (* handler) ();
{
    irq_lock_init(handler);
}


char *
pds_mem_init(file,address,size,descriptor,option)
    char * file;
    char * address;
    pds_size_t size;
    pds_heap_descriptor_t * descriptor;
    unsigned option; 
{
    if (file && (file[0] == '\0'))
	file = (char *) 0;
    if (option && (size < PDS_MEM_SIZE_MIN))
	size = PDS_MEM_SIZE_MIN;
    return(shared_mem_init(option,file,address,size,
			   PDS_MEM_SIZE_INCR,0,descriptor));
}


