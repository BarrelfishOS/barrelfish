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
**        File: amsg.xdr.c
**      Author: Kees Schuerman
**      SccsId: "@(#)amsg.xdr.c	1.7 8/31/95"
** Description: Amsg XDR Primitives
***********************************************************************/

#include "machine.h"    /* architecture specific constant definitions */

#include <stdio.h>
#include <rpc/rpc.h>

#include "pds.types.h"
#include "pds.mdt.h"
#include "pds.xdr.h"
#include "bmsg.msg.h"
#include "bmsg.xdr.h"
#include "amsg.msg.h"
#include "amsg.xdr.h"


bool_t 
xdr_aport(xdrs,port)
    XDR * xdrs;
    aport_t * port;
{
    return(xdr_aport_id(xdrs,&port->aport_id) &&
	   xdr_bport_id(xdrs,&port->bport_id) &&
	   xdr_bdomain_id(xdrs,&port->bdomain_id));
}

