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
**      System: BMSG
**        File: bmsg.xdr.c
**      Author: Kees Schuerman
**      SccsId: "@(#)bmsg.xdr.c	1.7 14 Nov 1995"
** Description: XDR Primitives
***********************************************************************/

#include "machine.h"    /* architecture specific constant definitions */

#include <stdio.h>
#include <rpc/rpc.h>

#include "pds.types.h"
#include "pds.mdt.h"
#include "pds.xdr.h"
#include "bmsg.msg.h"
#include "bmsg.xdr.h"


bool_t
xdr_bnet_address(xdrs,address)
    XDR * xdrs;
    char * * address;
{
    return(xdr_vector(xdrs,*address,BNET_ADDRESSLEN_MAX+1,
	   sizeof(char),(xdrproc_t) xdr_char));
}


bool_t 
xdr_bport(xdrs,port)
    XDR * xdrs;
    bport_t * port;
{
    char * bnet_address = port->bnet_address;

    return(xdr_bpid(xdrs,&port->bpid) &&
	   xdr_bport_id(xdrs,&port->bport_id) &&
	   xdr_bdomain_id(xdrs,&port->bdomain_id) &&
           xdr_pds_address(xdrs,(pds_address_t *)
                                ((char *) &port->bmsg_queue_address +
					  PDS_ADDR_OFFSET)) &&
	   xdr_bnet_address(xdrs,&bnet_address) &&
	   xdr_bport_number(xdrs,&port->bport_number));
}


bool_t 
xdr_bdomain(xdrs,domain)
    XDR * xdrs;
    bdomain_t * domain;
{
    char * bdomain_file = domain->bdomain_file;

    return(xdr_bdomain_id(xdrs,&domain->bdomain_id) &&
	   xdr_string(xdrs,&bdomain_file,BMSG_FILENAMELEN_MAX) &&
	   xdr_pds_address(xdrs,(pds_address_t *) 
				((char *) &domain->bdomain_start +
					  PDS_ADDR_OFFSET)) &&
	   xdr_bmem_size(xdrs,&domain->bdomain_size));
}


