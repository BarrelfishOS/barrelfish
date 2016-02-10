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
**        File: bmsg.xdr.h
**      Author: Kees Schuerman
**      SccsId: "@(#)bmsg.xdr.h	1.6 14 Nov 1995"
** Description: XDR Primitives API
***********************************************************************/

#ifndef _BMSG_XDR_H_
#define _BMSG_XDR_H_


#define xdr_bport_id			xdr_pds_word
#define xdr_bdomain_id			xdr_pds_word
#define xdr_bport_number		xdr_pds_u_int16
#define xdr_bpid			xdr_pds_word
#define xdr_bmem_size			xdr_pds_size
#define xdr_bmsg_size			xdr_pds_size
#define xdr_bmsg_address		xdr_pds_double_word
#define xdr_bmem_id			xdr_pds_word
#define xdr_bmsg_ret			xdr_pds_ret


#if defined(__STDC__)
extern bool_t xdr_bnet_address(XDR * xdrs,
			       char * * address);
extern bool_t xdr_bport(XDR * xdrs,
			bport_t * port);
extern bool_t xdr_bdomain(XDR * xdrs,
			  bdomain_t * domain);
#else /* __STDC__ */
extern bool_t xdr_bnet_address();
extern bool_t xdr_bport();
extern bool_t xdr_bdomain();
#endif /* __STDC__ */


#endif /* _BMSG_XDR_H_ */

