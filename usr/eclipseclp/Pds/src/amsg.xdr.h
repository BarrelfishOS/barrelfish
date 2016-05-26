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
**        File: amsg.xdr.h
**      Author: Kees Schuerman
**      SccsId: "@(#)amsg.xdr.h	1.2 4/6/95"
** Description: Amsg XDR Interface
***********************************************************************/

#ifndef _AMSG_XDR_H_
#define _AMSG_XDR_H_


#define xdr_aport_id		xdr_pds_word
#define xdr_amsg_type		xdr_msg_type
#define xdr_amsg_count		xdr_msg_count
#define xdr_amsg_option		xdr_msg_option


#if defined(__STDC__)
extern bool_t xdr_aport(XDR * xdrs, 
			aport_t * port);
#else /* __STDC__ */
extern bool_t xdr_aport();
#endif /* __STDC__ */


#endif /* _AMSG_XDR_H_ */

