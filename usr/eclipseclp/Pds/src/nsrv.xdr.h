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
**        File: nsrv.xdr.h
**      Author: Kees Schuerman
**      SccsId: "@(#)nsrv.xdr.h	1.2 3/27/95"
** Description: Nsrv XDR Interface
***********************************************************************/

#ifndef _NSRV_XDR_H_
#define _NSRV_XDR_H_


#define xdr_nsrv_ret		xdr_pds_ret


#if defined(__STDC__)
extern bool_t xdr_nsrv_version(XDR * xdrs, 
			       nsrv_version_t * version);
extern bool_t xdr_nsrv_name(XDR * xdrs, 
			    char * * name);
#else /* __STDC__ */
extern bool_t xdr_nsrv_version();
extern bool_t xdr_nsrv_name();
#endif /* __STDC__ */


#endif /* _NSRV_XDR_H_ */

