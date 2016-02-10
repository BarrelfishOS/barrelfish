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
**        File: pds.error.h
**      Author: Kees Schuerman
**      SccsId: "@(#)pds.error.h	1.2 8/31/95"
***********************************************************************/

#ifndef _PDS_ERROR_H_
#define _PDS_ERROR_H_


/*
** Return Codes
*/

#define PDS_OK                 0       /* success                 */
#define PDS_NYI                1       /* not yet implemented     */
#define PDS_WARN               2       /* general warning         */
#define PDS_ERROR              3       /* general error           */
#define PDS_IMPLIM             4       /* implementation limit    */
#define PDS_INVAL              5       /* invalid argument        */
#define PDS_NORESOURCES        6       /* no resources            */
#define PDS_NOMEMORY           7       /* no memory               */
#define PDS_NOT_READY          8       /* not ready               */
#define PDS_NOF_RETS	       9
#define PDS_RET_MAX          127


#if defined(__STDC__)
extern void pds_perror(pds_ret_t pret, char * s);
#else /* __STDC__ */
extern void pds_perror();
#endif /* __STDC__ */


#endif /* _PDS_ERROR_H_ */
