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
**        File: pds.msgdd.h
**      Author: Kees Schuerman
**      SccsId: "@(#)pds.msgdd.h	1.3 3/16/95"
** Description: Message Data Description
***********************************************************************/

#ifndef _PDS_MSGDD_H_
#define _PDS_MSGDD_H_

/*
** Return Codes
*/

#define MSGDD_OK              PDS_OK            /* success               */
#define MSGDD_NYI             PDS_NYI           /* not yet implemented   */
#define MSGDD_WARN            PDS_WARN          /* general warning       */
#define MSGDD_ERROR           PDS_ERROR         /* general error         */
#define MSGDD_IMPLIM          PDS_IMPLIM        /* implementation limit  */
#define MSGDD_INVAL           PDS_INVAL         /* invalid argument      */
#define MSGDD_NORESOURCES     PDS_NORESOURCES   /* no resources          */
#define MSGDD_NOMEMORY        PDS_NOMEMORY      /* no memory             */


/*
** Message Descriptor Types
*/

#define MDT_BEGIN		 0
#define MDT_STRUCT_OPEN		 1
#define MDT_STRUCT_CLOSE	 2
#define MDT_ARRAY_OF		 3
#define MDT_BYTE		 4
#define MDT_HWORD		 5
#define MDT_WORD		 6
#define MDT_DWORD		 7
#define MDT_ADDRESS		 8
#define MDT_CHAR		 9
#define MDT_UCHAR		10
#define MDT_INT8		11
#define MDT_UINT8		12
#define MDT_INT16		13
#define MDT_UINT16		14
#define MDT_INT32		15
#define MDT_UINT32		16
#define MDT_SP_FLOAT		17
#define MDT_DP_FLOAT		18
#define MDT_END		 	19

#define MDT_SIZE		MDT_UINT32
#define MDT_RET			MDT_INT32


/*
** Type Definitions
*/

typedef pds_ret_t	msgdd_ret_t;
typedef pds_uint32 	msg_type_t;
typedef msg_type_t 	msg_typedef_t;


/*
** Function Declarations
*/

#if defined(__STDC__)
extern msgdd_ret_t msg_type_define(msg_typedef_t * msg_typedef,
		       	           msg_type_t * msg_type);
#else /* __STDC__ */
extern msgdd_ret_t msg_type_define();
#endif /* __STDC__ */


#endif /* _PDS_MSGDD_H_ */
