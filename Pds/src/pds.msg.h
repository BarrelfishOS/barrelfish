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
**        File: pds.msg.h
**      Author: Kees Schuerman
**      SccsId: "@(#)pds.msg.h	1.1 4/6/95"
** Description: Messages 
***********************************************************************/

#ifndef _PDS_MSG_H_
#define _PDS_MSG_H_


/*
** Message Data Type Definition Constructors
*/

#define MDT_BEGIN		 0
#define MDT_END		 	 1
#define MDT_STRUCT_OPEN		 2
#define MDT_STRUCT_CLOSE	 3
#define MDT_ARRAY_OF		 4


/*
** Standard Message Data Types
*/

#define MDT_STD_MIN	 	 5
#define MDT_BYTE		 5
#define MDT_HWORD		 6
#define MDT_WORD		 7
#define MDT_DWORD		 8
#define MDT_ADDRESS		 9
#define MDT_CHAR		10
#define MDT_UCHAR		11
#define MDT_INT8		12
#define MDT_UINT8		13
#define MDT_INT16		14
#define MDT_UINT16		15
#define MDT_INT32		16
#define MDT_UINT32		17
#define MDT_SP_FLOAT		18
#define MDT_DP_FLOAT		19
#define MDT_STD_MAX		19

#define MDT_SIZE		MDT_UINT32
#define MDT_RET			MDT_INT32


/*
** Type Definitions
*/

typedef pds_uint32 	msg_type_t;
typedef msg_type_t 	msg_typedef_t;
typedef msg_typedef_t 	msg_count_t;
typedef pds_byte_t 	msg_data_t;
typedef pds_word_t	msg_option_t;

#define MSG_TYPE_MAX	UINT32_MAX
#define MSG_TYPEDEF_MAX	MSG_TYPE_MAX
#define MSG_COUNT_MAX	MSG_TYPEDEF_MAX
#define MSG_DATA_MAX	PDS_BYTE_MAX
#define MSG_OPTION_MAX	PDS_WORD_MAX

#define MDT_MSGTYPE	MDT_UINT32
#define MDT_MSGTYPEDEF	MDT_MSGTYPE
#define MDT_MSGCOUNT	MDT_MSGTYPEDEF
#define MDT_MSGDATA	MDT_BYTE
#define MDT_MSGOPTION	MDT_WORD


/*
** Function Declarations
*/

#if defined(__STDC__)
extern pds_ret_t msg_type_define(msg_typedef_t * msg_typedef,
		       	         msg_type_t * msg_type);
#else /* __STDC__ */
extern pds_ret_t msg_type_define();
#endif /* __STDC__ */


#endif /* _PDS_MSG_H_ */
