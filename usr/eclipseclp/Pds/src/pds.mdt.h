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
**        File: pds.mdt.h
**      Author: Kees Schuerman
**      SccsId: "@(#)pds.mdt.h	1.2 14 Nov 1995"
** Description: Message Data Types
***********************************************************************/

#ifndef _PDS_MDT_H_
#define _PDS_MDT_H_

/*
** Configuration
*/

#define MDT_HEAP_SIZE		0x100000


/*
** Message Data Type Definition Constructors
*/

#define MDT_BEGIN                1
#define MDT_END                  2
#define MDT_STRUCT_OPEN          3
#define MDT_STRUCT_CLOSE         4
#define MDT_ARRAY_OF             5


/*
** Standard Message Data Types
*/

#define MDT_STD_MIN              6
#define MDT_BYTE                 6
#define MDT_HWORD                7
#define MDT_WORD                 8
#define MDT_DWORD                9
#define MDT_ADDRESS             10
#define MDT_CHAR                11
#define MDT_UCHAR               12
#define MDT_INT8                13
#define MDT_UINT8               14
#define MDT_INT16               15
#define MDT_UINT16              16
#define MDT_INT32               17
#define MDT_UINT32              18
#define MDT_SP_FLOAT            19
#define MDT_DP_FLOAT            20
#define MDT_STD_MAX             20

#define MDT_SIZE                MDT_UINT32
#define MDT_RET                 MDT_INT32


/*
** Some Type Definitions
*/

typedef pds_uint32 msg_type_t;
typedef pds_uint32 msg_type_no_t;
typedef pds_uint32 msg_intfc_no_t;
typedef msg_type_t msg_typedef_t;
typedef msg_typedef_t msg_count_t;
typedef pds_byte_t msg_data_t;
typedef pds_word_t msg_option_t;

#define MSG_TYPE_MAX            UINT32_MAX
#define MSG_TYPE_NO_MAX         UINT16_MAX
#define MSG_INTFC_NO_MAX        UINT16_MAX
#define MSG_TYPEDEF_MAX         MSG_TYPE_MAX
#define MSG_COUNT_MAX           MSG_TYPEDEF_MAX
#define MSG_DATA_MAX            PDS_BYTE_MAX
#define MSG_OPTION_MAX          PDS_WORD_MAX

#define MDT_MSGTYPE             MDT_UINT32
#define MDT_MSGTYPENO           MDT_UINT32
#define MDT_MSGINTFCNO          MDT_UINT32
#define MDT_MSGTYPEDEF          MDT_MSGTYPE
#define MDT_MSGCOUNT            MDT_MSGTYPEDEF
#define MDT_MSGDATA             MDT_BYTE
#define MDT_MSGOPTION           MDT_WORD


/*
** Message Data Representations
*/

#define MDT_XDR   	0x04
#define MDT_IDR   	0x08


/*
** Function Declarations
*/

#if defined(__STDC__)
extern pds_ret_t pds_types_init(char * address);
extern pds_ret_t pds_type_define(msg_intfc_no_t msg_intfc_no,
                                 msg_type_no_t msg_type_no,
                                 msg_typedef_t * msg_typedef,
                                 msg_type_t * msg_type);
extern pds_ret_t pds_type_size(msg_type_t msg_type,
                               pds_size_t * size,
			       msg_option_t option);
extern bool_t pds_type_xdr(XDR * xdrs,
                           msg_type_t msg_type,
                           msg_data_t * msg_data);
extern bool_t pds_msg_xdr(XDR * xdrs,
                          msg_type_t msg_type,
		          msg_count_t msg_count,
                          msg_data_t * msg_data);
#else /* __STDC__ */
extern pds_ret_t pds_types_init();
extern pds_ret_t pds_type_define();
extern pds_ret_t pds_type_size();
extern bool_t pds_type_xdr();
extern bool_t pds_msg_xdr();
#endif /* __STDC__ */


#endif /* _PDS_MDT_H_ */
