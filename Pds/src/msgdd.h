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
**        File: msgdd.h
**      Author: Kees Schuerman
**      SccsId: "@(#)msgdd.h	1.2 4/6/95"
** Description: Message Data Description
***********************************************************************/

#ifndef _MSGDD_H_
#define _MSGDD_H_

/*
** Align Options 
*/

#define MSGDD_ENCODE	0x01
#define MSGDD_DECODE	0x02


/*
** Message Data Options
*/

#define MSGDD_XDR   	0x04
#define MSGDD_IDR   	0x08


/*
** Function Declarations
*/

#if defined(__STDC__)
extern pds_ret_t msgdd_init(msg_type_t key);
extern pds_ret_t msg_type_size(msg_type_t msg_type,
                               pds_size_t * size,
			       msg_option_t option);
extern bool_t xdr_msg_data(XDR * xdrs,
                           msg_type_t msg_type,
			   msg_count_t msg_count,
                           msg_data_t * msg_data);
#else /* __STDC__ */
extern pds_ret_t msgdd_init();
extern pds_ret_t msg_type_size();
extern bool_t xdr_msg_data();
#endif /* __STDC__ */


#endif /* _MSGDD_H_ */
