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
**        File: pds.xdr.h
**      Author: Kees Schuerman
**      SccsId: "@(#)pds.xdr.h	1.5 5/15/95"
** Description: XDR Interface
***********************************************************************/

#ifndef _PDS_XDR_H_
#define _PDS_XDR_H_


#define xdr_pds_char		xdr_char
#define xdr_pds_u_char		xdr_u_char
#define xdr_pds_int16		xdr_short
#define xdr_pds_u_int16		xdr_u_short
#define xdr_pds_int32		xdr_int
#define xdr_pds_u_int32		xdr_u_int
#define xdr_pds_sp_float	xdr_float
#define xdr_pds_dp_float	xdr_double

#define xdr_pds_size		xdr_pds_u_int32
#define xdr_pds_ret		xdr_pds_int32

#define xdr_msg_type    	xdr_pds_u_int32
#define xdr_msg_typedef 	xdr_msg_type
#define xdr_msg_count   	xdr_msg_typedef
#define xdr_msg_option  	xdr_pds_word


#if defined(__STDC__)
extern bool_t xdr_pds_int8(XDR * xdrs,
			   pds_int8 * int8);
extern bool_t xdr_pds_u_int8(XDR * xdrs,
                             pds_uint8 * uint8);
extern bool_t xdr_pds_byte(XDR * xdrs,
			   pds_byte_t * pds_byte);
extern bool_t xdr_pds_half_word(XDR * xdrs,
                                pds_half_word_t * pds_half_word);
extern bool_t xdr_pds_word(XDR * xdrs,
                           pds_word_t * pds_word);
extern bool_t xdr_pds_double_word(XDR * xdrs,
			          pds_double_word_t * pds_double_word);
extern bool_t xdr_pds_address(XDR * xdrs,
			      pds_address_t * address);
#else /* __STDC__ */
extern bool_t xdr_pds_int8();
extern bool_t xdr_pds_u_int8();
extern bool_t xdr_pds_byte();
extern bool_t xdr_pds_half_word();
extern bool_t xdr_pds_word();
extern bool_t xdr_pds_double_word();
extern bool_t xdr_pds_address();
#endif /* __STDC__ */


#endif /* _PDS_XDR_H_ */

