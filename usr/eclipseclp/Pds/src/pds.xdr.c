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
**        File: pds.xdr.c
**      Author: Kees Schuerman
**      SccsId: "@(#)pds.xdr.c	1.7 6/28/95"
** Description: XDR Primitives
***********************************************************************/

#include "machine.h"    /* architecture specific constant definitions */

#include <stdio.h>
#include <rpc/rpc.h>

#include "pds.types.h"
#include "pds.mdt.h"
#include "pds.xdr.h"


/*
** 8-bit integers are transferred as integers instead of 
** characters because character representation is platform 
** dependent and the format conversions may not preserve the 
** integer value it.
*/

bool_t
xdr_pds_int8(xdrs,int8)
    XDR * xdrs;
    pds_int8 * int8;
{
    pds_int16 int16;

    if (xdrs->x_op == XDR_ENCODE) {
	int16 = *int8;
        return(xdr_pds_int16(xdrs,&int16));
    }
    else {
        if (!xdr_pds_int16(xdrs,&int16))
	    return(0);
        *int8 = int16;
        return(1);
    }
}


bool_t
xdr_pds_u_int8(xdrs,uint8)
    XDR * xdrs;
    pds_uint8 * uint8;
{
    pds_uint16 uint16;

    if (xdrs->x_op == XDR_ENCODE) {
        uint16 = *uint8;
        return(xdr_pds_u_int16(xdrs,&uint16));
    }
    else {
        if (!xdr_pds_u_int16(xdrs,&uint16))
	    return(0);
        *uint8 = uint16;
        return(1);
    }
}


bool_t
xdr_pds_byte(xdrs,pds_byte)
    XDR * xdrs;
    pds_byte_t * pds_byte;
{
    return(xdr_opaque(xdrs,(char *) pds_byte,(u_int) sizeof(pds_byte_t)));
}


bool_t
xdr_pds_half_word(xdrs,pds_half_word)
    XDR * xdrs;
    pds_half_word_t * pds_half_word;
{
#if (WORDS_BIGENDIAN)
    return(xdr_opaque(xdrs,(char *) pds_half_word,(u_int) sizeof(pds_half_word_t)));
#else /* LITTLE ENDIAN */
    pds_half_word_t hword;

    if (xdrs->x_op == XDR_ENCODE) {
        hword = *pds_half_word;
        hword_byte_swap(&hword);
        return(xdr_opaque(xdrs,(char *) &hword,(u_int) sizeof(pds_half_word_t)));
    }
    else {
	if (!xdr_opaque(xdrs,(char *) &hword,(u_int) sizeof(pds_half_word_t)))
	    return(0);
        hword_byte_swap(&hword);
        *pds_half_word = hword;
	return(1);
    }
#endif /* WORDS_BIGENDIAN */
}


bool_t
xdr_pds_word(xdrs,pds_word)
    XDR * xdrs;
    pds_word_t * pds_word;
{
#if (WORDS_BIGENDIAN)
    return(xdr_opaque(xdrs,(char *) pds_word,(u_int) sizeof(pds_word_t)));
#else /* LITTLE ENDIAN */
    pds_word_t word;

    if (xdrs->x_op == XDR_ENCODE) {
        word = *pds_word;
        word_byte_swap(&word);
        return(xdr_opaque(xdrs,(char *) &word,(u_int) sizeof(pds_word_t)));
    }
    else {
	if (!xdr_opaque(xdrs,(char *) &word,(u_int) sizeof(pds_word_t)))
	    return(0);
        word_byte_swap(&word);
        *pds_word = word;
	return(1);
    }
#endif /* WORDS_BIGENDIAN */
}


bool_t 
xdr_pds_double_word(xdrs,pds_double_word)
    XDR * xdrs;
    pds_double_word_t * pds_double_word;
{
    return(xdr_pds_word(xdrs,&pds_double_word->high) &&
           xdr_pds_word(xdrs,&pds_double_word->low));
}


bool_t 
xdr_pds_address(xdrs,pds_address)
    XDR * xdrs;
    pds_address_t * pds_address;
{
    return(xdr_pds_double_word(xdrs,(pds_double_word_t *) pds_address));
}

