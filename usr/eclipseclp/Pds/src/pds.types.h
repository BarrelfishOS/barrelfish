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
**        File: pds.types.h
**      Author: Kees Schuerman
**      SccsId: "@(#)pds.types.h	1.7 5/15/95"
** Description: Generic Data Type Definitions
***********************************************************************/

#ifndef _PDS_TYPES_H_
#define _PDS_TYPES_H_


/*
** Simple Data Types
*/

typedef char pds_char;
typedef unsigned char pds_uchar;
typedef char pds_int8;
typedef unsigned char pds_uint8;
typedef short pds_int16;
typedef unsigned short pds_uint16;
typedef int pds_int32;
typedef unsigned int pds_uint32;
typedef float pds_sp_float;
typedef double pds_dp_float;

#define INT8_MIN        -0x80
#define INT8_MAX         0x7F
#define INT16_MIN       -0x8000
#define INT16_MAX        0x7FFF
#define UINT16_MAX       0xFFFF
#define INT32_MIN       -0x80000000
#define INT32_MAX        0x7FFFFFFF
#define UINT32_MAX       0xFFFFFFFF

#if HAVE_NO_VOID_PTR
typedef char * void_ptr;
#else
typedef void * void_ptr;
#endif



/*
** Basic Data Types
*/

typedef pds_uint8 pds_byte_t;
typedef pds_uint16 pds_half_word_t;
typedef pds_uint32 pds_word_t;

#if WORDS_BIGENDIAN
typedef struct {
    pds_word_t high;
    pds_word_t low;
} pds_double_word_t;
#else /* LITTLE ENDIAN */
typedef struct {
    pds_word_t low;
    pds_word_t high;
} pds_double_word_t;
#endif /* WORDS_BIGENDIAN */

#define PDS_BYTE_MAX		0XFF
#define PDS_HALF_WORD_MAX	0XFFFF
#define PDS_WORD_MAX		0XFFFFFFFF


/*
** More Data Types 
*/

#define PDS_PAD_BYTES (sizeof(pds_double_word_t) - sizeof(void_ptr))
#define PDS_PAD_BITS (8 * PDS_PAD_BYTES)

typedef struct {
#if WORDS_BIGENDIAN
    unsigned : PDS_PAD_BITS;	/* pad */
    void_ptr address;
#else /* LITTLE ENDIAN */
    void_ptr address;
    unsigned : PDS_PAD_BITS;	/* pad */
#endif /* WORDS_BIGENDIAN */
} pds_address_t;

#if WORDS_BIGENDIAN
#define PDS_ADDR_OFFSET	(-PDS_PAD_BYTES)
#else /* LITTLE ENDIAN */
#define PDS_ADDR_OFFSET	0
#endif /* WORDS_BIGENDIAN */

typedef pds_uint32 pds_size_t;
typedef pds_int32 pds_ret_t;



/*
** Generic Data Truncate, Round, and Swap Macros
*/

/* pds_word_truncate(pds_word_t word) 				*/

#define pds_word_truncate(word)					\
	((word) - (word) % sizeof(pds_word_t))


/* pds_word_round(pds_word_t word) 				*/

#define pds_word_round(word)    				\
	(pds_word_truncate((word) + sizeof(pds_word_t) -1))


/* pds_dword_truncate(pds_word_t word) 				*/

#define pds_dword_truncate(word)				\
	((word) - (word) % sizeof(pds_double_word_t))


/* pds_dword_round(pds_word_t word) 				*/

#define pds_dword_round(word)    				\
	(pds_dword_truncate((word) + sizeof(pds_double_word_t) -1))


/* hword_byte_swap(pds_half_word_t * hword)			*/

#define hword_byte_swap(hword) {				\
	pds_half_word_t tmp;					\
	pds_byte_t * s = (pds_byte_t *) (hword);		\
	pds_byte_t * d = (pds_byte_t *) &tmp;			\
	{							\
	    d[0] = s[1];					\
	    d[1] = s[0];					\
	    *(hword) = tmp;					\
	}							\
}


/* word_byte_swap(pds_word_t * word) 				*/

#define word_byte_swap(word) {					\
	pds_word_t tmp;						\
	pds_byte_t * s = (pds_byte_t *) (word);			\
	pds_byte_t * d = (pds_byte_t *) &tmp;			\
	{							\
	    d[0] = s[3];					\
	    d[1] = s[2];					\
	    d[2] = s[1];					\
	    d[3] = s[0];					\
	    *(word) = tmp;					\
	}							\
}


/* dword_byte_swap(pds_double_word_t * dword) 			*/

#define dword_byte_swap(dword) {				\
	pds_double_word_t tmp;					\
	pds_byte_t * s = (pds_byte_t *) (dword);		\
	pds_byte_t * d = (pds_byte_t *) &tmp;			\
	{							\
	    d[0] = s[7];					\
	    d[1] = s[6];					\
	    d[2] = s[5];					\
	    d[3] = s[4];					\
	    d[4] = s[3];					\
	    d[5] = s[2];					\
	    d[6] = s[1];					\
	    d[7] = s[0];					\
	    *(dword) = tmp;					\
	}							\
}

#endif /* _PDS_TYPES_H_ */
