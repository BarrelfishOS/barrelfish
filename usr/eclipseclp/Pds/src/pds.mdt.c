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
**        File: pds.mdt.c
**      Author: Kees Schuerman
**      SccsId: "@(#)pds.mdt.c	1.5 24 Nov 1995"
** Description: Message Data Types
***********************************************************************
** Message Data Description
** ------------------------
** A message data description defines the contents of a message, i.e.
** the type and number of message data elements. A type of a message
** data element is a standard C-type, a C-structure, or a single 
** dimensional array. The elements of the array and the fields of the 
** structure can again be standard C-types, C-structures and single
** dimensional arrays. 
** The transmission of pointers is supported by the possibility to
** transmit 64-bit addresses and the availability of primitives to
** convert between pointers and 64-bit addresses. 
** The transmission of unions, bit-fields, and linked data structures
** is not supported. 
**
**
** Alignment Requirements
** ----------------------
** Data conversions from the local format to XDR format (XDR: eXternal 
** Data Representation) and vice versa are based on standard C-type 
** format conversions.
** It is therefore important to know exactly where to fetch/store local 
** representation of the most basic elements (i.e. of standard C-type). 
** This depends on the alignment requirements of the standard C-types, 
** which may be machine dependent, and if they are part of a C-structure, 
** also of the C-structure alignment requirements.
** A C-structure inherits its alignment requirements from its fields, 
** i.e. when the alignment is required to be on n bytes then both the
** starting address and the size of the C-structure are multiples of 
** n bytes. C-structure alignment is thus fully determined by the 
** alignment requirements of the standard C-types.
**
**
** Message Data Type Definition
** ----------------------------
** Message data types are defined in terms of standard and earlier 
** defined message data types. The standard message data types are:
**
**		MDT_BYTE
**		MDT_HWORD
**		MDT_WORD
**		MDT_DWORD
**		MDT_ADDRESS
**		MDT_CHAR
**		MDT_UCHAR
**		MDT_INT8
**		MDT_UINT8
**		MDT_INT16
**		MDT_UINT16
**		MDT_INT32
**		MDT_UINT32
**		MDT_SP_FLOAT
**		MDT_DP_FLOAT
**
** Message data types can be defined dynamically by means of the primitive 
** pds_type_define(). Its input is a message data type definition using
** the constructors 
**
**		MDT_BEGIN
**		MDT_END
**		MDT_STRUCT_OPEN
**		MDT_STRUCT_CLOSE
**		MDT_ARRAY_OF
**
** and may use any existing message data type. A message data type 
** definition starts with MDT_BEGIN, followed by either a structure 
** definition or an array definition, and ends with MDT_END. A structure 
** definition starts with MDT_STRUCT_OPEN, has at least one field 
** definition, and ends with MDT_STRUCT_CLOSE. The field definitions of 
** a structure definition are message data types, structure definitions 
** or array definitions. An array definition starts with MDT_ARRAY_OF, 
** followed by an array count (i.e. the number of elements in the array) 
** and an array element definition. An array element definition, like a 
** structure element definition, can be message data types, structure 
** definitions or array definitions.
**
** The following two examples show the message data type definitions for 
** a structure and an array.
**
** MDT_BEGIN
** MDT_STRUCT_OPEN	struct {
** MDT_CHAR		    char c1;
** MDT_INT32		    pds_int32 i1; 
** MDT_INT32		    pds_int32 i2;
** MDT_SP_FLOAT		    pds_sp_float f1;
** MDT_CHAR		    char c2;
** MDT_STRUCT_CLOSE	}
** MDT_END 
**
** MDT_BEGIN
** MDT_ARRAY_OF		pds_int32 i[12];
** 12
** MDT_INT32
** MDT_END 
**
** The following example uses the structure defined above. We assume that
** it is identified with MDT_S1.
**
** MDT_BEGIN
** MDT_ARRAY_OF		
** 17
** MDT_ARRAY_OF
** 9
** MDT_S1
** MDT_END
**
** This could for example be used for the transmission of the following
** C-variable: a 2-dimensional array of structures.
**
** typedef struct {
**    char c1;
**    pds_int32 i1; 
**    pds_int32 i2;
**    pds_sp_float f1;
**    char c2;
** } variable [17][9];
**
** Message data type definitions are used to control the conversion of 
** the message data between local format and XDR format. It carries 
** information on location and type of the data. The exact location is 
** determined by taking into account the (platform dependent) alignment 
** requirements as discussed above.
**
**
** Message Data Type Identifiers
** -----------------------------
** For identification purposes every message data type has an associated
** message data type interface number, a message data type number, and a 
** message data type identifier. A message data type interface consists
** thus of a set of message data type definitions. Defining such an 
** interface involves selecting a unique interface number and a unique
** type number for every message data type. The result is a unique
** type identifier for every message data type.
**
**
** Message Data Type Tables
** ------------------------
** A message data type interface is defined by a a message data type 
** table which containts the type definitions of the interface's message
** data types. There are therefore as many message data type tables as 
** there are message data type interfaces. The associated table and table
** entry of a message data type can be derived from its type identifier.
***********************************************************************/

#include "machine.h"    /* architecture specific constant definitions */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <rpc/rpc.h>

#include "pds.types.h"
#include "pds.error.h"
#include "pds.mem.h"
#include "pds.mdt.h"
#include "pds.xdr.h"


/**********************************************************************
** Configuration
***********************************************************************/

#define MDT_HEAP_SIZE		0x100000
#define MDT_MAX_NOF_INTFCS	256     /* max number of interfaces */
#define MDT_MAX_NOF_TYPES	256     /* max number of types per interface */


/**********************************************************************
** Align Options
***********************************************************************/

#define MDT_ENCODE      0x01
#define MDT_DECODE      0x02


/**********************************************************************
** Padding
***********************************************************************/

#define MDT_PAD		0


/**********************************************************************
** Message Data Types (MDT), Interface Numbers, and Type Numbers
***********************************************************************/

#define IntfcNo(MDT)	((MDT) / 0x10000)
#define TypeNo(MDT)	((MDT) % 0x10000)


/**********************************************************************
** Type Definitions
***********************************************************************/

typedef struct {
    msg_type_no_t type_no;  /* message type number			 */
    unsigned int def_size;  /* size of message type definition           */
    unsigned int idr_align; /* alignment of internal data representation */
    unsigned int idr_size;  /* size of internal data representation      */
    unsigned int xdr_size;  /* size of external data representation      */
    msg_typedef_t * def;    /* message type definition                   */
} mdt_t;		    /* entry of message data type table		 */

typedef struct {
    msg_intfc_no_t intfc_no;  /* message type interface number           */
    mdt_t * msg_types; 	      /* message type table                      */
} msg_intfcs_t;		      /* message type interface table		 */



/**********************************************************************
** Some Global Variables 
***********************************************************************/

static pds_heap_descriptor_t mdt_phd;		      /* private heap */

static int mdt_initialised = 0;  		      /* state	      */

static msg_typedef_t mdt_byte = MDT_BYTE;	      /* message      */
static msg_typedef_t mdt_hword = MDT_HWORD;	      /* data type    */
static msg_typedef_t mdt_word = MDT_WORD;	      /* definitions  */
static msg_typedef_t mdt_dword = MDT_DWORD;	      /*      .       */
static msg_typedef_t mdt_address = MDT_ADDRESS;       /*      .       */
static msg_typedef_t mdt_char = MDT_CHAR;
static msg_typedef_t mdt_uchar = MDT_UCHAR;
static msg_typedef_t mdt_int8 = MDT_INT8;
static msg_typedef_t mdt_uint8 = MDT_UINT8;
static msg_typedef_t mdt_int16 = MDT_INT16;
static msg_typedef_t mdt_uint16 = MDT_UINT16;
static msg_typedef_t mdt_int32 = MDT_INT32;
static msg_typedef_t mdt_uint32 = MDT_UINT32;
static msg_typedef_t mdt_sp_float = MDT_SP_FLOAT;
static msg_typedef_t mdt_dp_float = MDT_DP_FLOAT;     /*      .       */

/* standard types table */
static mdt_t std_types[MDT_MAX_NOF_TYPES];  

/* type interface table */
static msg_intfcs_t msg_intfcs[MDT_MAX_NOF_INTFCS];

static l_mutex_t mdt_mutex;	     /* protects type interface table */

#define MsgDD_Lock()	l_mutex_lock(&mdt_mutex)
#define MsgDD_Unlock()	l_mutex_unlock(&mdt_mutex)



/**********************************************************************
** Runtime Consistency Checking
***********************************************************************/

#if defined(NDEBUG)
#define mdt_assert(ex)
#else
#define mdt_assert(ex) {                                      	\
    if (!(ex)) {                                                \
        (void) fprintf(stderr,                                  \
               "PDS MPS-M Assertion Failed:");                  \
        (void) fprintf(stderr, " file \"%s\"", __FILE__);       \
        (void) fprintf(stderr, " line %d\n", __LINE__);         \
        exit(1);                                                \
    }                                                           \
}
#endif /* NDEBUG */
#define mdt_assert_always() {                                 	\
        (void) fprintf(stderr,                                  \
               "PDS MPS-M Assertion Failed:");                  \
        (void) fprintf(stderr, " file \"%s\"", __FILE__);       \
        (void) fprintf(stderr, " line %d\n", __LINE__);         \
        exit(1);                                                \
}




/**********************************************************************
************************ Some Local Primitives  ***********************
***********************************************************************/

#if defined(__STDC__)
static pds_ret_t idrmsg_pad_info(msg_typedef_t * * msg_typedef,
				 pds_size_t * size,
				 unsigned int * align);
static pds_ret_t idrmsg_array_info(msg_typedef_t * * msg_typedef,
				   pds_size_t * size,
				   unsigned int * align);
static pds_ret_t idrmsg_struct_info(msg_typedef_t * * msg_typedef,
			            pds_size_t * size,
			            unsigned int * align);
static pds_ret_t idrmsg_opaque_info(msg_typedef_t * * msg_typedef,
    				    pds_size_t * size,
				    unsigned int * align);
static pds_ret_t xdrmsg_pad_info(msg_typedef_t * * msg_typedef,
				 pds_size_t * size);
static pds_ret_t xdrmsg_array_info(msg_typedef_t * * msg_typedef,
				   pds_size_t * size);
static pds_ret_t xdrmsg_struct_info(msg_typedef_t * * msg_typedef,
				    pds_size_t * size);
static pds_ret_t xdrmsg_opaque_info(msg_typedef_t * * msg_typedef,
    				    pds_size_t * size);
static bool_t xdr_msg_stdtype(XDR * xdrs,
                              msg_typedef_t * * msg_typedef,
                              msg_data_t * * msg_data);
static bool_t xdr_msg_pad(XDR * xdrs,
                          msg_typedef_t * * msg_typedef,
                          msg_data_t * * msg_data);
static bool_t xdr_msg_array(XDR * xdrs,
                            msg_typedef_t * * msg_typedef,
                            msg_data_t * * msg_data);
static bool_t xdr_msg_structure(XDR * xdrs,
                                msg_typedef_t * * msg_typedef,
                                msg_data_t * * msg_data);
static bool_t xdr_msg_opaque(XDR * xdrs,
                             msg_typedef_t * * msg_typedef,
                             msg_data_t * * msg_data);
#else /* __STDC__ */
static pds_ret_t idrmsg_pad_info();
static pds_ret_t idrmsg_array_info();
static pds_ret_t idrmsg_struct_info();
static pds_ret_t idrmsg_opaque_info();
static pds_ret_t xdrmsg_pad_info();
static pds_ret_t xdrmsg_array_info();
static pds_ret_t xdrmsg_struct_info();
static pds_ret_t xdrmsg_opaque_info();
static bool_t xdr_msg_stdtype();
static bool_t xdr_msg_pad();
static bool_t xdr_msg_array();
static bool_t xdr_msg_structure();
static bool_t xdr_msg_opaque();
#endif /* __STDC__ */


static pds_ret_t
get_matching_index(msg_type,intfc_index,type_index)
    msg_type_t msg_type;
    unsigned * intfc_index;
    unsigned * type_index;
{
    mdt_t * msg_types;
    unsigned intfc_no;
    unsigned type_no;
    unsigned index;

    /* initialise indexes to default value */
    *intfc_index = 0;
    *type_index = 0;

    /* derive intfc_index */
    intfc_no = IntfcNo(msg_type);
    index = intfc_no % 0x10 + intfc_no / 0x10;
    index = index % (MDT_MAX_NOF_INTFCS / 2);
    if (msg_intfcs[index].intfc_no == intfc_no)
	*intfc_index = index;
    else {
	index = MDT_MAX_NOF_INTFCS - 1;
	while (index && 
	       (msg_intfcs[index].intfc_no != intfc_no))
	    index--;
	if (!index)
	    return(PDS_INVAL);
	else
	    *intfc_index = index;
    }
    mdt_assert(msg_intfcs[index].msg_types);

    /* derive type_index */
    msg_types = msg_intfcs[index].msg_types;
    type_no = TypeNo(msg_type);
    index = type_no % 0x10 + type_no / 0x10;
    index = index % (MDT_MAX_NOF_TYPES / 2);
    if (msg_types[index].type_no == type_no)
        *type_index = index;
    else {
	index = MDT_MAX_NOF_TYPES - 1;
	while (index && 
	       (msg_types[index].type_no != type_no))
	    index--;
	if (!index)
	    return(PDS_INVAL);
	else
	    *type_index = index;
    }
    mdt_assert(msg_types[index].def);

    return(PDS_OK);
}


static pds_ret_t
get_free_index(msg_type,intfc_index,type_index)
    msg_type_t msg_type;
    unsigned * intfc_index;
    unsigned * type_index;
{
    mdt_t * msg_types;
    unsigned intfc_no;
    unsigned type_no;
    unsigned index;
    int i;

    if (get_matching_index(msg_type,intfc_index,type_index) == PDS_OK)
	return(PDS_INVAL);

    /* derive intfc_index */
    intfc_no = IntfcNo(msg_type);
    index = *intfc_index;
    if (!index) {
        index = intfc_no % 0x10 + intfc_no / 0x10;
        index = index % (MDT_MAX_NOF_INTFCS / 2);
        if (!msg_intfcs[index].intfc_no)
	    *intfc_index = index;
        else {
	    index = MDT_MAX_NOF_INTFCS - 1;
	    while (index && msg_intfcs[index].intfc_no)
	        index--;
	    if (!index)
	        return(PDS_NORESOURCES);
	    else 
	        *intfc_index = index;
        }

	/* allocate message type table */
	msg_types = (mdt_t *) 
		    pds_mem_alloc(&mdt_phd,
				  MDT_MAX_NOF_TYPES * sizeof(mdt_t));
	if (!msg_types)
	    return(PDS_NOMEMORY);
	/* initialise message type table */
        for (i=0;i<MDT_MAX_NOF_TYPES;i++) {
            msg_types[i].type_no = (msg_type_no_t) 0;
            msg_types[i].def = (msg_typedef_t *) 0;
            msg_types[i].def_size = 0;
            msg_types[i].idr_align = 1;
            msg_types[i].idr_size = 0;
            msg_types[i].xdr_size = 0;
        }

        msg_intfcs[index].intfc_no = intfc_no;
        msg_intfcs[index].msg_types = msg_types;
        *type_index = (msg_type & 0xffff) % (MDT_MAX_NOF_TYPES / 2);
        return(PDS_OK);
    }
    else {
        /* derive type_index */
        msg_types = msg_intfcs[index].msg_types;
        type_no = TypeNo(msg_type);
        index = type_no % 0x10 + type_no / 0x10;
        index = index % (MDT_MAX_NOF_TYPES / 2);
        if (!msg_types[index].type_no)
            *type_index = index;
        else {
	    index = MDT_MAX_NOF_TYPES - 1;
	    while (index && msg_types[index].type_no)
	        index--;
	    if (!index)
	        return(PDS_NORESOURCES);
	    else
	        *type_index = index;
        }
        return(PDS_OK);
    }
}


static pds_ret_t
get_mdt(msg_type,mdt)
    msg_type_t msg_type;
    mdt_t * * mdt;
{
    unsigned intfc_index;
    unsigned type_index;
    pds_ret_t pret;

    if (!IntfcNo(msg_type)) {
	if ((msg_type < MDT_STD_MIN) || (msg_type > MDT_STD_MAX))
	    return(PDS_INVAL);
	else {
	    *mdt = &std_types[msg_type];
	    return(PDS_OK);
	}
    }

    pret = get_matching_index(msg_type,&intfc_index,&type_index);
    if (pret != PDS_OK)
	return(pret);
    else {
        *mdt = &msg_intfcs[intfc_index].msg_types[type_index];
	return(PDS_OK);
    }
}


/*
**   Primitive: padalign()
** Description: aligns ptr on <align>-byte boundary. When in encoding
**		mode, padding with nil-bytes takes place.
*/

static void
padalign(ptr,align,size,option)
    char * * ptr;
    unsigned align;
    unsigned * size;
    msg_option_t option;
{
    int i;
    long count;

    mdt_assert(align>0);

    count = (*ptr - (char *) 0) % align;
    if (count <= 0)
	count = -count;
    else
	count = align - count;
    if (option == MDT_ENCODE) /* pad with zeros */
	for (i=0;i<count;i++)
	    (*ptr)[i] = 0;
    *ptr = *ptr + count;
    if (size)
	*size = count;
}


/* 
**   Primitive: derive_std_alignments()
** Description: derives the alignment requirements of the standard 
**		message data types and records this in the message
**		data type table.
*/

static void
derive_std_alignments()
{
    struct {
	char c;
	pds_half_word_t t;
    } pds_hword_str;

    struct {
	char c;
	pds_word_t t;
    } pds_word_str;

    struct {
	char c;
	pds_double_word_t t;
    } pds_dword_str;

    struct {
	char c;
	pds_address_t t;
    } pds_address_str;

    struct {
	char c;
	pds_int8 t;
    } pds_int8_str;

    struct {
	char c;
	pds_uint8 t;
    } pds_uint8_str;

    struct {
	char c;
	pds_int16 t;
    } pds_int16_str;

    struct {
	char c;
	pds_uint16 t;
    } pds_uint16_str;

    struct {
	char c;
	pds_int32 t;
    } pds_int32_str;

    struct {
	char c;
	pds_uint32 t;
    } pds_uint32_str;

    struct {
	char c;
	pds_sp_float t;
    } pds_sp_float_str;

    struct {
	char c;
	pds_dp_float t;
    } pds_dp_float_str;

    mdt_assert(sizeof(pds_byte_t) == 1);
    mdt_assert(sizeof(char) == 1);
    mdt_assert(sizeof(unsigned char) == 1);

    std_types[MDT_BYTE].idr_align = 1;
    std_types[MDT_HWORD].idr_align = sizeof(pds_hword_str) -
				     sizeof(pds_hword_str.t);
    std_types[MDT_WORD].idr_align = sizeof(pds_word_str) -
				    sizeof(pds_word_str.t);
    std_types[MDT_DWORD].idr_align = sizeof(pds_dword_str) -
				     sizeof(pds_dword_str.t);
    std_types[MDT_ADDRESS].idr_align = sizeof(pds_address_str) -
				       sizeof(pds_address_str.t);
    std_types[MDT_CHAR].idr_align = 1;
    std_types[MDT_UCHAR].idr_align = 1;
    std_types[MDT_INT8].idr_align = sizeof(pds_int8_str) - 
				    sizeof(pds_int8_str.t);
    std_types[MDT_UINT8].idr_align = sizeof(pds_uint8_str) -
				     sizeof(pds_uint8_str.t);
    std_types[MDT_INT16].idr_align = sizeof(pds_int16_str) - 
				     sizeof(pds_int16_str.t);
    std_types[MDT_UINT16].idr_align = sizeof(pds_uint16_str) - 
				      sizeof(pds_uint16_str.t);
    std_types[MDT_INT32].idr_align = sizeof(pds_int32_str) - 
				     sizeof(pds_int32_str.t);
    std_types[MDT_UINT32].idr_align = sizeof(pds_uint32_str) - 
				      sizeof(pds_uint32_str.t);
    std_types[MDT_SP_FLOAT].idr_align = sizeof(pds_sp_float_str) - 
					sizeof(pds_sp_float_str.t);
    std_types[MDT_DP_FLOAT].idr_align = sizeof(pds_dp_float_str) - 
					sizeof(pds_dp_float_str.t);
}


static pds_ret_t
idrmsg_mdt_info(msg_typedef,size,align)
    msg_typedef_t * * msg_typedef;
    pds_size_t * size; 
    unsigned int * align;
{
    mdt_t * mdt;
    pds_ret_t pret;

    if ((pret = get_mdt(**msg_typedef,&mdt)) != PDS_OK)
	return(pret);
 
    *size = mdt->idr_size;
    *align = mdt->idr_align;
    (*msg_typedef)++;
    return(PDS_OK);
}


static pds_ret_t
xdrmsg_mdt_info(msg_typedef,size)
    msg_typedef_t * * msg_typedef;
    pds_size_t * size; 
{
    mdt_t * mdt;
    pds_ret_t pret;
 
    if ((pret = get_mdt(**msg_typedef,&mdt)) != PDS_OK)
	return(pret);
 
    *size = mdt->xdr_size;
    (*msg_typedef)++;
    return(PDS_OK);
}


static pds_ret_t
idrmsg_pad_info(msg_typedef,size,align)
    msg_typedef_t * * msg_typedef;
    pds_size_t * size;
    unsigned * align;
{
    mdt_assert(**msg_typedef == MDT_PAD);

    (*msg_typedef)++;

    *size = (pds_size_t) **msg_typedef;
    *align = 1;

    (*msg_typedef)++;

    return(PDS_OK);
}


static pds_ret_t
xdrmsg_pad_info(msg_typedef,size)
    msg_typedef_t * * msg_typedef;
    pds_size_t * size;
{
    mdt_assert(**msg_typedef == MDT_PAD);

    (*msg_typedef)++;

    /* skip pad_count */
    (*msg_typedef)++;

    *size = 0;

    return(PDS_OK);
}


static pds_ret_t
idrmsg_array_info(msg_typedef,size,align)
    msg_typedef_t * * msg_typedef;
    pds_size_t * size;
    unsigned * align;
{
    pds_uint32 array_count;
    pds_size_t element_size;
    pds_ret_t pret;

    mdt_assert(**msg_typedef == MDT_ARRAY_OF);

    /* get array_count */
    (*msg_typedef)++;
    array_count = (pds_uint32) **msg_typedef;

    /* get element_size and alignment */ 
    (*msg_typedef)++;
    if ((pret = idrmsg_opaque_info(msg_typedef,&element_size,align)) != PDS_OK)
	return(pret);
    else {
        *size = array_count * element_size;
        return(PDS_OK);
    }
}


static pds_ret_t
xdrmsg_array_info(msg_typedef,size)
    msg_typedef_t * * msg_typedef;
    pds_size_t * size;
{
    pds_uint32 array_count;
    pds_size_t element_size;
    pds_ret_t pret;

    mdt_assert(**msg_typedef == MDT_ARRAY_OF);

    /* get array_count */
    (*msg_typedef)++;
    array_count = (pds_uint32) **msg_typedef;

    (*msg_typedef)++;
    if (**msg_typedef == MDT_BYTE) { /* single opaque array */
        unsigned int remainder;
	remainder = array_count % 4;
	if (remainder)
	    *size = 4 + array_count + (4 - remainder);
	else
	    *size = 4 + array_count;
	(*msg_typedef)++;
    } 
    else {
        /* get element_size */ 
        if ((pret = xdrmsg_opaque_info(msg_typedef,&element_size)) != PDS_OK)
	    return(pret);
        *size = array_count * element_size;
    }

    return(PDS_OK);
}


static pds_ret_t
idrmsg_struct_info(msg_typedef,size,align)
    msg_typedef_t * * msg_typedef;
    pds_size_t * size;
    unsigned int * align;
{
    unsigned field_align;
    pds_size_t field_size;
    pds_size_t struct_size;
    unsigned struct_align;
    pds_ret_t pret;

    mdt_assert(**msg_typedef == MDT_STRUCT_OPEN);

    struct_size = 0;
    struct_align = 1;
    (*msg_typedef)++;
    while (**msg_typedef != MDT_STRUCT_CLOSE) {
	if ((pret = idrmsg_opaque_info(msg_typedef,&field_size,&field_align)) != PDS_OK)
	    return(pret);
	struct_size += field_size;
	if (field_align > struct_align)
	    struct_align = field_align;
    }
    (*msg_typedef)++;
    *size = struct_size;
    *align = struct_align;

    return(PDS_OK);
}


static pds_ret_t
xdrmsg_struct_info(msg_typedef,size)
    msg_typedef_t * * msg_typedef;
    pds_size_t * size;
{
    pds_size_t field_size;
    pds_size_t struct_size;

    mdt_assert(**msg_typedef == MDT_STRUCT_OPEN);

    struct_size = 0;
    (*msg_typedef)++;
    while (**msg_typedef != MDT_STRUCT_CLOSE) {
	xdrmsg_opaque_info(msg_typedef,&field_size);
	struct_size += field_size;
    }
    (*msg_typedef)++;
    *size = struct_size;

    return(PDS_OK);
}


static pds_ret_t
idrmsg_opaque_info(msg_typedef,size,align)
    msg_typedef_t * * msg_typedef;
    pds_size_t * size;
    unsigned int * align;
{
    switch (**msg_typedef) {
        case MDT_STRUCT_OPEN :
            return(idrmsg_struct_info(msg_typedef,size,align));
        case MDT_STRUCT_CLOSE :
            return(PDS_INVAL);
        case MDT_ARRAY_OF :
            return(idrmsg_array_info(msg_typedef,size,align));
        case MDT_PAD :
            return(idrmsg_pad_info(msg_typedef,size,align));
        default :
	    return(idrmsg_mdt_info(msg_typedef,size,align));
    }
}


static pds_ret_t
xdrmsg_opaque_info(msg_typedef,size)
    msg_typedef_t * * msg_typedef;
    pds_size_t * size;
{
    switch (**msg_typedef) {
        case MDT_STRUCT_OPEN :
            return(xdrmsg_struct_info(msg_typedef,size));
        case MDT_STRUCT_CLOSE :
            return(PDS_INVAL);
        case MDT_ARRAY_OF :
            return(xdrmsg_array_info(msg_typedef,size));
        case MDT_PAD :
            return(xdrmsg_pad_info(msg_typedef,size));
        default :
	    return(xdrmsg_mdt_info(msg_typedef,size));
    }
}


static bool_t
xdr_msg_stdtype(xdrs,msg_typedef,msg_data)
    XDR * xdrs;
    msg_typedef_t * * msg_typedef;
    msg_data_t * * msg_data;
{
    msg_type_t msg_type;

    msg_type = **msg_typedef;
    (*msg_typedef)++;

    switch (msg_type) {
	case MDT_BYTE :
	    if (xdr_pds_byte(xdrs,(pds_byte_t *) *msg_data)) {
		*msg_data = (msg_data_t *) 
			    ((char *) *msg_data + sizeof(pds_byte_t));
		return(1);
	    } else return(0);
	case MDT_HWORD :
	    if (xdr_pds_half_word(xdrs,(pds_half_word_t *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(pds_half_word_t));
		return(1);
	    } else return(0);
	case MDT_WORD :
	    if (xdr_pds_word(xdrs,(pds_word_t *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(pds_word_t));
		return(1);
	    } else return(0);
	case MDT_DWORD :
	    if (xdr_pds_double_word(xdrs,(pds_double_word_t *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(pds_double_word_t));
		return(1);
	    } else return(0);
	case MDT_ADDRESS :
	    if (xdr_pds_address(xdrs,(pds_address_t *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(pds_address_t));
		return(1);
	    } else return(0);
	case MDT_CHAR :
	    if (xdr_pds_char(xdrs,(pds_char *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(pds_char));
		return(1);
	    } else return(0);
	case MDT_UCHAR :
	    if (xdr_pds_u_char(xdrs,(pds_uchar *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(pds_uchar));
		return(1);
	    } else return(0);
	case MDT_INT8 :
	    if (xdr_pds_int8(xdrs,(pds_int8 *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(pds_int8));
		return(1);
	    } else return(0);
	case MDT_UINT8 :
	    if (xdr_pds_u_int8(xdrs,(pds_uint8 *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(pds_uint8));
		return(1);
	    } else return(0);
	case MDT_INT16 :
	    if (xdr_pds_int16(xdrs,(pds_int16 *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(pds_int16));
		return(1);
	    } else return(0);
	case MDT_UINT16 :
	    if (xdr_pds_u_int16(xdrs,(pds_uint16 *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(pds_uint16));
		return(1);
	    } else return(0);
	case MDT_INT32 :
	    if (xdr_pds_int32(xdrs,(pds_int32 *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(pds_int32));
		return(1);
	    } else return(0);
	case MDT_UINT32 :
	    if (xdr_pds_u_int32(xdrs,(pds_uint32 *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(pds_uint32));
		return(1);
	    } else return(0);
	case MDT_SP_FLOAT :
	    if (xdr_pds_sp_float(xdrs,(pds_sp_float *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(pds_sp_float));
		return(1);
	    } else return(0);
	case MDT_DP_FLOAT :
	    if (xdr_pds_dp_float(xdrs,(pds_dp_float *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(pds_dp_float));
		return(1);
	    } else return(0);
	default :
	    return(0);
    }
}


static bool_t
xdr_msg_pad(xdrs,msg_typedef,msg_data)
    XDR * xdrs;
    msg_typedef_t * * msg_typedef;
    msg_data_t * * msg_data;
{
    pds_uint32 pad_count;
    int i;

    mdt_assert(**msg_typedef == MDT_PAD);

    /* get pad_count */
    (*msg_typedef)++;
    pad_count = (pds_uint32) **msg_typedef;

    if (xdrs->x_op == XDR_ENCODE) {
	*msg_data = (msg_data_t *)
		    ((char *) *msg_data + pad_count);
    }
    else {
        for (i=0;i<pad_count;i++) {
	    * (char *) *msg_data = (char) 0;
	    *msg_data = (msg_data_t *)
                    ((char *) *msg_data + 1);
	}
    }

    (*msg_typedef)++;

    return(1);
}


static bool_t
xdr_msg_array(xdrs,msg_typedef,msg_data)
    XDR * xdrs;
    msg_typedef_t * * msg_typedef;
    msg_data_t * * msg_data;
{
    pds_uint32 array_count;
    msg_typedef_t * def;
    int i;

    mdt_assert(**msg_typedef == MDT_ARRAY_OF);

    /* get array_count */
    (*msg_typedef)++;
    array_count = (pds_uint32) **msg_typedef;

    (*msg_typedef)++;
    if (**msg_typedef == MDT_BYTE) { /* single opaque array */
	(*msg_typedef)++;
	if (!xdr_opaque(xdrs, (char *) *msg_data, (u_int) array_count))
	    return(0);
	*msg_data = (msg_data_t *)
		    ((char *) *msg_data + array_count * sizeof(pds_byte_t));
    }
    else { 
        for (i=0;i<array_count;i++) {
	    def = *msg_typedef;
	    if (!xdr_msg_opaque(xdrs,&def,msg_data))
	        return(0);
        }
	*msg_typedef = def;
    }
    return(1);
}


static bool_t
xdr_msg_structure(xdrs,msg_typedef,msg_data)
    XDR * xdrs;
    msg_typedef_t * * msg_typedef;
    msg_data_t * * msg_data;
{
    mdt_assert(**msg_typedef == MDT_STRUCT_OPEN);

    (*msg_typedef)++;
    while (**msg_typedef != MDT_STRUCT_CLOSE)
	if (!xdr_msg_opaque(xdrs,msg_typedef,msg_data))
	    return(0);
    (*msg_typedef)++;
    return(1);
}


static bool_t 
xdr_msg_opaque(xdrs,msg_typedef,msg_data)
    XDR * xdrs;
    msg_typedef_t * * msg_typedef;
    msg_data_t * * msg_data;
{
    switch (**msg_typedef) {
    	case MDT_STRUCT_OPEN :
            return(xdr_msg_structure(xdrs,msg_typedef,msg_data));
    	case MDT_STRUCT_CLOSE :
            return(0);
    	case MDT_ARRAY_OF :
            return(xdr_msg_array(xdrs,msg_typedef,msg_data));
    	case MDT_PAD :
            return(xdr_msg_pad(xdrs,msg_typedef,msg_data));
    	default :
	    return(xdr_msg_stdtype(xdrs,msg_typedef,msg_data));
    }
}




/**********************************************************************
************************  Exported Primitives  ************************
***********************************************************************/

#define MDT_BUF_SIZE 512

pds_ret_t
pds_types_init(address)
    char * address;
{
    pds_byte_t idr_byte;
    pds_half_word_t idr_hword;
    pds_word_t idr_word;
    pds_double_word_t idr_dword;
    pds_address_t idr_address;
    pds_char idr_char;
    pds_uchar idr_uchar;
    pds_int8 idr_int8;
    pds_uint8 idr_uint8;
    pds_int16 idr_int16;
    pds_uint16 idr_uint16;
    pds_int32 idr_int32;
    pds_uint32 idr_uint32;
    pds_sp_float idr_sp_float;
    pds_dp_float idr_dp_float;

    char buffer[MDT_BUF_SIZE];
    unsigned int i,pos_old,pos_new;
    XDR xdrs;

    mdt_assert(sizeof(msg_count_t) == sizeof(msg_typedef_t));
    mdt_assert(sizeof(msg_type_t) == 4);

    /* private heap for message type definitions */
    if (pds_mem_init((char *) 0,address,MDT_HEAP_SIZE,&mdt_phd,1) == (char *) -1)
        return(PDS_NOMEMORY);

    /* initialise message type interface table */
    for (i=0;i<MDT_MAX_NOF_INTFCS;i++) {
	msg_intfcs[i].intfc_no = (msg_intfc_no_t) 0;
        msg_intfcs[i].msg_types = (mdt_t *) 0;
    }

    /* initialise standard message type table */
    for (i=0;i<MDT_MAX_NOF_TYPES;i++) {
	std_types[i].type_no = (msg_type_no_t) 0;
	std_types[i].def = (msg_typedef_t *) 0;
	std_types[i].def_size = 0;
	std_types[i].idr_align = 1;
	std_types[i].idr_size = 0;
	std_types[i].xdr_size = 0;
    }

    derive_std_alignments();

    std_types[MDT_BYTE].type_no = MDT_BYTE;
    std_types[MDT_HWORD].type_no = MDT_HWORD;
    std_types[MDT_WORD].type_no = MDT_WORD;
    std_types[MDT_DWORD].type_no = MDT_DWORD;
    std_types[MDT_ADDRESS].type_no = MDT_ADDRESS;
    std_types[MDT_CHAR].type_no = MDT_CHAR;
    std_types[MDT_UCHAR].type_no = MDT_UCHAR;
    std_types[MDT_INT8].type_no = MDT_INT8;
    std_types[MDT_UINT8].type_no = MDT_UINT8;
    std_types[MDT_INT16].type_no = MDT_INT16;
    std_types[MDT_UINT16].type_no = MDT_UINT16;
    std_types[MDT_INT32].type_no = MDT_INT32;
    std_types[MDT_UINT32].type_no = MDT_UINT32;
    std_types[MDT_SP_FLOAT].type_no = MDT_SP_FLOAT;
    std_types[MDT_DP_FLOAT].type_no = MDT_DP_FLOAT;

    std_types[MDT_BYTE].def = &mdt_byte;
    std_types[MDT_HWORD].def = &mdt_hword;
    std_types[MDT_WORD].def = &mdt_word;
    std_types[MDT_DWORD].def = &mdt_dword;
    std_types[MDT_ADDRESS].def = &mdt_address;
    std_types[MDT_CHAR].def = &mdt_char;
    std_types[MDT_UCHAR].def = &mdt_uchar;
    std_types[MDT_INT8].def = &mdt_int8;
    std_types[MDT_UINT8].def = &mdt_uint8;
    std_types[MDT_INT16].def = &mdt_int16;
    std_types[MDT_UINT16].def = &mdt_uint16;
    std_types[MDT_INT32].def = &mdt_int32;
    std_types[MDT_UINT32].def = &mdt_uint32;
    std_types[MDT_SP_FLOAT].def = &mdt_sp_float;
    std_types[MDT_DP_FLOAT].def = &mdt_dp_float;

    std_types[MDT_BYTE].def_size = 1;
    std_types[MDT_HWORD].def_size = 1;
    std_types[MDT_WORD].def_size = 1;
    std_types[MDT_DWORD].def_size = 1;
    std_types[MDT_ADDRESS].def_size = 1;
    std_types[MDT_CHAR].def_size = 1;
    std_types[MDT_UCHAR].def_size = 1;
    std_types[MDT_INT8].def_size = 1;
    std_types[MDT_UINT8].def_size = 1;
    std_types[MDT_INT16].def_size = 1;
    std_types[MDT_UINT16].def_size = 1;
    std_types[MDT_INT32].def_size = 1;
    std_types[MDT_UINT32].def_size = 1;
    std_types[MDT_SP_FLOAT].def_size = 1;
    std_types[MDT_DP_FLOAT].def_size = 1;

    std_types[MDT_BYTE].idr_size = sizeof(pds_byte_t);
    std_types[MDT_HWORD].idr_size = sizeof(pds_half_word_t);
    std_types[MDT_WORD].idr_size = sizeof(pds_word_t);
    std_types[MDT_DWORD].idr_size = sizeof(pds_double_word_t);
    std_types[MDT_ADDRESS].idr_size = sizeof(pds_address_t);
    std_types[MDT_CHAR].idr_size = sizeof(char);
    std_types[MDT_UCHAR].idr_size = sizeof(unsigned char);
    std_types[MDT_INT8].idr_size = sizeof(pds_int8);
    std_types[MDT_UINT8].idr_size = sizeof(pds_uint8);
    std_types[MDT_INT16].idr_size = sizeof(pds_int16);
    std_types[MDT_UINT16].idr_size = sizeof(pds_uint16);
    std_types[MDT_INT32].idr_size = sizeof(pds_int32);
    std_types[MDT_UINT32].idr_size = sizeof(pds_uint32);
    std_types[MDT_SP_FLOAT].idr_size = sizeof(pds_sp_float);
    std_types[MDT_DP_FLOAT].idr_size = sizeof(pds_dp_float);

    xdrmem_create(&xdrs,(const caddr_t) buffer,
		  (const u_int) MDT_BUF_SIZE, 
                  XDR_ENCODE);

    pos_old = 0;

    if (!xdr_pds_byte(&xdrs,&idr_byte)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_BYTE].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_half_word(&xdrs,&idr_hword)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_HWORD].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_word(&xdrs,&idr_word)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_WORD].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_double_word(&xdrs,&idr_dword)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_DWORD].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_address(&xdrs,&idr_address)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_ADDRESS].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_char(&xdrs,&idr_char)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_CHAR].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_u_char(&xdrs,&idr_uchar)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_UCHAR].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_int8(&xdrs,&idr_int8)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_INT8].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_u_int8(&xdrs,&idr_uint8)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_UINT8].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_int16(&xdrs,&idr_int16)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_INT16].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_u_int16(&xdrs,&idr_uint16)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_UINT16].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_int32(&xdrs,&idr_int32)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_INT32].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_u_int32(&xdrs,&idr_uint32)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_UINT32].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_sp_float(&xdrs,&idr_sp_float)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_SP_FLOAT].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_dp_float(&xdrs,&idr_dp_float)) return(PDS_NORESOURCES); 
    pos_new = xdr_getpos(&xdrs);
    std_types[MDT_DP_FLOAT].xdr_size = pos_new - pos_old;

    xdr_destroy(&xdrs);

    l_mutex_init(&mdt_mutex);

    mdt_initialised = 1;

    return(PDS_OK);
}


bool_t 
pds_type_xdr(xdrs,msg_type,msg_data)
    XDR * xdrs;
    msg_type_t msg_type;
    msg_data_t * msg_data;
{
    msg_typedef_t * msg_typedef;
    mdt_t * mdt;

#if !defined(TRUSTED)
    if ((!xdrs) || (!msg_data))
        return((bool_t) 0);
#endif

    mdt_assert(mdt_initialised);

    if (get_mdt(msg_type,&mdt) != PDS_OK)
        return((bool_t) 0);

    msg_typedef = mdt->def;
    if (!xdr_msg_opaque(xdrs,&msg_typedef,&msg_data))
        return((bool_t) 0);

    return((bool_t) 1);
}


bool_t 
pds_msg_xdr(xdrs,msg_type,msg_count,msg_data)
    XDR * xdrs;
    msg_type_t msg_type;
    msg_count_t msg_count;
    msg_data_t * msg_data;
{
    msg_typedef_t * msg_typedef;
    mdt_t * mdt;

#if !defined(TRUSTED)
    if ((!xdrs) || (!msg_data))
        return((bool_t) 0);
#endif

    mdt_assert(mdt_initialised);

    if (get_mdt(msg_type,&mdt) != PDS_OK)
        return((bool_t) 0);

    while (msg_count > 0) {
        msg_typedef = mdt->def;
	if (!xdr_msg_opaque(xdrs,&msg_typedef,&msg_data))
	    return((bool_t) 0);
	msg_count--;
    }

    return((bool_t) 1);
}


pds_ret_t
pds_type_size(msg_type,size,option)
    msg_type_t msg_type;
    pds_size_t * size;
    msg_option_t option;
{
    mdt_t * mdt;
    pds_ret_t pret;

#if !defined(TRUSTED)
    if (!size)
	return(PDS_INVAL);
#endif

    mdt_assert(mdt_initialised);

    if ((pret = get_mdt(msg_type,&mdt)) != PDS_OK)
	return(pret);

    if (option == MDT_IDR)
	*size = mdt->idr_size;
#if !defined(TRUSTED)
    else if (option != MDT_XDR)
	return(PDS_INVAL);
#endif
    else  
	*size = mdt->xdr_size;

    return(PDS_OK);
}


pds_ret_t 
pds_type_define(msg_intfc_no,msg_type_no,msg_typedef,msg_type)
    msg_intfc_no_t msg_intfc_no;
    msg_type_no_t msg_type_no;
    msg_typedef_t * msg_typedef;
    msg_type_t * msg_type;
{
    unsigned intfc_index;
    unsigned type_index;
    mdt_t * mdt;
    pds_ret_t pret;
    msg_typedef_t * msg_typedef_start;
    unsigned idr_align;
    unsigned array_align;
    unsigned idr_size;
    unsigned xdr_size;
    unsigned def_size;
    unsigned array_size;
    msg_typedef_t * definition;
    msg_typedef_t * def;
    msg_typedef_t * array_def;
    char * address;
    unsigned pad_size;
    unsigned multiplier;
    int mdt_structure;
    int pad_done;

#if !defined(TRUSTED)
    if ((!msg_typedef) || (!msg_type))
	return(PDS_INVAL);
#endif

    if (!msg_intfc_no || (msg_intfc_no > MSG_INTFC_NO_MAX) || 
        !msg_type_no || (msg_type_no > MSG_TYPE_NO_MAX))
	return(PDS_INVAL);

    mdt_assert(mdt_initialised);

    if (msg_typedef[0] != MDT_BEGIN)   /* invalid type definition */
	return(PDS_INVAL);
    if (msg_typedef[1] == MDT_END)     /* empty type definition   */
	return(PDS_INVAL);

    mdt_structure = 0;
    msg_typedef_start = msg_typedef;
    msg_typedef++;

    /* 
    ** Sanity check and size estimation of message type definition.
    */

    def_size = 0;

    while (*msg_typedef != MDT_END) {
	switch (*msg_typedef) {
	    case MDT_BEGIN :
		return(PDS_INVAL);
	    case MDT_STRUCT_OPEN :
		if (*(msg_typedef-1) != MDT_BEGIN)
		    return(PDS_INVAL);
		if (*(msg_typedef+1) == MDT_STRUCT_CLOSE) /* empty structure */
		    return(PDS_INVAL);
		if (mdt_structure)      /* nested structure */
		    return(PDS_INVAL);
		mdt_structure = 1;
		def_size++;		/* MDT_STRUCT_OPEN  */
		break;
	    case MDT_STRUCT_CLOSE :
		if (*(msg_typedef+1) != MDT_END)
		    return(PDS_INVAL);
		if (!mdt_structure)     /* bracket mismatch */
		    return(PDS_INVAL);
    		def_size += 2;		/* pad definition   */
		def_size++;		/* MDT_STRUCT_CLOSE */
		break;
	    case MDT_ARRAY_OF :
	        if ((*(msg_typedef+2) == MDT_END) ||
		    (*(msg_typedef+2) == MDT_STRUCT_CLOSE)) /* invalid array */
		    return(PDS_INVAL);
		def_size ++;		/* MDT_ARRAY_OF     */
	    	msg_typedef++;
	    	def_size ++;		/* array count      */
		break;
	    default :
		pret = get_mdt(*msg_typedef,&mdt);
    		if (pret != PDS_OK)
        	    return(pret);
		if ((mdt_structure) && (mdt->idr_align > 1))
		    def_size += 2;	    /* pad definition     */
		def_size += mdt->def_size;  /* subtype definition */
		break;
	}
	msg_typedef++;
    }

    /* allocate memory for temporary message type definition */
    definition = (msg_typedef_t *) 
	         pds_mem_alloc(&mdt_phd,def_size * sizeof(msg_typedef_t));
    if (!definition)
	return(PDS_NOMEMORY);

    /* 
    ** Generate message type definition 
    */

    def = definition;
    msg_typedef = msg_typedef_start+1;

    idr_align = 1;
    multiplier = 1;
    mdt_structure = 0;
    pad_done = 0;
    address = (char *) 0;

    while (*msg_typedef != MDT_END) {
	switch (*msg_typedef) {
	    case MDT_STRUCT_OPEN :
    		mdt_structure = 1;
		*def++ = *msg_typedef;
		break;
	    case MDT_STRUCT_CLOSE :
		/* insert definition for possible pad */
    		padalign(&address,idr_align,&pad_size,MDT_DECODE);
		if (pad_size > 0) {
		    *def++ = MDT_PAD;
		    *def++ = pad_size;
		}
		*def++ = *msg_typedef;
		break;
	    case MDT_ARRAY_OF :
		/* insert definition for possible pad */
		if (mdt_structure && !pad_done) {
		    array_def = msg_typedef;
		    pret = idrmsg_array_info(&array_def,&array_size,&array_align);
		    if (pret != PDS_OK)
		        return(pret);
		    if (array_align > 1) {
    		        padalign(&address,array_align,&pad_size,MDT_DECODE);
		        if (pad_size > 0) {
		            *def++ = MDT_PAD;
		            *def++ = pad_size;
		        }
		    }
		    pad_done = 1;
		}

		*def++ = *msg_typedef;
		/* append array count */
	        *def++ = *++msg_typedef;
		multiplier *= *msg_typedef;
		break;
	    default :
		pret = get_mdt(*msg_typedef,&mdt);
		mdt_assert(pret == PDS_OK);

		/* inherit alignment restrictions */
		if (mdt->idr_align > idr_align)
		    idr_align = mdt->idr_align;

		/* insert definition for possible pad */
		if (!pad_done && mdt_structure && (mdt->idr_align > 1)) {
    		    padalign(&address,mdt->idr_align,&pad_size,MDT_DECODE);
		    if (pad_size > 0) {
		        *def++ = MDT_PAD;
		        *def++ = pad_size;
		    }
		}

		/* append subtype definition */
		(void) memcpy((char *) def,
	       	      	      (char *) mdt->def,
	       	      	      mdt->def_size * sizeof(msg_typedef_t));
		def = (msg_typedef_t *) def + mdt->def_size;
		address += multiplier * mdt->idr_size;

		multiplier = 1;
		pad_done = 0;

		break;
	}
	msg_typedef++;
    }

    idr_size = address - (char *) 0;

    mdt_assert((def-definition) <= def_size);

    def_size = def-definition;

    /* allocate memory for message type definition */
    def = (msg_typedef_t *) 
	  pds_mem_alloc(&mdt_phd,def_size * sizeof(msg_typedef_t));
    if (!def)
	return(PDS_NOMEMORY);

    /* copy message type definition */
    (void) memcpy((char *) def,
	       	  (char *) definition,
	       	  def_size * sizeof(msg_typedef_t));

    /* free memory for temporary message type definition */
    pds_mem_free(&mdt_phd, (void_ptr) definition);

    definition = def;

#if !defined(NDEBUG)
    {
        unsigned mdt_align;
        unsigned mdt_size;

        def = definition;
        pret = idrmsg_opaque_info(&def,&mdt_size,&mdt_align);

	mdt_assert(pret == PDS_OK);
        mdt_assert(mdt_align == idr_align);
        mdt_assert(mdt_size == idr_size);
    }
#endif

    /* derive xdr_size */
    def = definition;
    if ((pret = xdrmsg_opaque_info(&def,&xdr_size)) != PDS_OK) {
	pds_mem_free(&mdt_phd, (void_ptr) definition);
        return(pret);
    }

    MsgDD_Lock();
    *msg_type = 0x10000 * msg_intfc_no + msg_type_no;
    pret = get_free_index(*msg_type,&intfc_index,&type_index);

    if (pret != PDS_OK) {
        pds_mem_free(&mdt_phd, (void_ptr) definition);
        MsgDD_Unlock();
	return(PDS_IMPLIM);
    }
    else {
	mdt = &msg_intfcs[intfc_index].msg_types[type_index];
        mdt->type_no = msg_type_no;
        mdt->xdr_size = xdr_size;
        mdt->idr_size = idr_size;
        mdt->idr_align = idr_align;
        mdt->def_size = def_size;
        mdt->def = definition;
        MsgDD_Unlock();
        return(PDS_OK);
    }
}

