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
**        File: msgdd.c
**      Author: Kees Schuerman
**      SccsId: "@(#)msgdd.c	1.4 4/6/95"
** Description: Message Data Description
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
** mdt_define(). Its input is a message data type definition using
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
** MDT_SP_FLOAT		    float f1;
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
**    float f1;
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
** message data type identifier. The standard message data types have 
** globally unique type identifiers. The application defined data types
** may (and will) however be defined multiple times (and have multiple
** associated type identifiers), i.e. once in every process where the 
** message data type is being used. It is important to note that a data 
** type identifier of an application defined message data type is only 
** valid in the process that defined the type. Global uniqueness of these 
** message type identifiers is not really necessary but can be quite useful 
** for debugging purposes. This uniqueness is guaranteed when every process 
** initialised the type system (i.e. invokes msgdd_init()) with a globally 
** unique message data type key. 
**
**
** Message Data Type Table
** -----------------------
** Message data type defintions are stored in the message data type 
** table. The identifier of the standard message data types can be used 
** as index to retrieve type related information from the table. The 
** message data type index of the application defined message data types 
** is computed by subtracting an offset (message data type offset) from 
** their associated message type identifier. The message data type 
** offset is derived from the message data type key at initialisation 
** time. 
***********************************************************************/

#include "machine.h"    /* architecture specific constant definitions */

#include <stdio.h>
#include <stdlib.h>
#include <rpc/rpc.h>

#include "pds.types.h"
#include "pds.mem.h"
#include "pds.msg.h"
#include "pds.xdr.h"
#include "msgdd.h"


/**********************************************************************
** Configuration
***********************************************************************/

#define MSGDD_MAX_NOF_TYPES	256
#define MSGDD_INDEX_MAX 	MSGDD_MAX_NOF_TYPES - 1


/**********************************************************************
** Type Definitions
***********************************************************************/

typedef struct {
    unsigned int def_size;  /* size of message type definition           */
    unsigned int idr_align; /* alignment of internal data representation */
    unsigned int idr_size;  /* size of internal data representation      */
    unsigned int xdr_size;  /* size of external data representation      */
    msg_typedef_t * def;    /* message type definition                   */
} msgdd_type_t;		    /* entry of message data type table		 */


/**********************************************************************
** Some Global Variables 
***********************************************************************/

static pds_heap_descriptor_t msgdd_phd;		      /* private heap */

static int msgdd_initialised = 0;  		      /* state	      */

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

static l_mutex_t msgdd_mutex; 		       /* protects type table */

#define MsgDD_Lock()	l_mutex_lock(&msgdd_mutex)
#define MsgDD_Unlock()	l_mutex_unlock(&msgdd_mutex)

static msgdd_type_t msg_types[MSGDD_MAX_NOF_TYPES];     /* type table */

static msg_type_t msgdd_index_max;
static msg_type_t msgdd_offset; 	     	


/**********************************************************************
** Runtime Consistency Checking
***********************************************************************/

#if defined(NDEBUG)
#define msgdd_assert(ex)
#else
#define msgdd_assert(ex) {                                      \
    if (!(ex)) {                                                \
        (void) fprintf(stderr,                                  \
               "PDS MPS-M Assertion Failed:");                  \
        (void) fprintf(stderr, " file \"%s\"", __FILE__);       \
        (void) fprintf(stderr, " line %d\n", __LINE__);         \
        exit(1);                                                \
    }                                                           \
}
#endif /* NDEBUG */
#define msgdd_assert_always() {                                 \
        (void) fprintf(stderr,                                  \
               "PDS MPS-M Assertion Failed:");                  \
        (void) fprintf(stderr, " file \"%s\"", __FILE__);       \
        (void) fprintf(stderr, " line %d\n", __LINE__);         \
        exit(1);                                                \
}




/**********************************************************************
**************************  Local Primitives  *************************
***********************************************************************/

#if defined(__STDC__)
static msg_type_t msg_index(msg_type_t msg_type);
static void padalign(char * * ptr, 
		     unsigned int align,
    		     msg_option_t option);
static void derive_alignments(void);
static pds_ret_t idrmsg_mdttype_info(msg_typedef_t * * msg_typedef,
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
static pds_ret_t xdrmsg_mdttype_info(msg_typedef_t * * msg_typedef,
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
static msg_type_t msg_index();
static void padalign();
static void derive_alignments();
static pds_ret_t idrmsg_mdttype_info();
static pds_ret_t idrmsg_array_info();
static pds_ret_t idrmsg_struct_info();
static pds_ret_t idrmsg_opaque_info();
static pds_ret_t xdrmsg_mdttype_info();
static pds_ret_t xdrmsg_array_info();
static pds_ret_t xdrmsg_struct_info();
static pds_ret_t xdrmsg_opaque_info();
static bool_t xdr_msg_stdtype();
static bool_t xdr_msg_array();
static bool_t xdr_msg_structure();
static bool_t xdr_msg_opaque();
#endif /* __STDC__ */


static msg_type_t 
msg_index(msg_type)
    msg_type_t msg_type;
{
    msg_type_t index;

    if ((msg_type >= MDT_STD_MIN) && (msg_type <= MDT_STD_MAX))
	return(msg_type);
    else {
	index = msg_type - msgdd_offset;
	if ((index <= msgdd_index_max) && (index > MDT_STD_MAX))
	    return(index);
	else
	    return(0);
    }
}


/* 
**   Primitive: padalign()
** Description: aligns ptr on <align>-byte boundary. When in encoding
**		mode, padding with nil-bytes takes place.
*/

static void
padalign(ptr,align,option)
    char * * ptr;
    unsigned int align;
    msg_option_t option;
{
    int i,count;

    msgdd_assert(align>0);

    count = (*ptr - (char *) 0) % align;
    if (count < 0)
	count = -count;
    else
	count = align - count;
    if (option == MSGDD_ENCODE) /* pad with zeros */
	for (i=0;i<count;i++)
	    (*ptr)[i] = 0;
    *ptr = *ptr + count;
}


/* 
**   Primitive: derive_alignments()
** Description: derives the alignment requirements of the standard 
**		message data types and records this in the message
**		data type table.
*/

static void
derive_alignments()
{
    struct {
	char c;
	pds_half_word_t t;
    } msgdd_hword;

    struct {
	char c;
	pds_word_t t;
    } msgdd_word;

    struct {
	char c;
	pds_double_word_t t;
    } msgdd_dword;

    struct {
	char c;
	pds_address_t t;
    } msgdd_address;

    struct {
	char c;
	pds_int8 t;
    } msgdd_int8;

    struct {
	char c;
	pds_uint8 t;
    } msgdd_uint8;

    struct {
	char c;
	pds_int16 t;
    } msgdd_int16;

    struct {
	char c;
	pds_uint16 t;
    } msgdd_uint16;

    struct {
	char c;
	pds_int32 t;
    } msgdd_int32;

    struct {
	char c;
	pds_uint32 t;
    } msgdd_uint32;

    struct {
	char c;
	float t;
    } msgdd_float;

    struct {
	char c;
	double t;
    } msgdd_double;

    msgdd_assert(sizeof(pds_byte_t) == 1);
    msgdd_assert(sizeof(char) == 1);
    msgdd_assert(sizeof(unsigned char) == 1);

    msg_types[MDT_BYTE].idr_align = 1;
    msg_types[MDT_HWORD].idr_align = (char *) &msgdd_hword - 
				       (char *) &msgdd_hword.t;
    msg_types[MDT_WORD].idr_align = (char *) &msgdd_word -
				      (char *) &msgdd_word.t;
    msg_types[MDT_DWORD].idr_align = (char *) &msgdd_dword - 
				       (char *) &msgdd_dword.t;
    msg_types[MDT_ADDRESS].idr_align = (char *) &msgdd_address - 
				         (char *) &msgdd_address.t;
    msg_types[MDT_CHAR].idr_align = 1;
    msg_types[MDT_UCHAR].idr_align = 1;
    msg_types[MDT_INT8].idr_align = (char *) &msgdd_int8 - 
				      (char *) &msgdd_int8.t;
    msg_types[MDT_UINT8].idr_align = (char *) &msgdd_uint8 -
				       (char *) &msgdd_uint8.t;
    msg_types[MDT_INT16].idr_align = (char *) &msgdd_int16 - 
				       (char *) &msgdd_int16.t;
    msg_types[MDT_UINT16].idr_align = (char *) &msgdd_uint16 - 
					(char *) &msgdd_uint16.t;
    msg_types[MDT_INT32].idr_align = (char *) &msgdd_int32 - 
				       (char *) &msgdd_int32.t;
    msg_types[MDT_UINT32].idr_align = (char *) &msgdd_uint32 - 
					(char *) &msgdd_uint32.t;
    msg_types[MDT_SP_FLOAT].idr_align = (char *) &msgdd_float - 
					  (char *) &msgdd_float.t;
    msg_types[MDT_DP_FLOAT].idr_align = (char *) &msgdd_double - 
					  (char *) &msgdd_double.t;
}


static pds_ret_t
idrmsg_mdttype_info(msg_typedef,size,align)
    msg_typedef_t * * msg_typedef;
    pds_size_t * size; 
    unsigned int * align;
{
    msg_type_t index;

    if (!(index = msg_index(**msg_typedef)))
	return(PDS_INVAL);
    *size = msg_types[index].idr_size;
    *align = msg_types[index].idr_align;
    (*msg_typedef)++;
    return(PDS_OK);
}


static pds_ret_t
xdrmsg_mdttype_info(msg_typedef,size)
    msg_typedef_t * * msg_typedef;
    pds_size_t * size; 
{
    msg_type_t index;

    if (!(index = msg_index(**msg_typedef)))
	return(PDS_INVAL);
    *size = msg_types[index].xdr_size;
    (*msg_typedef)++;
    return(PDS_OK);
}


static pds_ret_t
idrmsg_array_info(msg_typedef,size,align)
    msg_typedef_t * * msg_typedef;
    pds_size_t * size;
    unsigned int * align;
{
    pds_uint32 array_count;
    pds_size_t element_size;
    pds_ret_t mret;

    msgdd_assert(**msg_typedef == MDT_ARRAY_OF);

    /* get array_count */
    (*msg_typedef)++;
    array_count = (pds_uint32) **msg_typedef;

    /* get element_size and alignment */ 
    (*msg_typedef)++;
    if ((mret = idrmsg_opaque_info(msg_typedef,&element_size,align)) != PDS_OK)
	return(mret);
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
    pds_ret_t mret;

    msgdd_assert(**msg_typedef == MDT_ARRAY_OF);

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
        if ((mret = xdrmsg_opaque_info(msg_typedef,&element_size)) != PDS_OK)
	    return(mret);
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
    unsigned int field_align;
    pds_size_t field_size;
    char * field_address;
    unsigned int struct_align;

    msgdd_assert(**msg_typedef == MDT_STRUCT_OPEN);

    field_address = (char *) 0;
    struct_align = 1;
    (*msg_typedef)++;
    while (**msg_typedef != MDT_STRUCT_CLOSE) {
	idrmsg_opaque_info(msg_typedef,&field_size,&field_align);
	padalign(&field_address,field_align,MSGDD_DECODE);
	field_address += field_size;
	if (field_align > struct_align)
	    struct_align = field_align;
    }
    padalign(&field_address,struct_align,MSGDD_DECODE);
    *size = field_address - (char *) 0;
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

    msgdd_assert(**msg_typedef == MDT_STRUCT_OPEN);

    struct_size = 0;
    (*msg_typedef)++;
    while (**msg_typedef != MDT_STRUCT_CLOSE) {
	xdrmsg_opaque_info(msg_typedef,&field_size);
	struct_size += field_size;
    }
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
        default :
	    return(idrmsg_mdttype_info(msg_typedef,size,align));
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
        default :
	    return(xdrmsg_mdttype_info(msg_typedef,size));
    }
}


static bool_t
xdr_msg_stdtype(xdrs,msg_typedef,msg_data)
    XDR * xdrs;
    msg_typedef_t * * msg_typedef;
    msg_data_t * * msg_data;
{
    msg_type_t msg_type;
    msg_option_t option;

    if (xdrs->x_op == XDR_ENCODE)
        option = MSGDD_ENCODE;
    else
        option = MSGDD_DECODE;

    /* pad message data and align message data pointer */
    padalign((char * *) msg_data,
	     msg_types[**msg_typedef].idr_align,
	     option);

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
	    if (xdr_char(xdrs,(char *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(char));
		return(1);
	    } else return(0);
	case MDT_UCHAR :
	    if (xdr_u_char(xdrs,(unsigned char *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(unsigned char));
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
	    if (xdr_float(xdrs,(float *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(float));
		return(1);
	    } else return(0);
	case MDT_DP_FLOAT :
	    if (xdr_double(xdrs,(double *) *msg_data)) {
		*msg_data = (msg_data_t *)
			    ((char *) *msg_data + sizeof(double));
		return(1);
	    } else return(0);
	default :
	    return(0);
    }
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

    msgdd_assert(**msg_typedef == MDT_ARRAY_OF);

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
    msgdd_assert(**msg_typedef == MDT_STRUCT_OPEN);

    (*msg_typedef)++;
    while (**msg_typedef != MDT_STRUCT_CLOSE)
	if (!xdr_msg_opaque(xdrs,msg_typedef,msg_data))
	    return(0);
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
    	default :
	    return(xdr_msg_stdtype(xdrs,msg_typedef,msg_data));
    }
}




/**********************************************************************
************************  Exported Primitives  ************************
***********************************************************************/

#define MSGDD_BUF_SIZE 512

pds_ret_t
msgdd_init(key)
    msg_type_t key;
{
    pds_byte_t idr_byte;
    pds_half_word_t idr_hword;
    pds_word_t idr_word;
    pds_double_word_t idr_dword;
    pds_address_t idr_address;
    char idr_char;
    unsigned char idr_uchar;
    pds_int8 idr_int8;
    pds_uint8 idr_uint8;
    pds_int16 idr_int16;
    pds_uint16 idr_uint16;
    pds_int32 idr_int32;
    pds_uint32 idr_uint32;
    float idr_float;
    double idr_double;

    char buffer[MSGDD_BUF_SIZE];
    unsigned int i,pos_old,pos_new;
    XDR xdrs;

/*    if (key > MSG_TYPE_MAX / MSGDD_MAX_NOF_TYPES)
	return(PDS_INVAL);*/
    msgdd_offset = key * MSG_TYPE_MAX;

    derive_alignments();

    msgdd_assert(sizeof(msg_count_t) == sizeof(msg_typedef_t));

    /* private heap for message type definitions */
    if (pds_mem_init((char *) 0,(char *) 0,0,&msgdd_phd,1) == (char *) -1)
        return(PDS_NOMEMORY);

    for (i=0;i<MSGDD_MAX_NOF_TYPES;i++) {
	msg_types[i].def = (msg_typedef_t *) 0;
	msg_types[i].def_size = 0;
	msg_types[i].idr_align = 1;
	msg_types[i].idr_size = 0;
	msg_types[i].xdr_size = 0;
    }

    msg_types[MDT_BYTE].def = &mdt_byte;
    msg_types[MDT_HWORD].def = &mdt_hword;
    msg_types[MDT_WORD].def = &mdt_word;
    msg_types[MDT_DWORD].def = &mdt_dword;
    msg_types[MDT_ADDRESS].def = &mdt_address;
    msg_types[MDT_CHAR].def = &mdt_char;
    msg_types[MDT_UCHAR].def = &mdt_uchar;
    msg_types[MDT_INT8].def = &mdt_int8;
    msg_types[MDT_UINT8].def = &mdt_uint8;
    msg_types[MDT_INT16].def = &mdt_int16;
    msg_types[MDT_UINT16].def = &mdt_uint16;
    msg_types[MDT_INT32].def = &mdt_int32;
    msg_types[MDT_UINT32].def = &mdt_uint32;
    msg_types[MDT_SP_FLOAT].def = &mdt_sp_float;
    msg_types[MDT_DP_FLOAT].def = &mdt_dp_float;

    msg_types[MDT_BYTE].def_size = sizeof(msg_typedef_t);
    msg_types[MDT_HWORD].def_size = sizeof(msg_typedef_t);
    msg_types[MDT_WORD].def_size = sizeof(msg_typedef_t);
    msg_types[MDT_DWORD].def_size = sizeof(msg_typedef_t);
    msg_types[MDT_ADDRESS].def_size = sizeof(msg_typedef_t);
    msg_types[MDT_CHAR].def_size = sizeof(msg_typedef_t);
    msg_types[MDT_UCHAR].def_size = sizeof(msg_typedef_t);
    msg_types[MDT_INT8].def_size = sizeof(msg_typedef_t);
    msg_types[MDT_UINT8].def_size = sizeof(msg_typedef_t);
    msg_types[MDT_INT16].def_size = sizeof(msg_typedef_t);
    msg_types[MDT_UINT16].def_size = sizeof(msg_typedef_t);
    msg_types[MDT_INT32].def_size = sizeof(msg_typedef_t);
    msg_types[MDT_UINT32].def_size = sizeof(msg_typedef_t);
    msg_types[MDT_SP_FLOAT].def_size = sizeof(msg_typedef_t);
    msg_types[MDT_DP_FLOAT].def_size = sizeof(msg_typedef_t);

    msg_types[MDT_BYTE].idr_size = sizeof(pds_byte_t);
    msg_types[MDT_HWORD].idr_size = sizeof(pds_half_word_t);
    msg_types[MDT_WORD].idr_size = sizeof(pds_word_t);
    msg_types[MDT_DWORD].idr_size = sizeof(pds_double_word_t);
    msg_types[MDT_ADDRESS].idr_size = sizeof(pds_address_t);
    msg_types[MDT_CHAR].idr_size = sizeof(char);
    msg_types[MDT_UCHAR].idr_size = sizeof(unsigned char);
    msg_types[MDT_INT8].idr_size = sizeof(pds_int8);
    msg_types[MDT_UINT8].idr_size = sizeof(pds_uint8);
    msg_types[MDT_INT16].idr_size = sizeof(pds_int16);
    msg_types[MDT_UINT16].idr_size = sizeof(pds_uint16);
    msg_types[MDT_INT32].idr_size = sizeof(pds_int32);
    msg_types[MDT_UINT32].idr_size = sizeof(pds_uint32);
    msg_types[MDT_SP_FLOAT].idr_size = sizeof(float);
    msg_types[MDT_DP_FLOAT].idr_size = sizeof(double);

    xdrmem_create(&xdrs,(const caddr_t) buffer,
		  (const u_int) MSGDD_BUF_SIZE, 
                  XDR_ENCODE);

    pos_old = 0;

    if (!xdr_pds_byte(&xdrs,&idr_byte)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_BYTE].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_half_word(&xdrs,&idr_hword)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_HWORD].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_word(&xdrs,&idr_word)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_WORD].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_double_word(&xdrs,&idr_dword)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_DWORD].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_address(&xdrs,&idr_address)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_ADDRESS].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_char(&xdrs,&idr_char)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_CHAR].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_u_char(&xdrs,&idr_uchar)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_UCHAR].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_int8(&xdrs,&idr_int8)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_INT8].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_u_int8(&xdrs,&idr_uint8)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_UINT8].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_int16(&xdrs,&idr_int16)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_INT16].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_u_int16(&xdrs,&idr_uint16)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_UINT16].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_int32(&xdrs,&idr_int32)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_INT32].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_pds_u_int32(&xdrs,&idr_uint32)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_UINT32].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_float(&xdrs,&idr_float)) return(PDS_NORESOURCES);
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_SP_FLOAT].xdr_size = pos_new - pos_old;
    pos_old = pos_new;

    if (!xdr_double(&xdrs,&idr_double)) return(PDS_NORESOURCES); 
    pos_new = xdr_getpos(&xdrs);
    msg_types[MDT_DP_FLOAT].xdr_size = pos_new - pos_old;

    xdr_destroy(&xdrs);

    msgdd_index_max = MDT_STD_MAX; 
    l_mutex_init(&msgdd_mutex);

    msgdd_initialised = 1;

    return(PDS_OK);
}


bool_t 
xdr_msg_data(xdrs,msg_type,msg_count,msg_data)
    XDR * xdrs;
    msg_type_t msg_type;
    msg_count_t msg_count;
    msg_data_t * msg_data;
{
    msg_typedef_t * msg_typedef;
    msg_type_t index;

#if !defined(TRUSTED)
    if ((!xdrs) || (!msg_data))
        return(PDS_INVAL);
#endif

    msgdd_assert(msgdd_initialised);

    if (!(index = msg_index(msg_type)))
	return(PDS_INVAL);

    do {
        msg_typedef = msg_types[index].def;
	if (!xdr_msg_opaque(xdrs,&msg_typedef,&msg_data))
	    return((bool_t) 0);
    } while (msg_count-- > 0);

    return((bool_t) 1);
}


pds_ret_t
msg_type_size(msg_type,size,option)
    msg_type_t msg_type;
    pds_size_t * size;
    msg_option_t option;
{
    msg_type_t index;

#if !defined(TRUSTED)
    if (!size)
	return(PDS_INVAL);
#endif

    msgdd_assert(msgdd_initialised);

    if (!(index = msg_index(msg_type)))
	return(PDS_INVAL);

    if (option == MSGDD_IDR)
	*size = msg_types[index].idr_size;
    else
	*size = msg_types[index].xdr_size;

    return(PDS_OK);
}


pds_ret_t 
msg_type_define(msg_typedef,msg_type)
    msg_typedef_t * msg_typedef;
    msg_type_t * msg_type;
{
    pds_ret_t mret;
    msg_type_t index;
    msg_typedef_t * msg_typedef_start;
    unsigned int idr_size;
    unsigned int idr_align;
    unsigned int xdr_size;
    unsigned int def_size;
    msg_typedef_t * definition;
    msg_typedef_t * def;
    int bracket_count;

#if !defined(TRUSTED)
    if ((!msg_typedef) || (!msg_type))
	return(PDS_INVAL);
#endif

    msgdd_assert(msgdd_initialised);

    if (msg_typedef[0] != MDT_BEGIN)   /* invalid type definition */
	return(PDS_INVAL);
    if (msg_typedef[1] == MDT_END)     /* empty type definition   */
	return(PDS_INVAL);
    
    if (msgdd_index_max >= MSGDD_INDEX_MAX)
	return(PDS_IMPLIM);

    bracket_count = 0;
    msg_typedef_start = msg_typedef;
    msg_typedef++;

    /* 
    ** Derive size of message type definition.
    */

    def_size = 0;

    do {
	switch (*msg_typedef) {
	    case MDT_BEGIN :
		return(PDS_INVAL);
	    case MDT_STRUCT_OPEN :
		if (*(msg_typedef+1) == MDT_STRUCT_CLOSE) /* empty structure */
		    return(PDS_INVAL);
		else {
		    bracket_count++;
		    break;
		}
	    case MDT_STRUCT_CLOSE :
		if (bracket_count<=0) /* bracket mismatch */
		    return(PDS_INVAL);
		else {
		    bracket_count--;
		    break;
		}
	    case MDT_ARRAY_OF :
	        if ((*(msg_typedef+2) == MDT_END) ||
		    (*(msg_typedef+2) == MDT_STRUCT_CLOSE)) /* invalid array */
		    return(PDS_INVAL);
		else { /* reserve space for array count */
	    	    msg_typedef++;
	    	    def_size += sizeof(msg_typedef_t);
		    break;
		}
	    default :
		if (!(index = msg_index(*msg_typedef)))
            	    return(PDS_INVAL);
		else { /* reserve space for subtype definition */
		    def_size += msg_types[index].def_size;
		    break;
		}
	}
    }
    while (*msg_typedef++ != MDT_END);

    if (bracket_count != 0) /* missing closing bracket(s) */
	return(PDS_INVAL);

    /* allocate memory for message type definition */
    definition = (msg_typedef_t *) pds_mem_alloc(&msgdd_phd,def_size);
    if (!definition)
	return(PDS_NOMEMORY);

    /* 
    ** Generate message type definition 
    */

    def = definition;
    msg_typedef = msg_typedef_start+1;

    do {

	index = msg_index(*msg_typedef);
	msgdd_assert(index);

	/* append subtype definition */
	(void) memcpy((char *) def,
	       	      (char *) msg_types[index].def,
	       	      msg_types[index].def_size);
	def = (msg_typedef_t *) 
	       ((char *) def + msg_types[index].def_size);

	/* append array count */
	if (*msg_typedef == MDT_ARRAY_OF) { 
	    msg_typedef++;
	    *def = *msg_typedef;
	    def++; 
	}

    } while ((*msg_typedef++) != MDT_END);
   
    msgdd_assert((def-definition)*sizeof(msg_typedef_t) == def_size);

    /* derive idr_size, idr_align, and xdr_size */
    def = definition;
    if ((mret = idrmsg_opaque_info(&def,&idr_size,&idr_align)) != PDS_OK) {
	pds_mem_free(&msgdd_phd, (void_ptr) definition);
        return(mret);
    } 
    def = definition;
    if ((mret = xdrmsg_opaque_info(&def,&xdr_size)) != PDS_OK) {
	pds_mem_free(&msgdd_phd, (void_ptr) definition);
        return(mret);
    }

    MsgDD_Lock();
    if (msgdd_index_max >= MSGDD_INDEX_MAX) {
        pds_mem_free(&msgdd_phd, (void_ptr) definition);
        MsgDD_Unlock();
	return(PDS_IMPLIM);
    }
    else {
        msgdd_index_max++;
        msg_types[msgdd_index_max].xdr_size = xdr_size;
        msg_types[msgdd_index_max].idr_size = idr_size;
        msg_types[msgdd_index_max].idr_align = idr_align;
        msg_types[msgdd_index_max].def_size = def_size;
        msg_types[msgdd_index_max].def = definition;
        *msg_type = msgdd_index_max + msgdd_offset;    
        MsgDD_Unlock();
        return(PDS_OK);
    }
}

