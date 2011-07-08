/*
 * Internal definitions used by Flounder generated code
 *
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE
 * file. If you do not find this file, copies can be found by
 * writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich.
 *  Attn: Systems Group.
 *
 */

#ifndef __INTERNAL_H
#define __INTERNAL_H

#include <stdlib.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/dispatcher_shared.h> // for DISP_NAME_LEN
#include <barrelfish/flounder_glue_binding.h>

#define FL_MARSHAL_uint8(_msg,_val) (msgbuf_marshall_uint8(_msg,_val))
#define FL_MARSHAL_int8(_msg,_val) (msgbuf_marshall_int8(_msg,_val))
#define FL_MARSHAL_uint16(_msg,_val) (msgbuf_marshall_uint16(_msg,_val))
#define FL_MARSHAL_uint32(_msg,_val) (msgbuf_marshall_uint32(_msg,_val))
#define FL_MARSHAL_uint64(_msg,_val) (msgbuf_marshall_uint64(_msg,_val))
#define FL_MARSHAL_int32(_msg,_val) (msgbuf_marshall_int32(_msg,_val))
#define FL_MARSHAL_uintptr(_msg,_val) (msgbuf_marshall_uintptr(_msg,_val))
#define FL_MARSHAL_size(_msg,_val) (msgbuf_marshall_size(_msg,_val))
#define FL_MARSHAL_string(_msg,_val) (msgbuf_marshall_string(_msg,_val))
#define FL_MARSHAL_buffer(_msg, _val, _size) (msgbuf_marshall_buffer(_msg,_val,_size))
#define FL_MARSHAL_float(_msg,_val) (msgbuf_marshall_float(_msg,_val))
#define FL_MARSHAL_cap(_msg,_val) (msgbuf_marshall_cap(_msg,_val))
#define FL_MARSHAL_iref(_msg,_val) (msgbuf_marshall_iref(_msg,_val))

#define FL_UNMARSHAL_uint8(_msg,_val) (msgbuf_unmarshall_uint8(_msg,_val))
#define FL_UNMARSHAL_int8(_msg,_val) (msgbuf_unmarshall_int8(_msg,_val))
#define FL_UNMARSHAL_uint16(_msg,_val) (msgbuf_unmarshall_uint16(_msg,_val))
#define FL_UNMARSHAL_uint32(_msg,_val) (msgbuf_unmarshall_uint32(_msg,_val))
#define FL_UNMARSHAL_uint64(_msg,_val) (msgbuf_unmarshall_uint64(_msg,_val))
#define FL_UNMARSHAL_int32(_msg,_val) (msgbuf_unmarshall_int32(_msg,_val))
#define FL_UNMARSHAL_uintptr(_msg,_val) (msgbuf_unmarshall_uintptr(_msg,_val))
#define FL_UNMARSHAL_size(_msg,_val) (msgbuf_unmarshall_size(_msg,_val))
#define FL_UNMARSHAL_string(_msg,_val) (msgbuf_unmarshall_string(_msg,_val))
#define FL_UNMARSHAL_buffer(_msg, _val) (msgbuf_unmarshall_buffer(_msg,_val))
#define FL_UNMARSHAL_float(_msg,_val) (msgbuf_unmarshall_float(_msg,_val))
#define FL_UNMARSHAL_cap(_msg,_val) (msgbuf_unmarshall_cap(_msg,_val))
#define FL_UNMARSHAL_iref(_msg,_val) (msgbuf_unmarshall_iref(_msg,_val))

#if defined(FLOUNDER_FAILED_DEBUG) || defined(FLOUNDER_DEBUG)
#include <barrelfish/dispatch.h>
# define FL_FAILED_DEBUG(msg...) { \
        printf("Flounder detected failure in %.*s %s:%d",       \
               DISP_NAME_LEN, disp_name(), __FILE__, __LINE__); \
        printf(" " msg); printf("\n"); }
#else
# define FL_FAILED_DEBUG(msg...)    ((void)0)
#endif

#if defined(FLOUNDER_DEBUG)
# define FL_DEBUG_PRINT_TYPE(x) \
        printf("Flounder: %s on core %d got message type %" PRIu16 "\n", \
               __FILE__, disp_get_core_id(), x)
#else
# define FL_DEBUG_PRINT_TYPE(x)     ((void)0)
#endif

extern void flounder_glue_error_handler(void *st, errval_t err);

#endif // __INTERNAL_H
