/**
 * \file
 * \brief lwIP architecture configuration file for Barrelfish.
 */

/*
 * Copyright (c) 2007, 2008, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CC_H
#define CC_H

#include <string.h>
#include <stdint.h>

/* Define platform endianness */
#ifndef BYTE_ORDER
#define BYTE_ORDER LITTLE_ENDIAN
#endif /* BYTE_ORDER */

typedef uint8_t         u8_t;
typedef int8_t          s8_t;
typedef uint16_t        u16_t;
typedef int16_t         s16_t;
typedef uint32_t        u32_t;
typedef int32_t         s32_t;
typedef uintptr_t       mem_ptr_t;

// (sn)printf() format strings for integer types
#define U16_F "hu"
#define S16_F "hd"
#define X16_F "hx"
#define U32_F "u"
#define S32_F "d"
#define X32_F "x"

/* Compiler hints for packing structures */
#if __GNUC__ >= 4
/* GCC 4.1 doesn't like '__attribute__((packed))' after a field declaration */
#define PACK_STRUCT_FIELD(x) x 
#define PACK_STRUCT_STRUCT __attribute__((packed))
#define PACK_STRUCT_BEGIN
#define PACK_STRUCT_END
#else
#define PACK_STRUCT_FIELD(x) x __attribute__((packed)) 
#define PACK_STRUCT_STRUCT __attribute__((packed))
#define PACK_STRUCT_BEGIN
#define PACK_STRUCT_END
#endif /* __GNUC__ */

#define INLINE  inline

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#define LWIP_PLATFORM_DIAG(x)   do {printf x;} while(0)
#define LWIP_PLATFORM_ASSERT(x) do {printf("Assertion \"%s\" failed at line %d in %s\n", \
                                     x, __LINE__, __FILE__); fflush(NULL); abort();} while(0)

#endif
