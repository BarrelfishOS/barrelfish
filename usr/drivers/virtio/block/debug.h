/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VBLOCK_DEBUG_H_
#define VBLOCK_DEBUG_H_

#define VBLOCK_DEBUG_HOST_ENABLED 1
#define VBLOCK_DEBUG_GUEST_ENABLED 1

#define VBLOCK_DEBUG_SVC_ENABLED 1
#define VBLOCK_DEBUG_REQ_ENABLED 1
#define VBLOCK_DEBUG_DEV_ENABLED 1


#ifdef __VIRTIO_HOST__
#define VBLOCK_DEBUG_HOST VBLOCK_DEBUG_HOST_ENABLED
#define VBLOCK_DEBUG_GUEST 0
#else
#define VBLOCK_DEBUG_HOST 0
#define VBLOCK_DEBUG_GUEST VBLOCK_DEBUG_GUEST_ENABLED
#endif


#if VBLOCK_DEBUG_HOST
#define VBLOCK_DEBUG_PRINT_HOST(msg...) debug_printf(msg)
#else
#define VBLOCK_DEBUG_PRINT_HOST(msg...)
#endif

#if VBLOCK_DEBUG_GUEST
#define VBLOCK_DEBUG_PRINT_GUEST(msg...) debug_printf(msg)
#else
#define VBLOCK_DEBUG_PRINT_GUEST(msg...)
#endif

#define VBLOCK_DEBUG_PRINT(msg...) VBLOCK_DEBUG_PRINT_HOST(msg) \
                                   VBLOCK_DEBUG_PRINT_GUEST(msg)

#ifdef VBLOCK_DEBUG_DEV_ENABLED
#define VBLOCK_DEBUG_DEV(msg...) VBLOCK_DEBUG_PRINT("[dev] " msg)
#else
#define VBLOCK_DEBUG_DEV(msg...)
#endif

#ifdef VBLOCK_DEBUG_REQ_ENABLED
#define VBLOCK_DEBUG_REQ(msg...) VBLOCK_DEBUG_PRINT("[req] " msg)
#else
#define VBLOCK_DEBUG_REQ(msg...)
#endif

#ifdef VBLOCK_DEBUG_SVC_ENABLED
#define VBLOCK_DEBUG_SVC(msg...) VBLOCK_DEBUG_PRINT("[svc] " msg)
#else
#define VBLOCK_DEBUG_SVC(msg...)
#endif

#endif /* VBLOCK_DEBUG_H_ */
