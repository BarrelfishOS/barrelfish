/**
 * \file net_constants.h
 * \brief File to hold the constants needed by network stack
 * across driver, userspace and control plane values.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_NETCONSTANTS_H
#define LIBBARRELFISH_NETCONSTANTS_H

#define FILTER_SERVICE_SUFFIX      "_FILT"

#define CTL_SERVICE_SUFFIX          "_CTRL"



#ifdef CONFIG_QEMU_NETWORK

#if !defined(__scc__)
/// Size of (static) heap memory
#ifndef MEM_SIZE
#define MEM_SIZE            (60*1024)
#endif // MEM_SIZE

/// Number of PBUF structs available
#ifndef MEMP_NUM_PBUF
#define MEMP_NUM_PBUF           128
#endif // MEMP_NUM_PBUF

/// Number of PBUF buffers available
#ifndef PBUF_POOL_SIZE
#define PBUF_POOL_SIZE          128
#endif // PBUF_POOL_SIZE

/* Used in the ethersrv.c and the driver. */
#define RECEIVE_BUFFERS 128
#define TRANSMIT_BUFFERS 128 //< Number of transmit descriptors
                              //< (must be multiple of 8)

/// the size of the pool
#ifndef PBUF_POOL_BUFSIZE
#define PBUF_POOL_BUFSIZE       (4 * 2048)
#endif // PBUF_POOL_BUFSIZE

/* from where the memory conf is coming? */
#ifndef MEM_CONF_LOC
#define MEM_CONF_LOC     "for_qemu"
#endif // MEM_CONF_LOC

#else // !defined(__scc__)

/************** SCC machine ******************/
/// Size of (static) heap memory
#ifndef MEM_SIZE
#define MEM_SIZE            (60*1024)
#endif // MEM_SIZE

/// Number of PBUF structs available
#ifndef MEMP_NUM_PBUF
#define MEMP_NUM_PBUF           2024
#endif // MEMP_NUM_PBUF

/// Number of PBUF buffers available
#ifndef PBUF_POOL_SIZE
#define PBUF_POOL_SIZE          2024
#endif // PBUF_POOL_SIZE

/* Used in the ethersrv.c and the driver. */
#define RECEIVE_BUFFERS 1024
#define TRANSMIT_BUFFERS 1024 //< Number of transmit descriptors
                              //< (must be multiple of 8)

/// the size of the pool
#ifndef PBUF_POOL_BUFSIZE
#define PBUF_POOL_BUFSIZE       (4 * 2048)
#endif // PBUF_POOL_BUFSIZE

/* from where the memory conf is coming? */
#ifndef MEM_CONF_LOC
#define MEM_CONF_LOC     "for_scc"
#endif // MEM_CONF_LOC


#endif // !defined(__scc__)


#else // CONFIG_QEMU_NETWORK
//  This is setup for n1000 card running on real hardware
#ifndef MEM_SIZE
#define MEM_SIZE                (60*1024*1024)
#endif // MEM_SIZE

/// Number of PBUF structs available
#ifndef MEMP_NUM_PBUF
#define MEMP_NUM_PBUF           16384
//#define MEMP_NUM_PBUF           128
#endif // MEMP_NUM_PBUF

/// Number of PBUF buffers available
#ifndef PBUF_POOL_SIZE
#define PBUF_POOL_SIZE         16384
//#define PBUF_POOL_SIZE          128
#endif // PBUF_POOL_SIZE

/* NOTE: This value should be bigger than NR_PREALLOCATED_PBUFS of lwipopts.h */
/* Used in the ethersrv.c and the driver. */

//#define RECEIVE_BUFFERS    ((PBUF_POOL_SIZE) / 2)
#define RECEIVE_BUFFERS     2000
#define TRANSMIT_BUFFERS (800 * 8) //< Number of transmit descriptors
                              //< (must be multiple of 8)

/// the size of the pool
#ifndef PBUF_POOL_BUFSIZE
#define PBUF_POOL_BUFSIZE       (1600)
#endif // PBUF_POOL_BUFSIZE

/* from where the memory conf is coming? */
#ifndef MEM_CONF_LOC
#define MEM_CONF_LOC     "BIG"
#endif // MEM_CONF_LOC

#endif // CONFIG_QEMU_NETWORK

/* NOTE: This value should be smaller than RECEIVE_BUFFERS of ethersrv.h */
#define NR_PREALLOCATED_PBUFS       ( RECEIVE_BUFFERS - 1)


#endif // LIBBARRELFISH_NETCONSTANTS_H
