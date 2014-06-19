/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_DEBUG_H_
#define VIRTIO_DEBUG_H_

/// Global VirtIO debug switch
#define VIRTIO_DEBUG_ENABLED 1

/// Enables the virtqueue debugging
#define VIRTIO_DEBUG_VQ_ENABLED 1

/// Enables the VirtIO general device debugging messages
#define VIRTIO_DEBUG_DEV_ENABLED 1

/// Enables the VirtIO device specific debugging messages
#define VIRTIO_DEBUG_DT_ENABLED 1

/// Enables the VirtIO transport layer debug messages
#define VIRTIO_DEBUG_TL_ENABLED 1

/// Enables the VirtIO management layer debug messages
#define VIRTIO_DEBUG_CHAN_ENABLED 1

#define VIRTIO_DEBUG_BUF_ENABELD 1

#if VIRTIO_DEBUG_ENABLED
#define VIRTIO_DEBUG_PRINT(msg...) debug_printf(msg)
#else
#define VIRTIO_DEBUG_PRINT(msg...)
#endif

#ifdef VIRTIO_DEBUG_VQ_ENABLED
#define VIRTIO_DEBUG_VQ(msg...) VIRTIO_DEBUG_PRINT("[virtq] " msg)
#else
#define VIRTIO_DEBUG_VQ(msg...)
#endif

#ifdef VIRTIO_DEBUG_DEV_ENABLED
#define VIRTIO_DEBUG_DEV(msg...) VIRTIO_DEBUG_PRINT("[virtio dev] " msg)
#else
#define VIRTIO_DEBUG_DEV(msg...)
#endif

#ifdef VIRTIO_DEBUG_DT_ENABLED
#define VIRTIO_DEBUG_DT(msg...) VIRTIO_DEBUG_PRINT("[virtio type] " msg)
#else
#define VIRTIO_DEBUG_DT(msg...)
#endif

#ifdef VIRTIO_DEBUG_TL_ENABLED
#define VIRTIO_DEBUG_TL(msg...) VIRTIO_DEBUG_PRINT("[virtio tl] " msg)
#else
#define VIRTIO_DEBUG_TL(msg...)
#endif

#ifdef VIRTIO_DEBUG_CHAN_ENABLED
#define VIRTIO_DEBUG_CHAN(msg...) VIRTIO_DEBUG_PRINT("[virtio chan] " msg)
#else
#define VIRTIO_DEBUG_CHAN(msg...)
#endif

#ifdef VIRTIO_DEBUG_BUF_ENABELD
#define VIRTIO_DEBUG_BUF(msg...) VIRTIO_DEBUG_PRINT("[virtio buf] " msg)
#else
#define VIRTIO_DEBUG_BUF(msg...)
#endif
#endif /* VIRTIO_DEBUG_H_ */
