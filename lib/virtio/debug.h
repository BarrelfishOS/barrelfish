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

#define VIRTIO_DEBUG_ENABLED 1
#define VIRTIO_DEBUG_VQ_ENABLED 1

#if VIRTIO_DEBUG_ENABLED
#define VIRTIO_DEBUG_PRINT(msg...) debug_printf(msg)
#else
#define VIRTIO_DEBUG_PRINT(msg...)
#endif

#ifdef VIRTIO_DEBUG_VQ_ENABLED
#define VIRTIO_DEBUG_VQ(msg...) VIRTIO_DEBUG_PRINT("[VIRTQUEUE] " msg)
#else
#define VIRTIO_DEBUG_VQ(msg...)
#endif


#endif /* VIRTIO_DEBUG_H_ */
