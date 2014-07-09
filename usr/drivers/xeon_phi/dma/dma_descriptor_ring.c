/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>

#include "xeon_phi_internal.h"
#include "dma.h"
#include "dma_descriptor_ring.h"
#include "debug.h"

#define IS_POW2(num) (((num) != 0) && (((num) & (~(num) + 1)) == (num)))

/**
 * \brief initializes a dma descriptor ring and allocates memory for it
 *
 * \param ring  the ring structure to initialize
 * \param size  number of elements in the ring
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t xeon_phi_dma_desc_ring_alloc(struct xdma_ring *ring,
                                      uint16_t size)
{
    errval_t err;

    memset(ring, 0, sizeof(*ring));

    assert(size < (XEON_PHI_DMA_DESC_RING_MAX));
    assert(IS_POW2(size));

#ifndef __k1om__
    /*
     * we set the ram affinity to the maximum range mapped by the system memory
     * page tables when being on the host. Otherwise the card cannot access it.
     */
    uint64_t minbase, maxlimit;
    ram_get_affinity(&minbase, &maxlimit);
    ram_set_affinity(0, XEON_PHI_SYSMEM_SIZE-8*XEON_PHI_SYSMEM_PAGE_SIZE);
#endif

    size_t frame_size = ((size_t) size) * XEON_PHI_DMA_DESC_SIZE;
    err = frame_alloc(&ring->cap, frame_size, NULL);

#ifndef __k1om__
    ram_set_affinity(minbase, maxlimit);
#endif

    if (err_is_fail(err)) {
        return err;
    }

    err = vspace_map_one_frame_attr(&ring->vbase,
                                    frame_size,
                                    ring->cap,
                                    VREGION_FLAGS_READ_WRITE,
                                    NULL,
                                    NULL);
    if (err_is_fail(err)) {
        cap_destroy(ring->cap);
        return err;
    }

    struct frame_identity id;
    err = invoke_frame_identify(ring->cap, &id);
    assert(err_is_ok(err));
    ring->pbase = id.base;
    ring->size = size;

    memset(ring->vbase, 0, frame_size);

    return SYS_ERR_OK;
}

/**
 * \brief frees up the resources used by the ring.
 *
 * \param ring the descriptor ring to be freed
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_dma_desc_ring_free(struct xdma_ring *ring)
{
    errval_t err;

    if (capref_is_null(ring->cap)) {
        return SYS_ERR_OK;
    }

    if (ring->vbase) {
        vspace_unmap(ring->vbase);
    }


    err = cap_revoke(ring->cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "revokation of ring cap failed\n");
    }
    return cap_destroy(ring->cap);
}



