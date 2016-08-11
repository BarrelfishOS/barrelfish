/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <barrelfish/barrelfish.h>
#include "dma_mem.h"

errval_t dma_mem_from_capref(struct capref frame, struct dma_mem *mem)
{
    errval_t err = SYS_ERR_OK;
    if (mem == NULL) {
        return DMA_ERR_ARG_INVALID;
    }

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "invoke frame id");
        return err;
    }

    mem->paddr = id.base;
    mem->bytes = id.bytes;
    mem->requested = id.bytes;
    mem->frame = frame;

    void *addr;
    err = vspace_map_one_frame_attr(&addr, mem->bytes, mem->frame, VREGION_FLAGS_READ_WRITE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        dma_mem_free(mem);
        return err;
    }

    mem->vaddr = (lvaddr_t)addr;

    return SYS_ERR_OK;
}

/**
 * \brief allocates and maps a memory region to be used for DMA purposes
 *
 * \param bytes minimum size of the memory region in bytes
 * \param flags VREGION flags how the region gets mapped
 * \param mem   returns the mapping information
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_mem_alloc(size_t bytes, struct dma_mem *mem)
{
    errval_t err = SYS_ERR_OK;
    mem->requested = bytes;
    bytes = ROUND_UP(BASE_PAGE_SIZE, bytes);

    if (mem == NULL) {
        return DMA_ERR_ARG_INVALID;
    }

    err = frame_alloc(&mem->frame, bytes, &mem->bytes);
    if (err_is_fail(err)) {
        return err;
    }

    struct frame_identity id;
    err = invoke_frame_identify(mem->frame, &id);
    if (err_is_fail(err)) {
        dma_mem_free(mem);
        return err;
    }

    mem->paddr = id.base;

    void *addr;
    err = vspace_map_one_frame_attr(&addr, mem->bytes, mem->frame, VREGION_FLAGS_READ_WRITE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        dma_mem_free(mem);
        return err;
    }

    mem->vaddr = (lvaddr_t)addr;

    return SYS_ERR_OK;
}

/**
 * \brief tries to free the allocated memory region
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_mem_free(struct dma_mem *mem)
{
    errval_t err;

    if (mem->vaddr) {
        err = vspace_unmap((void*)mem->vaddr);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "call failed.");
        }
    }

    if (!capref_is_null(mem->frame)) {
        err = cap_destroy(mem->frame);
        if (err_is_fail(err)) {
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "call failed.");
            }
        }
    }

    memset(mem, 0, sizeof(*mem));

    return SYS_ERR_OK;
}
