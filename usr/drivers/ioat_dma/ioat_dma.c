/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <pci/pci.h>
#include <pci/devids.h>

#include "ioat_dma.h"
#include "ioat_dma_device.h"
#include "ioat_dma_channel.h"

#include "debug.h"

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
errval_t ioat_dma_mem_alloc(size_t bytes,
                            vregion_flags_t flags,
                            struct ioat_dma_mem *mem)
{
    errval_t err;

    if (mem == NULL) {
        return IOAT_ERR_ARG_INVALID;
    }

    err = frame_alloc(&mem->frame, bytes, &mem->bytes);
    if (err_is_fail(err)) {
        return err;
    }

    struct frame_identity id;
    err = invoke_frame_identify(mem->frame, &id);
    if (err_is_fail(err)) {
        ioat_dma_mem_free(mem);
        return err;
    }

    mem->paddr = id.base;

    err = vspace_map_one_frame_attr(&mem->addr, mem->bytes, mem->frame, flags, NULL, NULL);
    if (err_is_fail(err)) {
        ioat_dma_mem_free(mem);
        return err;
    }

    return SYS_ERR_OK;
}

/**
 * \brief tries to free the allocated memory region
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t ioat_dma_mem_free(struct ioat_dma_mem *mem)
{
    errval_t err;

    if (mem->addr) {
        err = vspace_unmap(mem->addr);
        if (err_is_fail(err)) {
            /* todo: error handling */
        }
    }

    if (!capref_is_null(mem->frame)) {
        err = cap_destroy(mem->frame);
        if (err_is_fail(err)) {
            /* todo: error handling */
        }
    }

    memset(mem, 0, sizeof(*mem));

    return SYS_ERR_OK;
}

