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

#include <dma_internal.h>
#include <dma_mem_utils.h>
#include <driverkit/iommu.h>

static lvaddr_t addr_start = (256UL << 30);

/**
 * \brief allocates and maps a memory region to be used for DMA purposes
 *
 * \param bytes minimum size of the memory region in bytes
 * \param flags VREGION flags how the region gets mapped
 * \param cl    IOMMU client if IOMMU is present
 * \param mem   returns the mapping information
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_mem_alloc(size_t bytes,
                       vregion_flags_t flags,
                       struct iommu_client *cl,
                       struct dmem *mem)
{
    // TODO might also include allocation with IOMMU present
    errval_t err;

    if (mem == NULL) {
        return DMA_ERR_ARG_INVALID;
    }

    if (cl != NULL) {
        err = driverkit_iommu_mmap_cl(cl, bytes, flags, mem);
        return err;
    }

    uint64_t base, limit;
    ram_get_affinity(&base, &limit);
    ram_set_affinity(4UL << 30, 128UL << 32);
    err = frame_alloc(&mem->mem, bytes, &mem->size);
    if (err_is_fail(err)) {
        return err;
    }
    ram_set_affinity(base, limit);

    struct frame_identity id;
    err = frame_identify(mem->mem, &id);
    if (err_is_fail(err)) {
        dma_mem_free(mem);
        return err;
    }

    mem->devaddr = id.base;


    void *addr;
    err = vspace_map_one_frame_fixed_attr(addr_start, mem->size, mem->mem, flags, NULL,
                                          NULL);
    if (err_is_fail(err)) {
        dma_mem_free(mem);
        return err;
    }

    addr = (void *)addr_start;
    addr_start += mem->size;

    mem->vbase = (lvaddr_t)addr;

    return SYS_ERR_OK;
}

/**
 * \brief tries to free the allocated memory region
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_mem_free(struct dmem *mem)
{
    errval_t err;

    if (mem->vbase) {
        err = vspace_unmap((void*)mem->vbase);
        if (err_is_fail(err)) {
            /* todo: error handling ignoring for now */
        }
    }

    if (!capref_is_null(mem->mem)) {
        err = cap_destroy(mem->mem);
        if (err_is_fail(err)) {
            /* todo: error handling ignoring for now */

        }
    }

    memset(mem, 0, sizeof(*mem));

    return SYS_ERR_OK;
}

