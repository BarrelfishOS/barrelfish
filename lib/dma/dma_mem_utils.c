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

#if 0
static lvaddr_t addr_start = (256UL << 30);

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
errval_t dma_mem_alloc(size_t bytes,
                       vregion_flags_t flags,
                       bool iommu_present,
                       struct dma_mem *mem)
{
    errval_t err;

    if (mem == NULL) {
        return DMA_ERR_ARG_INVALID;
    }

    uint64_t base, limit;
    ram_get_affinity(&base, &limit);
    ram_set_affinity(4UL << 30, 128UL << 32);
    err = frame_alloc(&mem->frame, bytes, &mem->bytes);
    if (err_is_fail(err)) {
        return err;
    }
    ram_set_affinity(base, limit);



    struct frame_identity id;
    err = frame_identify(mem->frame, &id);
    if (err_is_fail(err)) {
        dma_mem_free(mem);
        return err;
    }

    mem->paddr = id.base;


    void *addr;
    err = vspace_map_one_frame_fixed_attr(addr_start, mem->bytes, mem->frame, flags, NULL,
                                          NULL);
    //err = vspace_map_one_frame_attr(&addr, mem->bytes, mem->frame, flags, NULL,
    //                                NULL);
    if (err_is_fail(err)) {
        dma_mem_free(mem);
        return err;
    }

    addr = (void *)addr_start;
    addr_start += mem->bytes;

    mem->vaddr = (lvaddr_t)addr;

    if (iommu_present) {
        debug_printf("Overwriting the address with virtual mapped one\n");

        debug_printf("Allocated DMA memory: %lx..%lx @ %lx..%lx\n",
                     mem->paddr, mem->paddr + mem->bytes - 1,
                     (lpaddr_t)addr,(lpaddr_t)addr + mem->bytes - 1);

        mem->paddr = (lpaddr_t)addr;
    }

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
            /* todo: error handling ignoring for now */
        }
    }

    if (!capref_is_null(mem->frame)) {
        err = cap_destroy(mem->frame);
        if (err_is_fail(err)) {
            /* todo: error handling ignoring for now */

        }
    }

    memset(mem, 0, sizeof(*mem));

    return SYS_ERR_OK;
}

#endif