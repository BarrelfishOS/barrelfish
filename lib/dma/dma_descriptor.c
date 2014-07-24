/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <barrelfish/barrelfish.h>
#include <xeon_phi/xeon_phi.h>

#include <dma_mem_utils.h>

#include <dma_internal.h>
#include <dma_descriptor_internal.h>

#include <debug.h>

/* helper macros */
#define ALIGN(val, align) (((val) + (align)-1) & ~((align)-1))

/* DMA Descriptor flags */

/// this descriptor is valid
#define DMA_DESC_FLAG_VALID     0x01

/// this descriptor is currently used in a request
#define DMA_DESC_FLAG_USED      0x02

/// this is the first descriptor of a request
#define DMA_DESC_FLAG_FIRST     0x04

/// this is the last descriptor in a request
#define DMA_DESC_FLAG_LAST      0x08

/// the descriptor has not been processed yet
#define DMA_DESC_FLAG_PROGRESS  0x10

/// this descriptor is the head of the allocation unit (only this can be freed)
#define DMA_DESC_FLAG_HEAD      0x20

/// the descriptor has been executed
#define DMA_DESC_FLAG_DONE      0x40

/// there was an error during the execution of this descriptor
#define DMA_DESC_FLAG_ERR       0x80

/**
 * DMA Descriptor Meta structure. This wraps around the hardware descriptor
 * and is used to keep track of the descriptors.
 */
struct dma_descriptor
{
    lpaddr_t paddr;               ///< physical address of the descriptor
    uint8_t *desc;                ///< virtual address of the descriptor
    uint32_t flags;               ///< descriptor flags
    struct dma_descriptor *next;  ///< next descriptor in the list
    struct dma_request *req;      ///< pointer to the DMA request
    struct dma_mem *mem;          ///< the dma memory information
};

/*
 * ============================================================================
 * Library Internal Interface
 * ============================================================================
 */

/**
 * \brief allocates a number of hardware DMA descriptors and fills them into the
 *        array of descriptor pointers
 *
 * \param size  size of a signle descriptor in bytes
 * \param align alignment constraints of the descriptors
 * \param count number of descriptors to allocate in bits
 * \param desc  pointer to the array of descriptor pointers
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 *
 * NOTE: The descriptors are linked in a ring on a software level,
 *       the corresonding hardware descriptors are not linked.
 */
errval_t dma_desc_alloc(uint32_t size,
                        uint16_t align,
                        uint8_t count,
                        struct dma_descriptor **desc)
{
    errval_t err;

    assert(desc);

    uint32_t ndesc = (1 << count);

    size = ALIGN(size, align);

    struct dma_descriptor *dma_desc = calloc(ndesc, sizeof(*dma_desc));
    if (dma_desc == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    struct dma_mem *mem = malloc(sizeof(*mem));
    if (mem == NULL) {
        free(dma_desc);
        return LIB_ERR_MALLOC_FAIL;
    }

#ifndef __k1om__
    /*
     * we set the ram affinity to the maximum range mapped by the system memory
     * page tables when being on the host. Otherwise the card cannot access it.
     */
    uint64_t minbase, maxlimit;
    ram_get_affinity(&minbase, &maxlimit);
    ram_set_affinity(0, XEON_PHI_SYSMEM_SIZE-8*XEON_PHI_SYSMEM_PAGE_SIZE);
#endif

    err = dma_mem_alloc(ndesc * size, DMA_DESC_MAP_FLAGS, mem);
    if (err_is_fail(err)) {
        free(dma_desc);
        return err;
    }

#ifndef __k1om__
    ram_set_affinity(minbase, maxlimit);
#endif

    memset((void*) mem->vaddr, 0, ndesc * size);

    XPHIDESC_DEBUG("Allocated frame of size %lu bytes @ [%016lx]\n",
                   (uint64_t ) mem->bytes, mem->paddr);

    lpaddr_t desc_paddr = mem->paddr;
    uint8_t *desc_vaddr = (uint8_t*) mem->vaddr;

    /* set the last virtual address pointer */
    dma_desc[ndesc - 1].desc = desc_vaddr + ((ndesc - 1) * size);

    for (uint32_t i = 0; i < ndesc; ++i) {
        /* initialize the fields */
        dma_desc[i].desc = desc_vaddr;
        dma_desc[i].paddr = desc_paddr;
        dma_desc[i].req = NULL;
        dma_desc[i].flags = DMA_DESC_FLAG_VALID;
        dma_desc[i].mem = mem;

        /* mark the first one */
        if (i == 0) {
            dma_desc[i].flags |= DMA_DESC_FLAG_HEAD;
        }

        /* do the linkage */
        dma_desc[(i - 1) & (ndesc - 1)].next = &dma_desc[i];

        /* set the entry in the array */
        desc[i] = &dma_desc[i];

        desc_vaddr += size;
        desc_paddr += size;
    }

    DMADESC_DEBUG("Allocated %u desc of size %u\n", ndesc, size);

    return SYS_ERR_OK;
}

/**
 * \brief brief frees up the array of previously allocated descriptors
 *        and releases the resources
 *
 * \param desc  the descriptors to be freed
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t dma_desc_free(struct dma_descriptor *desc)
{
    errval_t err;

    if (desc->flags & DMA_DESC_FLAG_HEAD) {
        return DMA_ERR_ARG_INVALID;
    }

    struct dma_mem *mem = desc->mem;

    err = dma_mem_free(mem);
    if (err_is_fail(err)) {
        return err;
    }

    free(desc);
    free(mem);

    return SYS_ERR_OK;
}

/*
 * ----------------------------------------------------------------------------
 * Descriptor getters / setters
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns a virtual address pointer to the location where the descriptor
 *        is mapped
 *
 * \param desc DMA descriptor
 */
inline uint8_t *dma_desc_get_desc_handle(struct dma_descriptor *desc)
{
    return desc->desc;
}

/**
 * \brief returns the physical address of the descriptor
 *
 * \param desc DMA descriptor
 *
 * \returns physical address of the descriptor
 */
inline lpaddr_t dma_desc_get_paddr(struct dma_descriptor *desc)
{
    return desc->paddr;
}

/**
 * \brief sets the corresponding request
 *
 * \param desc  DMA descriptor
 */
inline void dma_desc_set_request(struct dma_descriptor *desc,
                                 struct dma_request *req)
{
    desc->req = req;
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/*
 * ----------------------------------------------------------------------------
 * Descriptor getters / setters
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns the corresponding DMA request this descriptor belongs
 *
 * \param desc DMA descriptor
 *
 * \brief pointer to the request
 *        NULL if there is none
 */
inline struct dma_request *dma_desc_get_request(struct dma_descriptor *desc)
{
    return desc->req;
}

/**
 * \brief returns a pointer to the next descriptor in a chain
 *
 * \param desc  DMA descriptor
 *
 * \returns next descriptor
 *          NULL if the end of chain
 */
struct dma_descriptor *dma_desc_get_next(struct dma_descriptor *desc)
{
    return desc->next;
}

