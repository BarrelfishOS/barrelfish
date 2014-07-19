/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <barrelfish/barrelfish.h>

#include <dev/ioat_dma_dev.h>

#include <dma_mem_utils.h>

#include <ioat/ioat_dma_internal.h>
#include <ioat/ioat_dma_descriptors_internal.h>

#include <debug.h>

/* helper makros */
#define ALIGN(val, align) (((val) + (align)-1) & ~((align)-1))
#define IS_POW2(num) (((num) != 0) && (((num) & (~(num) + 1)) == (num)))

/* IOAT DMA Descriptor flags */

/// this descriptor is valid
#define IOAT_DMA_DESC_FLAG_VALID     0x01

/// this descriptor is currently used in a request
#define IOAT_DMA_DESC_FLAG_USED      0x02

/// this is the first descriptor of a request
#define IOAT_DMA_DESC_FLAG_FIRST     0x04

/// this is the last descriptor in a request
#define IOAT_DMA_DESC_FLAG_LAST      0x08

/// the descriptor has not been processed yet
#define IOAT_DMA_DESC_FLAG_PROGRESS  0x10

/// this descriptor is the head of the allocation unit (only this can be freed)
#define IOAT_DMA_DESC_FLAG_HEAD      0x20

/// the descriptor has been executed
#define IOAT_DMA_DESC_FLAG_DONE      0x40

/// there was an error during the execution of this descriptor
#define IOAT_DMA_DESC_FLAG_ERR       0x80

/**
 * IOAT DMA Descriptor Meta structure. This wraps around the hardware descriptor
 * and is used to keep track of the descriptors.
 */
struct ioat_dma_descriptor
{
    lpaddr_t paddr;                     ///< physical address of the descriptor
    ioat_dma_desc_t desc;               ///< virtual address of the descriptor
    uint32_t flags;                     ///< descriptor flags
    struct ioat_dma_descriptor *next;   ///< next descriptor in the list
    struct ioat_dma_request *req;       ///< pointer to the DMA request
    struct dma_mem *mem;                ///< the dma memory information
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
 * \param size  descriptor size
 * \param align alignment constraints of the descriptors
 * \param count number of descriptors to allocate
 * \param desc  pointer to the array of descriptor pointers
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t ioat_dma_desc_alloc(uint16_t size,
                             uint16_t align,
                             uint16_t count,
                             struct ioat_dma_descriptor **desc)
{
    errval_t err;

    assert(desc);
    assert(IS_POW2(count));
    assert((align & (IOAT_DMA_DESC_ALIGN -1)) == 0);

    struct ioat_dma_descriptor *dma_desc = calloc(count, sizeof(*dma_desc));
    if (dma_desc == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    size = ALIGN(size, align);

    struct dma_mem *mem = malloc(sizeof(*mem));
    err = dma_mem_alloc(count * size, IOAT_DMA_DESC_MAP_FLAGS, mem);
    if (err_is_fail(err)) {
        free(dma_desc);
        return err;
    }

    memset((void*)mem->vaddr, 0, count * size);

    IOATDESC_DEBUG("Allocated frame of size %lu bytes @ [%016lx]\n",
                 (uint64_t ) mem->bytes, mem->paddr);

    lpaddr_t desc_paddr = mem->paddr;
    uint8_t *desc_vaddr = (uint8_t*)mem->vaddr;

    /* set the last virtual address pointer */
    dma_desc[count-1].desc = desc_vaddr + ((count-1)*size);

    for (uint32_t i = 0; i < count; ++i) {
        /* initialize the fields */
        dma_desc[i].desc = desc_vaddr;
        dma_desc[i].paddr = desc_paddr;
        dma_desc[i].req = NULL;
        dma_desc[i].flags = IOAT_DMA_DESC_FLAG_VALID;
        dma_desc[i].mem = mem;

        /* mark the first one */
        if (i == 0) {
            dma_desc[i].flags |= IOAT_DMA_DESC_FLAG_HEAD;
        }

        /* do the linkage */
        ioat_dma_desc_set_next(&dma_desc[(i-1) & (count - 1)], &dma_desc[i]);

        /* set the entry in the array */
        desc[i] = &dma_desc[i];

        desc_vaddr += size;
        desc_paddr += size;
    }

    IOATDESC_DEBUG("Allocated %u desc of size %u\n", count, size);

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
errval_t ioat_dma_desc_free(struct ioat_dma_descriptor *desc)
{
    errval_t err;

    if (desc->flags & IOAT_DMA_DESC_FLAG_HEAD) {
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
 * \brief sets the next pointer of the descriptor and does the corresponding
 *        hardware linkage
 *
 * \param desc descriptor to set the next field
 * \param next following descriptor
 */
inline void ioat_dma_desc_set_next(struct ioat_dma_descriptor *desc,
                                   struct ioat_dma_descriptor *next)
{
    ioat_dma_desc_next_insert(desc->desc, next->paddr);
    desc->next = next;
}

/**
 * \brief returns a virtual address pointer to the location where the descriptor
 *        is mapped
 *
 * \param desc IOAT DMA descriptor
 */
inline ioat_dma_desc_t ioat_dma_desc_get_desc_handle(struct ioat_dma_descriptor *desc)
{
    return desc->desc;
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/**
 * \brief initializes the hardware specific part of the descriptor
 *
 * \param desc  IOAT DMA descriptor
 * \param src   Source address of the transfer
 * \param dst   destination address of the transfer
 * \param size  number of bytes to copy
 * \param ctrl  control flags
 *
 * XXX: this function assumes that the size of the descriptor has already been
 *      checked and must match the maximum transfer size of the channel
 */
inline void ioat_dma_desc_fill_memcpy(struct ioat_dma_descriptor *desc,
                                      lpaddr_t src,
                                      lpaddr_t dst,
                                      uint32_t size,
                                      ioat_dma_desc_ctrl_t ctrl)
{
    ioat_dma_desc_size_insert(desc->desc, size);
    ioat_dma_desc_ctrl_insert(desc->desc, *((uint32_t *) ctrl));
    ioat_dma_desc_src_insert(desc->desc, src);
    ioat_dma_desc_dst_insert(desc->desc, dst);
}

/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for nop descriptors (null descriptors)
 *
 * \param desc  IOAT DMA descriptor
 */
inline void ioat_dma_desc_fill_nop(struct ioat_dma_descriptor *desc)
{
    uint32_t ctrl = 0;
    ioat_dma_desc_ctrl_t dma_ctrl = (ioat_dma_desc_ctrl_t)(&ctrl);
    ioat_dma_desc_ctrl_int_en_insert(dma_ctrl, 0x1);
    ioat_dma_desc_ctrl_null_insert(dma_ctrl, 0x1);
    ioat_dma_desc_ctrl_compl_write_insert(dma_ctrl, 0x1);

    ioat_dma_desc_size_insert(desc->desc, 1);   // size must be non zero
    ioat_dma_desc_ctrl_insert(desc->desc, ctrl);
    ioat_dma_desc_src_insert(desc->desc, 0);
    ioat_dma_desc_dst_insert(desc->desc, 0);
}

/*
 * ----------------------------------------------------------------------------
 * Descriptor getters / setters
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns the corresponding IOAT DMA request this descriptor belongs
 *
 * \param desc IOAT DMA descriptor
 *
 * \brief pointer to the request
 *        NULL if there is none
 */
inline struct ioat_dma_request *ioat_dma_desc_get_request(struct ioat_dma_descriptor *desc)
{
    return desc->req;
}

/**
 * \brief returns a pointer to the next descriptor in a chain
 *
 * \param desc IOAT DMA descriptor
 *
 * \returns next descriptor
 *          NULL if the end of chain
 */
struct ioat_dma_descriptor *ioat_dma_desc_get_next(struct ioat_dma_descriptor *desc)
{
    return desc->next;
}

/**
 * \brief returns the physical address of the descriptor
 *
 * \param desc IOAT DMA descriptor
 *
 * \returns physical address of the descriptor
 */
inline lpaddr_t ioat_dma_desc_get_paddr(struct ioat_dma_descriptor *desc)
{
    return desc->paddr;
}

/**
 * \brief returns a virtual address pointer to the location where the descriptor
 *        is mapped
 *
 * \param desc IOAT DMA descriptor
 */
inline ioat_dma_desc_t ioat_dma_desc_get_desc(struct ioat_dma_descriptor *desc)
{
    return desc->desc;
}

/**
 * \brief sets the corresponding request
 *
 * \param desc IOAT DMA descriptor
 */
inline void ioat_dma_desc_set_request(struct ioat_dma_descriptor *desc,
                                      struct ioat_dma_request *req)
{
    desc->req = req;
}
