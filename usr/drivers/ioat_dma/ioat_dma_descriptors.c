/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
/*
 *
 *
 */

#include <barrelfish/barrelfish.h>

#include <dev/ioat_dma_dev.h>

#include "ioat_dma.h"
#include "ioat_dma_descriptors.h"

#include "debug.h"

#define ALIGN(val, align) (((val) + (align)-1) & ~((align)-1))


#define IOAT_DMA_DESC_FLAG_VALID     0x01
#define IOAT_DMA_DESC_FLAG_USED      0x02
#define IOAT_DMA_DESC_FLAG_FIRST     0x03
#define IOAT_DMA_DESC_FLAG_LAST      0x08
#define IOAT_DMA_DESC_FLAG_PROGRESS  0x10
#define IOAT_DMA_DESC_FLAG_DONE      0x40
#define IOAT_DMA_DESC_FLAG_ERR       0x80

/**
 * IOAT DMA Descriptor Meta structure. This wrapps around the hardware descriptor
 * and is used to keep track of the descriptors.
 */
struct ioat_dma_descriptor
{
    lpaddr_t paddr;                     ///< physical address of the descriptor
    ioat_dma_desc_t desc;               ///< virtual address of the descriptor
    uint32_t flags;                     ///< descriptor flags
    struct ioat_dma_descriptor *next;   ///< next descriptor in the list
    struct ioat_dma_request *req;       ///< pointer to the DMA request
    struct ioat_dma_desc_alloc *alloc;  ///< allocator this descriptor belongs to
};

/**
 *
 */
struct frame_list
{
    struct capref frame;     ///< the frame backing the descriptors
    void *desc;              ///< pointer to the descriptor data structure
    struct frame_list *next;  ///< next memory in list
};

struct ioat_dma_desc_alloc
{
    uint16_t elem_size;                     ///< the size of the elements
    uint16_t elem_align;                    ///< alignment of the elements
    uint32_t elem_count;                    ///< number of elements when growing
    struct ioat_dma_descriptor *free_list;  ///< free list
    uint32_t free_count;                    ///< the number of elements in free list
    struct frame_list *frame_list;          ///< keep track of allocated frames
};

static inline struct ioat_dma_descriptor *desc_deq(struct ioat_dma_desc_alloc *alloc)
{
    struct ioat_dma_descriptor *ret = alloc->free_list;
    alloc->free_list = ret->next;
    alloc->free_count--;
    return ret;
}

static inline void desc_enq(struct ioat_dma_descriptor *desc)
{
    struct ioat_dma_desc_alloc *alloc = desc->alloc;
    desc->next = alloc->free_list;
    alloc->free_list = desc;
    alloc->free_count++;
}

/**
 *
 */
static errval_t alloc_grow(struct ioat_dma_desc_alloc *alloc)
{
    errval_t err;

    struct frame_list *flist = malloc(sizeof(struct frame_list));
    if (flist == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    size_t frame_size = alloc->elem_count * alloc->elem_size;
    err = frame_alloc(&flist->frame, frame_size, &frame_size);
    if (err_is_fail(err)) {
        free(flist);
        return err;
    }

    struct frame_identity id;
    err = invoke_frame_identify(flist->frame, &id);
    if (err_is_fail(err)) {
        cap_destroy(flist->frame);
        free(flist);
        return err;
    }

    uint8_t *descr_vaddr;
    err = vspace_map_one_frame_attr((void **) &descr_vaddr, frame_size, flist->frame,
    IOAT_DMA_DESC_MAP_FLAGS,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        cap_destroy(flist->frame);
        free(flist);
        return err;
    }

    IODESC_DEBUG("Allocated frame of size %lu bytes @ [%016lx]\n",
                 (uint64_t ) frame_size, id.base);

    uint32_t num_desc = frame_size / alloc->elem_size;

    struct ioat_dma_descriptor *desc = calloc(num_desc, sizeof(*desc));
    if (desc == NULL) {
        vspace_unmap(descr_vaddr);
        free(flist);
        return LIB_ERR_MALLOC_FAIL;
    }

    lpaddr_t descr_paddr = id.base;

    for (uint32_t i = 0; i < num_desc; ++i) {
        /* initialize the fields */
        desc->desc = descr_vaddr;
        desc->paddr = descr_paddr;
        desc->req = NULL;
        desc->alloc = alloc;
        desc->flags = IOAT_DMA_DESC_FLAG_VALID;

        /* put it onto the free list */
        desc_enq(desc);

        descr_vaddr += alloc->elem_size;
        descr_paddr += alloc->elem_size;
    }

    flist->next = alloc->frame_list;
    alloc->frame_list = flist;

    IODESC_DEBUG("Allocated %u desc of size %u\n", num_desc, alloc->elem_size);

    return SYS_ERR_OK;
}

/*
 * ----------------------------------------------------------------------------
 * Public interface
 * ----------------------------------------------------------------------------
 */

/**
 * \brief allocates and initializes a new descriptor allocator for descriptors
 *        of a give size.
 *
 * \param size      size of the descriptors in bytes
 * \param align     alignment constraint for the descriptors
 * \param count     number of initial descriptors to allocate
 * \param ret_alloc returns a pointer to the allocator
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t ioat_dma_desc_alloc_init(uint16_t size,
                                  uint16_t align,
                                  uint32_t count,
                                  struct ioat_dma_desc_alloc **ret_alloc)
{
    errval_t err;

    assert(ret_alloc);

    struct ioat_dma_desc_alloc *alloc = calloc(1, sizeof(*alloc));

    assert((align & (IOAT_DMA_DESC_ALIGN -1)) == 0);

    alloc->elem_size = ALIGN(size, align);
    alloc->elem_align = align;
    alloc->elem_count = count;

    IODESC_DEBUG("Initialized allocator from elements of size %u\n",
                 alloc->elem_size);

    alloc->free_count = 0;
    alloc->free_list = NULL;
    alloc->frame_list = NULL;

    err = alloc_grow(alloc);
    if (err_is_fail(err)) {
        free(alloc);
        *ret_alloc = NULL;
        return err;
    }

    *ret_alloc = alloc;

    return SYS_ERR_OK;
}

/**
 * \brief allocates a new descriptor from the allocator
 *
 * \param alloc the allocator to get the descriptors
 *
 * \returns pointer to the memory region of the descriptor
 *          NULL on failure
 *
 * Note: this function will automatically grow the allocator if there are
 *       no descriptors free
 */
struct ioat_dma_descriptor *ioat_dma_desc_alloc(struct ioat_dma_desc_alloc *alloc)
{
    errval_t err;

    if (alloc->free_count == 0) {
        assert(alloc->free_list == NULL);
        err = alloc_grow(alloc);
        if (err_is_fail(err)) {
            return NULL;
        }
    }

    struct ioat_dma_descriptor *ret = desc_deq(alloc);
    ret->flags |= IOAT_DMA_DESC_FLAG_USED;
    ret->flags |= IOAT_DMA_DESC_FLAG_FIRST;
    ret->flags |= IOAT_DMA_DESC_FLAG_LAST;
    assert(ret->flags & IOAT_DMA_DESC_FLAG_VALID);
    return ret;
}

/**
 * \brief frees a previously allocated DMA descriptor
 *
 * \param desc the descriptor to be returned
 */
void ioat_dma_desc_free(struct ioat_dma_descriptor *desc)
{
    assert(desc->flags & IOAT_DMA_DESC_FLAG_VALID);
    desc->flags = IOAT_DMA_DESC_FLAG_VALID;
    desc->req = NULL;
    desc_enq(desc);
}

/**
 * \brief allocates a a descriptor chain from the allocator
 *
 * \param alloc the allocator to get the descriptors
 * \param count number of descriptors in the chain
 *
 * \returns pointer to the head of the descriptor chain
 *          NULL on failure
 *
 * Note: this function will automatically grow the allocator if there are
 *       no descriptors free
 */
struct ioat_dma_descriptor *ioat_dma_desc_chain_alloc(struct ioat_dma_desc_alloc *alloc,
                                                      uint32_t count)
{
    errval_t err;

    if (alloc->free_count < count) {
        err = alloc_grow(alloc);
        if (err_is_fail(err)) {
            return NULL;
        }
    }
    assert(alloc->free_count > count);

    struct ioat_dma_descriptor *desc = NULL, *next = NULL;
    for (uint32_t i = 0; i < count; ++i) {
        desc = desc_deq(alloc);
        assert(desc);
        assert(desc->flags & IOAT_DMA_DESC_FLAG_VALID);

        if (!next) {
            desc->flags |= IOAT_DMA_DESC_FLAG_LAST;
            ioat_dma_desc_next_insert(desc->desc, 0);
        } else {
            ioat_dma_desc_next_insert(desc->desc, next->paddr);
        }
        desc->flags |= IOAT_DMA_DESC_FLAG_USED;
        desc->next = next;

        next = desc;
    }
    desc->flags |= IOAT_DMA_DESC_FLAG_FIRST;

    return desc;
}

/**
 * \brief frees a previously allocated DMA descriptor chain
 *
 * \param desc head of the descriptor chain to be freed
 */
void ioat_dma_desc_chain_free(struct ioat_dma_descriptor *head)
{
    struct ioat_dma_descriptor *desc;
    while (head) {
        assert(head->flags & IOAT_DMA_DESC_FLAG_VALID);
        head->flags = IOAT_DMA_DESC_FLAG_VALID;
        head->req = NULL;
        desc = head;
        head = head->next;
        desc_enq(desc);
    }
}

/*
 * ----------------------------------------------------------------------------
 * Descriptor getters / setters
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns the physical address of the descriptor
 *
 * \param desc IOAT DMA descriptor
 *
 * \returns physical address of the descriptor
 */
lpaddr_t ioat_dma_desc_get_paddr(struct ioat_dma_descriptor *desc)
{
    return desc->paddr;
}

/**
 * \brief returns a virtual address pointer to the location where the descriptor
 *        is mapped
 *
 * \param desc IOAT DMA descriptor
 */
ioat_dma_desc_t ioat_dma_desc_get_desc(struct ioat_dma_descriptor *desc)
{
    return desc->desc;
}

/**
 * \brief sets the corresponding request
 *
 * \param desc IOAT DMA descriptor
 */
void ioat_dma_desc_set_request(struct ioat_dma_descriptor *desc,
                               struct ioat_dma_request *req)
{
    desc->req = req;
}

/**
 * \brief returns the corresponding IOAT DMA request this descriptor belongs
 *
 * \param desc IOAT DMA descriptor
 *
 * \brief pointer to the request
 *        NULL if there is none
 */
struct ioat_dma_request *ioat_dma_desc_get_request(struct ioat_dma_descriptor *desc)
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
 * \brief sets the next descriptor pointer in the descriptor
 *
 * \param desc IOAT DMA descriptor
 * \param next IOAT DMA descriptor
 *
 * XXX: This does not check for cycles
 */
void ioat_dma_desc_set_next(struct ioat_dma_descriptor *desc,
                            struct ioat_dma_descriptor *next)
{
    if (desc->flags & IOAT_DMA_DESC_FLAG_LAST) {
        desc->flags &= ~IOAT_DMA_DESC_FLAG_LAST;
        next->flags |= IOAT_DMA_DESC_FLAG_LAST;
    }
    ioat_dma_desc_next_insert(desc->desc, next->paddr);
    desc->next = next;
}

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
void ioat_dma_desc_init(struct ioat_dma_descriptor *desc,
                        lpaddr_t src,
                        lpaddr_t dst,
                        uint32_t size,
                        ioat_dma_desc_ctrl_t ctrl)
{
    ioat_dma_desc_size_insert(desc->desc, size);
    ioat_dma_desc_ctrl_insert(desc->desc, *((uint32_t *)ctrl));
    ioat_dma_desc_src_insert(desc->desc, src);
    ioat_dma_desc_dst_insert(desc->desc, dst);
}

