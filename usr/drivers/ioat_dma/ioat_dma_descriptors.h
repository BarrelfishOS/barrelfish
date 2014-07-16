/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOAT_DMA_DESCRIPTORS_H
#define IOAT_DMA_DESCRIPTORS_H

#include <dev/ioat_dma_dev.h>

struct ioat_dma_descriptor;
struct ioat_dma_request;

#define IOAT_DMA_DESC_MAP_FLAGS VREGION_FLAGS_READ_WRITE

/// the minimum amount of DMA descriptors to allocate
#define IOAT_DMA_DESC_COUNT 256

/// the size of the basic descriptor
#define IOAT_DMA_DESC_SIZE 64

/// minimum alignment constraint for the descriptors
#define IOAT_DMA_DESC_ALIGN 64


/*
 * ----------------------------------------------------------------------------
 * DMA Descriptor Allocation / Free
 * ----------------------------------------------------------------------------
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
                             struct ioat_dma_descriptor **desc);


/*
 * ----------------------------------------------------------------------------
 * DMA Descriptor Setup Functions
 * ----------------------------------------------------------------------------
 */

/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for memcpy descriptors
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
void ioat_dma_desc_fill_memcpy(struct ioat_dma_descriptor *desc,
                               lpaddr_t src,
                               lpaddr_t dst,
                               uint32_t size,
                               ioat_dma_desc_ctrl_t ctrl);

/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for nop descriptors (null descriptors)
 *
 * \param desc  IOAT DMA descriptor
 */
void ioat_dma_desc_fill_nop(struct ioat_dma_descriptor *desc);


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
lpaddr_t ioat_dma_desc_get_paddr(struct ioat_dma_descriptor *desc);

/**
 * \brief returns a virtual address pointer to the location where the descriptor
 *        is mapped
 *
 * \param desc IOAT DMA descriptor
 */
ioat_dma_desc_t ioat_dma_desc_get_desc(struct ioat_dma_descriptor *desc);

/**
 * \brief sets the corresponding request
 *
 * \param desc IOAT DMA descriptor
 */
void ioat_dma_desc_set_request(struct ioat_dma_descriptor *desc,
                               struct ioat_dma_request *req);

/**
 * \brief returns the corresponding IOAT DMA request this descriptor belongs
 *
 * \param desc IOAT DMA descriptor
 *
 * \brief pointer to the request
 *        NULL if there is none
 */
struct ioat_dma_request *ioat_dma_desc_get_request(struct ioat_dma_descriptor *desc);

/**
 * \brief returns a pointer to the next descriptor in a chain
 *
 * \param desc IOAT DMA descriptor
 *
 * \returns next descriptor
 *          NULL if the end of chain
 */
struct ioat_dma_descriptor *ioat_dma_desc_get_next(struct ioat_dma_descriptor *desc);

/**
 * \brief sets the next descriptor pointer in the descriptor
 *
 * \param desc IOAT DMA descriptor
 * \param next IOAT DMA descriptor
 *
 * XXX: This does not check for cycles
 */
void ioat_dma_desc_set_next(struct ioat_dma_descriptor *desc,
                            struct ioat_dma_descriptor *next);


#if 0






/*
 * ----------------------------------------------------------------------------
 * Descriptor allocation / free
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
                                  struct ioat_dma_desc_alloc **ret_alloc);



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
struct ioat_dma_descriptor *ioat_dma_desc_alloc(struct ioat_dma_desc_alloc *alloc);

/**
 * \brief frees a previously allocated DMA descriptor
 *
 * \param desc the descriptor to be returned
 */
void ioat_dma_desc_free(struct ioat_dma_descriptor *desc);

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
                                                      uint32_t count);

/**
 * \brief frees a previously allocated DMA descriptor chain
 *
 * \param desc head of the descriptor chain to be freed
 */
void ioat_dma_desc_chain_free(struct ioat_dma_descriptor *head);







#endif
#endif /* IOAT_DMA_CHANNEL_H */
