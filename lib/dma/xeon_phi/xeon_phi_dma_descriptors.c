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
#include <dev/xeon_phi/xeon_phi_dma_dev.h>

#include <dma_mem_utils.h>

#include <xeon_phi/xeon_phi_dma_internal.h>
#include <xeon_phi/xeon_phi_dma_descriptors_internal.h>

#include <debug.h>


/*
 * ============================================================================
 * Library Internal Interface
 * ============================================================================
 */

#if 0
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
inline void xeon_phi_dma_desc_set_next(struct xeon_phi_dma_descriptor *desc,
                struct xeon_phi_dma_descriptor *next)
{
    xeon_phi_dma_desc_next_insert(desc->desc, next->paddr);
    desc->next = next;
}

/**
 * \brief returns a virtual address pointer to the location where the descriptor
 *        is mapped
 *
 * \param desc XEON_PHI DMA descriptor
 */
inline xeon_phi_dma_desc_t xeon_phi_dma_desc_get_desc_handle(struct xeon_phi_dma_descriptor *desc)
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
 * \param desc  XEON_PHI DMA descriptor
 * \param src   Source address of the transfer
 * \param dst   destination address of the transfer
 * \param size  number of bytes to copy
 * \param ctrl  control flags
 *
 * XXX: this function assumes that the size of the descriptor has already been
 *      checked and must match the maximum transfer size of the channel
 */
inline void xeon_phi_dma_desc_fill_memcpy(struct xeon_phi_dma_descriptor *desc,
                lpaddr_t src,
                lpaddr_t dst,
                uint32_t size,
                xeon_phi_dma_desc_ctrl_t ctrl)
{
    xeon_phi_dma_desc_size_insert(desc->desc, size);
    xeon_phi_dma_desc_ctrl_insert(desc->desc, *((uint32_t *) ctrl));
    xeon_phi_dma_desc_src_insert(desc->desc, src);
    xeon_phi_dma_desc_dst_insert(desc->desc, dst);
}

/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for nop descriptors (null descriptors)
 *
 * \param desc  XEON_PHI DMA descriptor
 */
inline void xeon_phi_dma_desc_fill_nop(struct xeon_phi_dma_descriptor *desc)
{
    uint32_t ctrl = 0;
    xeon_phi_dma_desc_ctrl_t dma_ctrl = (xeon_phi_dma_desc_ctrl_t)(&ctrl);
    xeon_phi_dma_desc_ctrl_int_en_insert(dma_ctrl, 0x1);
    xeon_phi_dma_desc_ctrl_null_insert(dma_ctrl, 0x1);
    xeon_phi_dma_desc_ctrl_compl_write_insert(dma_ctrl, 0x1);

    xeon_phi_dma_desc_size_insert(desc->desc, 1);   // size must be non zero
    xeon_phi_dma_desc_ctrl_insert(desc->desc, ctrl);
    xeon_phi_dma_desc_src_insert(desc->desc, 0);
    xeon_phi_dma_desc_dst_insert(desc->desc, 0);
}

/*
 * ----------------------------------------------------------------------------
 * Descriptor getters / setters
 * ----------------------------------------------------------------------------
 */

/**
 * \brief returns the corresponding XEON_PHI DMA request this descriptor belongs
 *
 * \param desc XEON_PHI DMA descriptor
 *
 * \brief pointer to the request
 *        NULL if there is none
 */
inline struct xeon_phi_dma_request *xeon_phi_dma_desc_get_request(struct xeon_phi_dma_descriptor *desc)
{
    return desc->req;
}

/**
 * \brief returns a pointer to the next descriptor in a chain
 *
 * \param desc XEON_PHI DMA descriptor
 *
 * \returns next descriptor
 *          NULL if the end of chain
 */
struct xeon_phi_dma_descriptor *xeon_phi_dma_desc_get_next(struct xeon_phi_dma_descriptor *desc)
{
    return desc->next;
}

/**
 * \brief returns the physical address of the descriptor
 *
 * \param desc XEON_PHI DMA descriptor
 *
 * \returns physical address of the descriptor
 */
inline lpaddr_t xeon_phi_dma_desc_get_paddr(struct xeon_phi_dma_descriptor *desc)
{
    return desc->paddr;
}

/**
 * \brief returns a virtual address pointer to the location where the descriptor
 *        is mapped
 *
 * \param desc XEON_PHI DMA descriptor
 */
inline xeon_phi_dma_desc_t xeon_phi_dma_desc_get_desc(struct xeon_phi_dma_descriptor *desc)
{
    return desc->desc;
}

/**
 * \brief sets the corresponding request
 *
 * \param desc XEON_PHI DMA descriptor
 */
inline void xeon_phi_dma_desc_set_request(struct xeon_phi_dma_descriptor *desc,
                struct xeon_phi_dma_request *req)
{
    desc->req = req;
}
#endif
