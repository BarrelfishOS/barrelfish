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

#include <ioat/ioat_dma_internal.h>
#include <ioat/ioat_dma_descriptors_internal.h>

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
inline void ioat_dma_desc_fill_memcpy(struct dma_descriptor *desc,
                                      lpaddr_t src,
                                      lpaddr_t dst,
                                      uint32_t size,
                                      ioat_dma_desc_ctrl_t ctrl)
{
    uint8_t *vbase = dma_desc_get_desc_handle(desc);
    ioat_dma_desc_size_insert(vbase, size);
    ioat_dma_desc_ctrl_insert(vbase, *((uint32_t *) ctrl));
    ioat_dma_desc_ctrl_op_insert(ctrl, ioat_dma_desc_op_copy);
    ioat_dma_desc_src_insert(vbase, src);
    ioat_dma_desc_dst_insert(vbase, dst);
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
inline void ioat_dma_desc_fill_memset(struct dma_descriptor *desc,
                                      uint64_t data,
                                      lpaddr_t dst,
                                      uint32_t size,
                                      ioat_dma_desc_ctrl_t ctrl)
{
    uint8_t *vbase = dma_desc_get_desc_handle(desc);
    ioat_dma_desc_size_insert(vbase, size);
    ioat_dma_desc_ctrl_op_insert(ctrl, ioat_dma_desc_op_memset);
    ioat_dma_desc_ctrl_insert(vbase, *((uint32_t *) ctrl));
    ioat_dma_desc_src_insert(vbase, data);
    ioat_dma_desc_dst_insert(vbase, dst);
}

/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for nop descriptors (null descriptors)
 *
 * \param desc  IOAT DMA descriptor
 */
inline void ioat_dma_desc_fill_nop(struct dma_descriptor *desc)
{
    uint8_t *vbase = dma_desc_get_desc_handle(desc);
    uint32_t ctrl = 0;
    ioat_dma_desc_ctrl_t dma_ctrl = (ioat_dma_desc_ctrl_t) (&ctrl);
    ioat_dma_desc_ctrl_int_en_insert(dma_ctrl, 0x1);
    ioat_dma_desc_ctrl_null_insert(dma_ctrl, 0x1);
    ioat_dma_desc_ctrl_compl_write_insert(dma_ctrl, 0x1);

    ioat_dma_desc_size_insert(vbase, 1);   // size must be non zero
    ioat_dma_desc_ctrl_insert(vbase, ctrl);
    ioat_dma_desc_src_insert(vbase, 0);
    ioat_dma_desc_dst_insert(vbase, 0);
}
