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

#define ASSERT_ALIGNED(x) \
    assert((x) && ((((uintptr_t)x) & (XEON_PHI_DMA_ALIGNMENT - 1)) == 0))

static inline void clear_descriptor(void *descriptor)
{
    uint64_t *desc = descriptor;
    desc[0] = 0;
    desc[1] = 0;
}

/*
 * ============================================================================
 * Public Interface
 * ============================================================================
 */

/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for memcpy descriptors
 *
 * \param desc  Xeon Phi descriptor
 * \param src   Source address of the transfer
 * \param dst   destination address of the transfer
 * \param size  number of bytes to copy
 * \param flags control flags
 *
 * XXX: this function assumes that the size of the descriptor has already been
 *      checked and must match the maximum transfer size of the channel
 */
inline void xeon_phi_dma_desc_fill_memcpy(struct dma_descriptor *desc,
                                          lpaddr_t src,
                                          lpaddr_t dst,
                                          uint32_t size,
                                          uint32_t flags)
{
    uint8_t *d = dma_desc_get_desc_handle(desc);

    clear_descriptor(d);

    ASSERT_ALIGNED(src);
    ASSERT_ALIGNED(dst);
    ASSERT_ALIGNED(size);

    if (flags & XEON_PHI_DMA_DESC_FLAG_INTR) {
        xeon_phi_dma_desc_memcpy_intr_insert(d, 0x1);
    }
    if (flags & XEON_PHI_DMA_DESC_FLAG_TWB) {
        xeon_phi_dma_desc_memcpy_twb_insert(d, 0x1);
    }
    if (flags & XEON_PHI_DMA_DESC_FLAG_C) {
        xeon_phi_dma_desc_memcpy_c_insert(d, 0x1);
    }
    if (flags & XEON_PHI_DMA_DESC_FLAG_CO) {
        xeon_phi_dma_desc_memcpy_co_insert(d, 0x1);
    }
    if (flags & XEON_PHI_DMA_DESC_FLAG_ECY) {
        xeon_phi_dma_desc_memcpy_ecy_insert(d, 0x1);
    }

    xeon_phi_dma_desc_memcpy_src_insert(d, src);
    xeon_phi_dma_desc_memcpy_dst_insert(d, dst);
    xeon_phi_dma_desc_memcpy_length_insert(d, (size >> XEON_PHI_DMA_ALIGN_SHIFT));
    xeon_phi_dma_desc_memcpy_dtype_insert(d, xeon_phi_dma_desc_memcpy);

}

/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for nop descriptors (null descriptors)
 *
 * \param desc  Xeon Phi descriptor
 */
inline void xeon_phi_dma_desc_fill_nop(struct dma_descriptor *desc)
{
    clear_descriptor(dma_desc_get_desc_handle(desc));
}

/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for general descriptors
 *
 * \param desc  Xeon Phi descriptor
 * \param dst   destination address
 * \param data  Data payload for the request (request specific)
 */
inline void xeon_phi_dma_desc_fill_general(struct dma_descriptor *desc,
                                           lpaddr_t dst,
                                           uint64_t data)
{
    uint8_t *d = dma_desc_get_desc_handle(desc);

    clear_descriptor(d);

    ASSERT_ALIGNED(dst);

    xeon_phi_dma_desc_general_data_insert(d, data);
    xeon_phi_dma_desc_general_dst_insert(d, dst);

    xeon_phi_dma_desc_general_dtype_insert(d,
    xeon_phi_dma_desc_general);
}

/**
 * \brief initializes the hardware specific part of the descriptor to be used
 *        for status descriptors
 *
 * \param desc  Xeon Phi descriptor
 * \param dst   destination address
 * \param data  Data payload for the request (request specific)
 * \param flags Descriptor flags
 */
inline void xeon_phi_dma_desc_fill_status(struct dma_descriptor *desc,
                                          lpaddr_t dst,
                                          uint64_t data,
                                          uint32_t flags)
{
    uint8_t *d = dma_desc_get_desc_handle(desc);

    clear_descriptor(d);

    ASSERT_ALIGNED(dst);

    xeon_phi_dma_desc_status_data_insert(d, data);
    xeon_phi_dma_desc_status_dst_insert(d, dst);
    if (flags & XEON_PHI_DMA_DESC_FLAG_INTR) {
        xeon_phi_dma_desc_status_intr_insert(d, 0x1);
    }

    xeon_phi_dma_desc_status_dtype_insert(d,
    xeon_phi_dma_desc_status);
}
