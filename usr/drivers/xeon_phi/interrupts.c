/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <pci/pci.h>

#include "xeon_phi_internal.h"
#include "interrupts.h"

xeon_phi_irq_t irq_registers;

/**
 * \brief Initializes the MSI-X Interrupt allocator
 *
 * \param alloc pointer to the allocator state struct
 * \param n     number of vectors to track
 *
 * \return true on SUCCESS, false on FAILURE
 */
bool msix_allocator_init(struct msix_allocator *alloc, size_t n)
{
    alloc->count = n;
    alloc->state = calloc((n + MSIX_ALLOCATOR_BITS -1 ) / MSIX_ALLOCATOR_BITS,
                          MSIX_ALLOCATOR_BITS / 8);
    return (alloc->state != NULL);
}


/**
 * \brief Destroys a previously allocated MSI-X Interrupt allocator
 *
 * \param alloc the allocator to destroy
 */
void msix_allocator_destory(struct msix_allocator *alloc)
{
    if (alloc->state) {
        free(alloc->state);
    }
    alloc->state = NULL;
    alloc->count = 0;
}

/**
 * \brief Allocates a new MSI-X vector ID
 *
 * \param alloc  pointer to the allocator state struct
 * \param ret_id returned MSI-X vector ID
 *
 * \return true on SUCCESS, false on FAILURE (no more vectors left)
 */
bool msix_allocator_new(struct msix_allocator *alloc, size_t *ret_id)
{
    size_t chunks = (alloc->count + MSIX_ALLOCATOR_BITS - 1) / MSIX_ALLOCATOR_BITS;

    for (uint32_t i = 0; i < chunks; i++) {
        if (alloc->state[i] != 0xFF) {

            uint32_t idx = 0;
            for (uint32_t j = i * MSIX_ALLOCATOR_BITS;  j < (i+1) * MSIX_ALLOCATOR_BITS; ++j) {
                if (!(j < alloc->count)) {
                    return false;
                }

                if (!(alloc->state[i] & (0x1 << idx))) {
                    alloc->state[i] |= (0x1 << idx);
                    *ret_id = j;
                    return true;
                }
                idx++;
            }
        }
    }
    return false;
}


/**
 * \brief Frees a previously allocated id
 *
 * \param alloc pointer to the allocator state struct
 * \param n     number of vectors to track
 *
 * \return true on if it was previously allocated, false on if not
 */
bool msix_allocator_free(struct msix_allocator *alloc, size_t id)
{
    bool result;

    if (id >= alloc->count) {
        return false;
    }

    size_t chunk = (id  / MSIX_ALLOCATOR_BITS);
    size_t shift = id - (chunk * MSIX_ALLOCATOR_BITS);
    result = alloc->state[chunk] & (0x1 << shift);
    alloc->state[chunk] &= ~(0x1 << shift);

    return result;

}

/**
 * \brief enables the MSI-X Interrupts
 *
 * \param phi   the xeon phi device data structure
 *
 * \return SYS_ERR_OK on success
 */
errval_t msix_enable(struct xeon_phi *phi)
{
    errval_t err;

    if (!phi->irq->msix_enabled) {
        return SYS_ERR_OK;
    }
    err = pci_msix_enable(&phi->irq->msix_count);
    if (err_is_fail(err)) {
        return err;
    }

    if (!msix_allocator_init(&phi->irq->msix_alloc, phi->irq->msix_count)) {
        return LIB_ERR_MALLOC_FAIL;
    }

    // Only support single MSI interrupt for now
    xeon_phi_irq_msi_vector_t val = xeon_phi_irq_msi_vector_default;
    val = xeon_phi_irq_msi_vector_dbr_insert(val, xeon_phi_irq_dbr_enable_all);
    val = xeon_phi_irq_msi_vector_dma_insert(val, xeon_phi_irq_dma_enable_all);
    xeon_phi_irq_msi_vector_wr(&phi->irq->irq_registers, 0, val);

    phi->irq->msix_enabled = 1;

    return SYS_ERR_OK;
}






errval_t interrupts_init(struct xeon_phi *phi)
{
    phi->irq = calloc(1, sizeof(struct irq_info));
    if (phi->irq == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    xeon_phi_irq_initialize(&phi->irq->irq_registers, XEON_PHI_MMIO_TO_SBOX(phi));

    return SYS_ERR_OK;
}

void interrupt_handler(void* arg)
{

}

