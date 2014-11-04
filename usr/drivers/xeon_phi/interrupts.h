/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_INTERRUPTS_H_
#define XEON_PHI_INTERRUPTS_H_

#include <dev/xeon_phi/xeon_phi_irq_dev.h>

// forward declaration
struct xeon_phi;

#define PASTER(x,y) uint ## x ## y
#define EVALUATOR(x)  PASTER(x,_t)

#define MSIX_ALLOCATOR_BITS 8
#define MSIX_ALLOCATOR_TYPE EVALUATOR(MSIX_ALLOCATOR_BITS)

#define XEON_PHI_NUM_MSIX_ENTRIES 1

/**
 *
 */
struct msix_allocator
{
    size_t   count;
    MSIX_ALLOCATOR_TYPE *state;
};

struct irq_info
{
    uint8_t                 irq_enabled;
    uint8_t                 msix_enabled;
    uint16_t                msix_count;
    xeon_phi_irq_t          irq_registers;
    struct msix_allocator   msix_alloc;
};


/**
 * \brief Initializes the MSI-X Interrupt allocator
 *
 * \param alloc pointer to the allocator state struct
 * \param n     number of vectors to track
 *
 * \return true on SUCCESS, false on FAILURE
 */
bool msix_allocator_init(struct msix_allocator *alloc, size_t n);

/**
 * \brief Destroys a previously allocated MSI-X Interrupt allocator
 *
 * \param alloc the allocator to destroy
 *
 * \return true on SUCCESS, false on FAILURE
 */
void msix_allocator_destory(struct msix_allocator *alloc);

/**
 * \brief Allocates a new MSI-X vector ID
 *
 * \param alloc  pointer to the allocator state struct
 * \param ret_id returned MSI-X vector ID
 *
 * \return true on SUCCESS, false on FAILURE (no more vectors left)
 */
bool msix_allocator_new(struct msix_allocator *alloc, size_t *ret_id);

/**
 * \brief Frees a previously allocated id
 *
 * \param alloc pointer to the allocator state struct
 * \param n     number of vectors to track
 *
 * \return true on if it was previously allocated, false on if not
 */
bool msix_allocator_free(struct msix_allocator *alloc, size_t id);


/**
 * \brief Enables the MSI-X interrupts
 *
 * \param phi   the xeon phi structure
 *
 * \return SYS_ERR_OK on success
 */
errval_t msix_enable(struct xeon_phi *phi);

/**
 * \brief Initializes the interrupts data structures
 *
 * \param phi   the xeon phi structure
 *
 * \return SYS_ERR_OK on success
 */
errval_t interrupts_init(struct xeon_phi *phi);


/**
 * \brief Enables the Xeon Phi Interrupts
 *
 * \param phi   the xeon phi structure
 */
static inline void interrupts_enable(struct xeon_phi *phi)
{
    xeon_phi_irq_int_enable_t val = xeon_phi_irq_int_enable_default;
    val = xeon_phi_irq_int_enable_dbr_insert(val, xeon_phi_irq_dbr_enable_all);
    val = xeon_phi_irq_int_enable_dma_insert(val, xeon_phi_irq_dma_enable_all);
    xeon_phi_irq_int_enable_wr(&phi->irq->irq_registers, val);

    phi->irq->irq_enabled = 1;
}

/**
 * \brief Disables the Xeon Phi Interrupts
 *
 * \param phi   the xeon phi structure
 */
static inline void interrupts_disable(struct xeon_phi *phi)
{
    xeon_phi_irq_int_enable_t val = xeon_phi_irq_int_enable_rd(&phi->irq->irq_registers);
    xeon_phi_irq_int_disable_wr(&phi->irq->irq_registers, val);

    phi->irq->irq_enabled = 0;
}
/**
 *
 */
void interrupt_handler(void* arg);

#endif /* XEON_PHI_INTERRUPTS_H_ */
