/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_XEON_PHI_H_
#define XEON_PHI_XEON_PHI_H_

/// The maximum number of coprocessor cards in a system
#define XEON_PHI_NUM_MAX 8


#define XEON_PHY_HOST_MEM_OFFSET 0x8000000000UL

#define XEON_PHI_HOST_TO_CARD_MEM(x) \
    ((lpaddr_t)(x)+XEON_PHY_HOST_MEM_OFFSET)


/**
 * this struct represents the information passed from the host to the
 * coprocessor kernels
 */
struct xeon_phi_info {
    uintptr_t comm_base; // TODO: communication base

    uint8_t present;    ///< flag indicating the present cards in the system
};

#endif // XEON_PHI_XEON_PHI_H_
