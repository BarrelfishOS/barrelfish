/**
 * \file
 * \brief Private memory region layout for the Cortex-A9 MPCore
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

/* Offsets taken from ARM Cortex A9 MPCore TRM Table 1-3. */

#ifndef A9MPCORE_MAP_H
#define A9MPCORE_MAP_H

#define A9MPCORE_SCU_OFFSET       0x0000
#define A9MPCORE_SCU_SIZE         0x0100

#define A9MPCORE_GIC_CPU_OFFSET   0x0100
#define A9MPCORE_GIC_CPU_SIZE     0x0100

#define A9MPCORE_TIMER_GBL_OFFSET 0x0200
#define A9MPCORE_TIMER_GBL_SIZE   0x0100

#define A9MPCORE_TIMER_LCL_OFFSET 0x0600
#define A9MPCORE_TIMER_LCL_SIZE   0x0100

#define A9MPCORE_GIC_DIST_OFFSET  0x1000
#define A9MPCORE_GIC_DIST_SIZE    0x1000

#endif /* A9MPCORE_MAP_H */
