/**
 * \file
 * \brief Private memory region layout for the Cortex-A57 MPCore
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

/* Offsets taken from ARM Cortex A53 MPCore TRM Table 8-1. */

#ifndef A53MPCORE_MAP_H
#define A53MPCORE_MAP_H

/*
 * Offset range from PERIPHBASE[43:18]
 *
 * It lists the address offsets for the GIC blocks relative to the PERIPHBASE
 * base address
 */

/* CPU interface */
#define A53MPCORE_GIC_CPU_OFFSET    0x0000
#define A53MPCORE_GIC_CPU_SIZE      0x2000

/* Virtual interface control */
#define A53MPCORE_GICH_CPU_OFFSET   0x10000
#define A53MPCORE_GICH_CPU_SIZE     0x1000

/* Virtual CPU interface (4KB page offset) */
#define A53MPCORE_GICV_OFFSET       0x20000
#define A53MPCORE_GICV_SIZE         0x2000

/* Alias of the Virtual CPU interface (64KB page offset alias) */
#define A53MPCORE_GICV_OFFSET       0x2F000
#define A53MPCORE_GICV_SIZE         0x2000


#endif /* A53MPCORE_MAP_H */
