/**
 * \file
 * \brief Private memory region layout for the Cortex-A15 MPCore
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

/* Offsets taken from ARM Cortex A15 MPCore TRM Table 8-1. */

#ifndef A15MPCORE_MAP_H
#define A15MPCORE_MAP_H 1

/* Distributor */
#define A15MPCORE_GICD_OFFSET     0x1000
#define A15MPCORE_GICD_SIZE       0x1000

/* CPU interface */
#define A15MPCORE_GICC_OFFSET     0x2000
#define A15MPCORE_GICC_SIZE       0x1000

/* Local virtual interface */
#define A15MPCORE_GICH_COM_OFFSET 0x4000
#define A15MPCORE_GICH_COM_SIZE   0x1000

/* Per-CPU virtual interfaces */
#define A15MPCORE_GICH_CPU_OFFSET 0x5000
#define A15MPCORE_GICH_CPU_SIZE   0x1000

/* Virtual CPU interface */
#define A15MPCORE_GICV_OFFSET     0x6000
#define A15MPCORE_GICV_SIZE       0x1000

#endif /* A15MPCORE_MAP_H */
