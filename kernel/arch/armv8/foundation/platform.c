/*
 * Copyright (c) 2012,2015, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <arch/arm/gic.h>
#include <dev/pl130_gic_dev.h>
#include <kernel.h>

//
// Interrupt controller
//

#define GIC_BASE    0x2C000000
#define DIST_OFFSET 0x1000
#define CPU_OFFSET  0x2000

void gic_map_and_init(pl130_gic_t *gic)
{
    pl130_gic_initialize(gic,
            (mackerel_addr_t)GIC_BASE + DIST_OFFSET,
            (mackerel_addr_t)GIC_BASE + CPU_OFFSET);
}
