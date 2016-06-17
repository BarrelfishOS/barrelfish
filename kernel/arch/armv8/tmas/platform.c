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
#include <offsets.h>

#include <errno.h>

#include <arch/armv8/arm_hal.h>
#include <arch/armv8/sysreg.h>

//
// Interrupt controller
//

// DIST base address
#define GIC_DIST_BASE    0x400100000
// DIST size, 8kiB
#define GIC_DIST_SIZE    (1 << 13)

// CPU interface base address
#define GIC_CPU_BASE    0x400080000
// CPU interface size, 2 64kiB blocks
#define GIC_CPU_SIZE    (1 << 17)

void gic_map_and_init(pl130_gic_t *gic)
{
    mackerel_addr_t gic_dist, gic_cpu;

    gic_dist = (mackerel_addr_t) (KERNEL_OFFSET + GIC_DIST_BASE);
    gic_cpu = (mackerel_addr_t) (KERNEL_OFFSET + GIC_CPU_BASE);
    pl130_gic_initialize(gic, gic_dist, gic_cpu);
}

bool hal_cpu_is_bsp(void)
{
    return sysreg_get_cpu_id() == 0;
}

uint32_t tsc_read(void)
{
    // Timers count down so invert it.
    return 0;
}

uint32_t tsc_get_hz(void)
{
    return 1;
}
