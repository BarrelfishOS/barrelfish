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
#include <arch/arm/gic.h>
#include <platform.h>

//
// Interrupt controller
//

// DIST base address
#define GIC_DIST_BASE    0x400100000

// CPU interface base address
#define GIC_CPU_BASE    0x400080000


lpaddr_t platform_get_distributor_address(void) {
    return GIC_DIST_BASE;
}

lpaddr_t platform_get_gic_cpu_address(void) {
    return GIC_CPU_BASE;
}

void platform_get_info(struct platform_info *pi)
{
    pi->arch     = PI_ARCH_ARMV8A;
    pi->platform = PI_PLATFORM_TMAS;
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
