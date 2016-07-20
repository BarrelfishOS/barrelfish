/*
 * Copyright (c) 2012,2015, ETH Zurich.
 * Copyright (c) 2015-2016, Hewlett Packard Enterprise Development LP.
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
#include <arch/armv8/platform.h>


struct platform_data {
    lpaddr_t gic_dist_base;
    lpaddr_t gic_cpu_base;
    lpaddr_t uart_base;
};

/*
 * Platform data, initialized to TMAS defaults.
 */
static struct platform_data platform_data = {
        .gic_dist_base = 0x400100000,
        .gic_cpu_base = 0x400080000,
        .uart_base = 0x402020000,
};

// TODO: Integrate this with platform_XXX functions.
lpaddr_t uart_base[] = { 0x402020000 | KERNEL_OFFSET };

size_t uart_size[] = {
        0x1000
};


//
// Interrupt controller
//

lpaddr_t platform_get_distributor_address(void) {
    return platform_data.gic_dist_base;
}

void platform_set_distributor_address(lpaddr_t gic_dist_base) {
    platform_data.gic_dist_base = gic_dist_base;
}

lpaddr_t platform_get_gic_cpu_address(void) {
    return platform_data.gic_cpu_base;
}

void platform_set_gic_cpu_address(lpaddr_t gic_cpu_base) {
    platform_data.gic_cpu_base = gic_cpu_base;
}

lpaddr_t platform_get_uart_address(void) {
    return platform_data.uart_base;
}

void platform_set_uart_address(lpaddr_t uart_base_) {
    platform_data.uart_base = uart_base_;
    uart_base[0] = uart_base_;
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
