/**
 * \file plat_a57mpcore.c
 * \brief 
 */


/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * The GIC registers are memory-mapped, with a physical base address specified
 * by PERIPHBASE[43:18]. This input must be tied to a constant value. The
 * PERIPHBASE value is sampled during reset into the Configuration Base Address
 * Register (CBAR) for each processor in the MPCore device. See Configuration
 * Base Address Register, EL1 and Configuration Base Address Register.
 */

#include <maps/a57mpcore_map.h>
#include <kernel.h>
#include <platform.h>
#include <paging_kernel_arch.h>

static lpaddr_t periphbase = 0;

/**
 * @brief returns the private memory region
 *
 * @return physical address of the CBAR region
 */
lpaddr_t platform_get_private_region(void) {
    if(periphbase == 0) return sysreg_read_cbar();
    else                return periphbase;
}

/**
 * @brief obtain the address of the GIC CPU interface
 *
 * @return physical address of the CBAR region
 */
lpaddr_t platform_get_gic_cpu_address(void) {
    assert(paging_mmu_enabled());
    if (platform_gic_cpu_base == 0) {
        return platform_get_private_region() + A57MPCORE_GIC_CPU_OFFSET;
    } else {
        return platform_gic_cpu_base;
    }
}

/**
 * @brief returns the size of the GIC cpu region
 * @return
 */
size_t platform_get_gic_cpu_size(void) {
    return A57MPCORE_GIC_CPU_SIZE;
}

lpaddr_t platform_get_distributor_address(void)
{
    if (platform_gic_dist_base == 0) {
        return platform_get_private_region() + A57MPCORE_GICH_CPU_OFFSET;
    } else {
        return platform_gic_dist_base;
    }
}

lpaddr_t platform_get_distributor_size(void)
{
    return A57MPCORE_GICH_CPU_SIZE;
}



