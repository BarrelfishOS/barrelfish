/**
 * \file plat_arm_vm.c
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

#include <kernel.h>
#include <offsets.h>
#include <arch/arm/platform.h>
#include <serial.h>
#include <arch/arm/pl011.h>
#include <arch/arm/gic.h>

errval_t serial_init(unsigned port, bool initialize_hw)
{
    lvaddr_t base = local_phys_to_mem(platform_uart_base[port]);
    pl011_init(port, base, initialize_hw);
    return SYS_ERR_OK;
};

/*
 * Do any extra initialisation for this particular CPU (e.g. A9/A15).
 */
void platform_revision_init(void)
{

}

/*
 * Figure out how much RAM we have
 */
size_t platform_get_ram_size(void)
{
    return 0;
}

/*
 * Boot secondary processors
 */
errval_t platform_boot_core(hwid_t target, genpaddr_t gen_entry, genpaddr_t context)
{
    return 0;
}

void platform_notify_bsp(lpaddr_t *mailbox)
{

}


/*
 * Return the core count
 */
size_t platform_get_core_count(void)
{
    return 0;
}

/*
 * Print system identification. MMU is NOT yet enabled.
 */
void platform_print_id(void)
{

}

/*
 * Fill out provided `struct platform_info`
 */
void platform_get_info(struct platform_info *pi)
{
    pi->arch = PI_ARCH_ARMV8A;
    pi->platform = PI_PLATFORM_FVP;
}

void armv8_get_info(struct arch_info_armv8 *ai)
{

}

uint32_t platform_get_timer_interrupt(void){
    // TODO (LH): Untested
    return 30;
}
