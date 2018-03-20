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
#include <platform.h>
#include <serial.h>
#include <arch/arm/pl011.h>
#include <arch/armv8/gic_v3.h>

// #include <dev/apm88xxxx/apm88xxxx_pc16550_dev.h>
// #include <arch/arm/gic.h>
#include <sysreg.h>
#include <dev/armv8_dev.h>
#include <barrelfish_kpi/arm_core_data.h>
#include <psci.h>
#include <arch/armv8/global.h>
// #include <arch/armv8/gic_v3.h>
// #include <paging_kernel_arch.h>

errval_t serial_init(unsigned port, bool initialize_hw)
{
    lvaddr_t base = local_phys_to_mem(uart_base[port]);
    pl011_init(port, base, initialize_hw);
    return SYS_ERR_OK;
};

/*
 * Return the address of the UART device.
 */
lpaddr_t platform_get_uart_address(unsigned port)
{
    return local_phys_to_mem(uart_base[port]);
}

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
    printf("Invoking PSCI on: cpu=0x%lx, entry=0x%lx, context=0x%lx\n", target, gen_entry, context);
    struct armv8_core_data *cd = (struct armv8_core_data *)local_phys_to_mem(context);
    cd->page_table_root = armv8_TTBR1_EL1_rd(NULL);
    cd->cpu_driver_globals_pointer = (uintptr_t)global;
    __asm volatile("dsb   sy\n"
                   "dmb   sy\n"
                   "isb     \n");
    return psci_cpu_on(target, gen_entry, context);
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
    pi->platform = PI_PLATFORM_QEMU;
}

void armv8_get_info(struct arch_info_armv8 *ai)
{

}

errval_t platform_gic_init(void) {
    return gicv3_init();
}

errval_t platform_gic_cpu_interface_enable(void) {
    return gicv3_cpu_interface_enable();
}
