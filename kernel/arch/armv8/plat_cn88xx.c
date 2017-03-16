/**
 * \file plat_arm_vm_consts.c
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
#include <maps/cn88xx.h>
#include <offsets.h>
#include <platform.h>
#include <serial.h>
#include <psci.h>
#include <arch/arm/pl011.h>
#include <arch/armv8/gic_v3.h>
#include <arch/armv8/global.h>
#include <sysreg.h>
#include <dev/armv8_dev.h>
#include <barrelfish_kpi/arm_core_data.h>


lpaddr_t phys_memory_start = CN88XX_MAP_LMC_OFFSET;

/*
 * ----------------------------------------------------------------------------
 * GIC
 * ----------------------------------------------------------------------------
 */

lpaddr_t platform_gic_cpu_base  = CN88XX_MAP_GIC_GICD_OFFFSET;
lpaddr_t platform_gic_dist_base = CN88XX_MAP_GIC_CCS_OFFSET;

/*
 * ----------------------------------------------------------------------------
 * UART
 * ----------------------------------------------------------------------------
 */

/* the maximum number of UARTS supported */
#define MAX_NUM_UARTS 2

/* the serial console port */
unsigned int serial_console_port = 0;

/* the debug console port */
unsigned int serial_debug_port = 0;

/* the number of physical ports */
unsigned serial_num_physical_ports = 1;

/* uart bases */
lpaddr_t uart_base[MAX_NUM_UARTS]= {
    CN88XX_MAP_UART0_OFFSET,
    CN88XX_MAP_UART1_OFFSET
};

/* uart sizes */
size_t uart_size[MAX_NUM_UARTS]= {
    4096, 4096
};


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
    printf("Invoking PSCI on: cpu=%lx, entry=%lx, context=%lx\n", target, gen_entry, context);
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
    return 48;
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
    pi->platform = PI_PLATFORM_CN88XX;
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
    return platform_gic_cpu_base;
}

/**
 * @brief returns the size of the GIC cpu region
 * @return
 */
size_t platform_get_gic_cpu_size(void) {
    return CN88XX_MAP_GIC_CCS_SIZE;
}

lpaddr_t platform_get_distributor_address(void)
{
    return CN88XX_MAP_GIC_GICD_OFFFSET;
}

lpaddr_t platform_get_distributor_size(void)
{
    return CN88XX_MAP_GIC_GICD_SIZE;
}
