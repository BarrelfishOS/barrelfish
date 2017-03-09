/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <sysreg.h>
#include <dev/armv8_dev.h>
#include <platform.h>
#include <paging_kernel_arch.h>
#include <arch/armv8/gic_v3.h>

static armv8_t armv8_dev;

/*
 * Initialize the global interrupt controller
 *
 * There are three types of interrupts
 * 1) Software generated Interrupts (SGI) - IDs 0-15
 * 2) Private Peripheral Interrupts (PPI) - IDs 16-31
 * 3) Shared Peripheral Interrups (SPI) - IDs 32...
 */
errval_t gicv3_init(void)
{
    armv8_initialize(&armv8_dev);
    // Enable system register access
    armv8_ICC_SRE_EL1_SRE_wrf(&armv8_dev, 1);

    printk(LOG_NOTE, "gicv3_init done\n");
    return SYS_ERR_OK;
}

/*
 * Returns active interrupt of group 1 
 */
uint32_t gicv3_get_active_irq(void)
{
    armv8_ICC_IAR1_EL1_t iar = armv8_ICC_IAR1_EL1_rd(NULL);
    return armv8_ICC_IAR1_EL1_intid_extract(iar);
}

/*
 * ACKs group 1 interrupt
 */
void gicv3_ack_irq(uint32_t irq)
{
    armv8_ICC_EOIR0_EL1_rawwr(NULL, irq);
}

errval_t gicv3_cpu_interface_enable(void)
{
    //TODO: GICD_CTLR: set affinity routing
    printk(LOG_NOTE, "gicv3_cpu_interface_enable: enabling group 1 int\n");

    // Linux does: 
    // sets priority mode: PMR to 0xf0
    armv8_ICC_PMR_EL1_wr(NULL, 0xf0);
    // Set binary point to 1, 6 group priority bits, 2 subpriority bits
    armv8_ICC_BPR1_EL1_wr(NULL, 1);

    //Enable group 1
    armv8_ICC_IGRPEN1_EL1_wr(NULL, 0x1);
    printk(LOG_NOTE, "gicv3_cpu_interface_enable: group 1 int enabled\n");

    return SYS_ERR_OK;
}
