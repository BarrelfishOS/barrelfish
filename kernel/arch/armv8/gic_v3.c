/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

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
void gicv3_init(void)
{
    armv8_initialize(&armv8_dev);
    // Enable system register access
    armv8_ICC_SRE_EL1_SRE_wrf(&armv8_dev, 1);

    printk(LOG_NOTE, "gic_init done\n");
}

