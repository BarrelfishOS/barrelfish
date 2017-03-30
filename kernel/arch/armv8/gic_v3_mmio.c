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
#include <dev/gic_v3_dev.h>
#include <dev/gic_v3_cpu_dev.h>
#include <platform.h>
#include <paging_kernel_arch.h>
#include <arch/armv8/gic_v3.h>

static gic_v3_t gic_v3_dev;
static gic_v3_cpu_t gic_v3_cpu_dev;

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
    printk(LOG_NOTE, "gicv3_init (mem-mapped CPU if) enter\n");

    lvaddr_t gic_dist = local_phys_to_mem(platform_get_distributor_address());
    gic_v3_initialize(&gic_v3_dev, (char *)gic_dist);

    lvaddr_t gic_cpu = local_phys_to_mem(platform_get_gic_cpu_address());
    gic_v3_cpu_initialize(&gic_v3_cpu_dev, (char *)gic_cpu);

    if(gic_v3_GICD_CTLR_secure_DS_rdf(&gic_v3_dev)){
        printk(LOG_NOTE, "gicv3_init: GIC supports secure mode\n"); 
    } else {
        printk(LOG_NOTE, "gicv3_init: GIC does not support secure mode\n"); 
    }

    printk(LOG_NOTE, "gicv3_init done\n");


    return SYS_ERR_OK;
}

/*
 * Returns active interrupt of group 1 
 */
uint32_t gicv3_get_active_irq(void)
{
    return gic_v3_cpu_IAR_intid_rdf(&gic_v3_cpu_dev);
}

/*
 * ACKs group 1 interrupt
 */
void gicv3_ack_irq(uint32_t irq)
{
    gic_v3_cpu_EOIR_t reg = 0;
    reg = gic_v3_cpu_EOIR_intid_insert(reg, irq);
    gic_v3_cpu_EOIR_rawwr(&gic_v3_cpu_dev, irq);
}

/*
 * Raise an SGI on a core. 
 */
void gicv3_raise_softirq(coreid_t cpuid, uint8_t irq)
{
    // assuming affinity routing DISABLED
    assert(irq <= 15);
    gic_v3_GICD_SGIR_t reg = 0;
    reg = gic_v3_GICD_SGIR_INTID_insert(reg, irq);
    reg = gic_v3_GICD_SGIR_CPUTargetList_insert(reg, 1<<(cpuid-1));
    gic_v3_GICD_SGIR_wr(&gic_v3_dev, reg);
    /*
    armv8_ICC_SGI1R_EL1_t reg = 0;
    reg = armv8_ICC_SGI1R_EL1_intid_insert(reg, 1);
    // TODO: make that work for cpuids > 15
    reg = armv8_ICC_SGI1R_EL1_target_insert(reg, 1<<(cpuid-1));
    reg = armv8_ICC_SGI1R_EL1_aff3_insert(reg, 0);
    reg = armv8_ICC_SGI1R_EL1_aff2_insert(reg, 0);
    reg = armv8_ICC_SGI1R_EL1_aff1_insert(reg, 0);
    armv8_ICC_SGI1R_EL1_wr(NULL, reg);
    */
}

/*
 * Enable GIC CPU-IF and local distributor
 */
errval_t gicv3_cpu_interface_enable(void)
{
    printk(LOG_NOTE, "gicv3_cpu_interface_enable: enabling group 1 int\n");

    printk(LOG_NOTE, "GICC IIDR "
            "implementer=0x%x, revision=0x%x, variant=0x%x, prodid=0x%x\n",
            gic_v3_cpu_IIDR_Implementer_rdf(&gic_v3_cpu_dev),
            gic_v3_cpu_IIDR_Revision_rdf(&gic_v3_cpu_dev),
            gic_v3_cpu_IIDR_Variant_rdf(&gic_v3_cpu_dev),
            gic_v3_cpu_IIDR_ProductID_rdf(&gic_v3_cpu_dev)
            );

    // Linux does: 
    // sets priority mode: PMR to 0xf0
    gic_v3_cpu_PMR_wr(&gic_v3_cpu_dev, 0xf0);
    // Set binary point to 1, 6 group priority bits, 2 subpriority bits
    gic_v3_cpu_BPR_wr(&gic_v3_cpu_dev, 1);

    //Enable group 1
    gic_v3_cpu_CTLR_NS_wr(&gic_v3_cpu_dev, 0x1);
    printk(LOG_NOTE, "gicv3_cpu_interface_enable: group 1 int enabled\n");

    printk(LOG_NOTE, "gicv3_cpu_interface_enable: configuring distributor\n");
    printk(LOG_NOTE, "GICD IIDR "
            "implementer=0x%x, revision=0x%x, variant=0x%x,prodid=0x%x\n",
            gic_v3_GICD_IIDR_Implementer_rdf(&gic_v3_dev),
            gic_v3_GICD_IIDR_Revision_rdf(&gic_v3_dev),
            gic_v3_GICD_IIDR_Variant_rdf(&gic_v3_dev),
            gic_v3_GICD_IIDR_ProductID_rdf(&gic_v3_dev)
            );

    gic_v3_GICD_CTLR_secure_t ctrl = 0;
    // Set affinity routing
    ctrl = gic_v3_GICD_CTLR_secure_ARE_NS_insert(ctrl, 1);
    // Enable group 1 interrupts
    ctrl = gic_v3_GICD_CTLR_secure_EnableGrp1NS_insert(ctrl, 1);
    gic_v3_GICD_CTLR_secure_wr(&gic_v3_dev, ctrl);

    return SYS_ERR_OK;
}


