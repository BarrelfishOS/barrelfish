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
#include <dev/gic_v3_dist_dev.h>
#include <dev/gic_v3_redist_dev.h>
#include <arch/arm/platform.h>
#include <paging_kernel_arch.h>
#include <arch/arm/gic.h>
#include <irq.h>
#include <getopt/getopt.h>

static gic_v3_dist_t gic_v3_dist_dev;
static gic_v3_redist_t gic_v3_redist_dev;

extern lpaddr_t platform_gic_distributor_base;
extern lpaddr_t platform_gic_redistributor_base;

// Command line arguments
static struct cmdarg cmdargs[] = {
    {"gicdist", ArgType_ULong, { .ulonginteger = &platform_gic_distributor_base }},
    {"gicredist", ArgType_ULong, { .ulonginteger = &platform_gic_redistributor_base }}
};

/*
 * Initialize the global interrupt controller
 *
 * There are three types of interrupts
 * 1) Software generated Interrupts (SGI) - IDs 0-15
 * 2) Private Peripheral Interrupts (PPI) - IDs 16-31
 * 3) Shared Peripheral Interrups (SPI) - IDs 32-1019
 * 4) Special - IDs 1020-1023
 * 5) Locality-specific Peripheral Interrups (LPI) - IDs 8192-...
 */
void gic_init(void)
{
    printk(LOG_NOTE, "GICv3: Initializing\n");
    parse_commandline(kernel_command_line, cmdargs);
    lvaddr_t gic_dist = local_phys_to_mem(platform_gic_distributor_base);
    gic_v3_dist_initialize(&gic_v3_dist_dev, (char *)gic_dist);

    printk(LOG_NOTE, "GICD IIDR "
            "implementer=0x%x, revision=0x%x, variant=0x%x,prodid=0x%x\n",
            gic_v3_dist_GICD_IIDR_Implementer_rdf(&gic_v3_dist_dev),
            gic_v3_dist_GICD_IIDR_Revision_rdf(&gic_v3_dist_dev),
            gic_v3_dist_GICD_IIDR_Variant_rdf(&gic_v3_dist_dev),
            gic_v3_dist_GICD_IIDR_ProductID_rdf(&gic_v3_dist_dev)
            );

    uint32_t itlines = gic_v3_dist_GICD_TYPER_ITLinesNumber_rdf(&gic_v3_dist_dev);
    itlines = (itlines + 1) * 32;
    if (itlines > 1020)
        itlines = 1020;
    printk(LOG_NOTE, "gic: #INTIDs supported: %" PRIu32 "\n", itlines);

    // Put all interrupts into Group 1 and disable them
    #define MASK_32     0xffffffff
    for (int i = 0; i * 32 < itlines; i++) {
        // Clear
        gic_v3_dist_GICD_ICACTIVER_wr(&gic_v3_dist_dev, i, MASK_32);
        // Disable all interrupts
        gic_v3_dist_GICD_ICENABLER_wr(&gic_v3_dist_dev, i, MASK_32);
        // And put in group 1
        gic_v3_dist_GICD_IGROUPR_rawwr(&gic_v3_dist_dev, i, MASK_32);
    }
    gic_v3_dist_GICD_CTLR_secure_t ctrl = 0;
    // Set affinity routing (redundant on CN88xx)
    ctrl = gic_v3_dist_GICD_CTLR_secure_ARE_NS_insert(ctrl, 1);
    // Enable group 1 interrupts
    ctrl = gic_v3_dist_GICD_CTLR_secure_EnableGrp1NS_insert(ctrl, 1);
    gic_v3_dist_GICD_CTLR_secure_wr(&gic_v3_dist_dev, ctrl);

    printk(LOG_NOTE, "GICv3: Initialized\n");
}

/*
 * Returns active interrupt of group 1
 */
uint32_t platform_get_active_irq(void)
{
    return armv8_ICC_IAR1_EL1_INTID_rdf(NULL);
}

/*
 * ACKs group 1 interrupt
 */
void platform_acknowledge_irq(uint32_t irq)
{
    armv8_ICC_EOIR1_EL1_rawwr(NULL, irq);
}

/*
 * Raise an SGI on a core.
 */
void gic_raise_softirq(coreid_t cpuid, uint8_t irq)
{
    assert(irq <= 15);
    armv8_ICC_SGI1R_EL1_t reg = 0;
    reg = armv8_ICC_SGI1R_EL1_INTID_insert(reg, 1);
    // TODO: make that work for cpuids > 15
    reg = armv8_ICC_SGI1R_EL1_TargetList_insert(reg, 1<<cpuid);
    reg = armv8_ICC_SGI1R_EL1_Aff3_insert(reg, 0);
    reg = armv8_ICC_SGI1R_EL1_Aff2_insert(reg, 0);
    reg = armv8_ICC_SGI1R_EL1_Aff1_insert(reg, 0);
    armv8_ICC_SGI1R_EL1_wr(NULL, reg);
}

/*
 * Enable GIC CPU-IF and a redistributor
 */
void gic_cpu_interface_enable(void)
{
    printk(LOG_NOTE, "GICv3: Enabling CPU interface\n");

    lvaddr_t gic_redist = local_phys_to_mem(platform_gic_redistributor_base);

    // Enable system register access
    armv8_ICC_SRE_EL1_SRE_wrf(NULL, 1);

// second socket of ThunderX hack
    if (my_core_id >= 48) {
        gic_v3_redist_initialize(&gic_v3_redist_dev, (char *)gic_redist + 0x100000000000 + 0x20000 * (my_core_id - 48));
    } else {
        gic_v3_redist_initialize(&gic_v3_redist_dev, (char *)gic_redist + 0x20000 * my_core_id);
    }

    // Linux does:
    // sets priority mode: PMR to 0xf0
    armv8_ICC_PMR_EL1_wr(NULL, 0xf0);
    // Set binary point to 1, 6 group priority bits, 2 subpriority bits
    armv8_ICC_BPR1_EL1_wr(NULL, 1);

    //Enable group 1
    armv8_ICC_IGRPEN1_EL1_wr(NULL, 0x1);

    gic_v3_redist_GICR_TYPER_t gicr_typer;
    gicr_typer = gic_v3_redist_GICR_TYPER_rd(&gic_v3_redist_dev);

    gic_v3_redist_GICR_ICACTIVER0_rawwr(&gic_v3_redist_dev, MASK_32);
    //Disable PPIs
    gic_v3_redist_GICR_ICENABLER0_wr(&gic_v3_redist_dev, MASK_32);

    gic_v3_redist_GICR_IGROUPR0_rawwr(&gic_v3_redist_dev, MASK_32);
    gic_v3_redist_GICR_IGRPMODR0_rawwr(&gic_v3_redist_dev, 0);

    printk(LOG_NOTE, "GICv3: CPU interface enabled\n");
}

/**
 * \brief Enable an interrupt
 *
 * \see ARM Generic Interrupt Controller Architecture Specification v1.0
 *
 * \param int_id
 * \param cpu_targets 8 Bit mask. One bit for each core in the system.
 *    (chapter 4.3.11)
 * \param prio Priority of the interrupt (lower is higher). We allow 0..15.
 *    The number of priority bits is implementation specific, but at least 16
 *    (using bits [7:4] of the priority field, chapter 3.3)
 * \param 0 is level-sensitive, 1 is edge-triggered
 * \param 0 is N-to-N, 1 is 1-N
 */
errval_t platform_enable_interrupt(uint32_t int_id, uint16_t prio,
                          bool edge_triggered, bool one_to_n)
{
    if(int_id<32) {
        gic_v3_redist_GICR_ISENABLER0_wr(&gic_v3_redist_dev, 1<<int_id );
    }
    else {  
        gic_v3_dist_GICD_ISENABLER_wr(&gic_v3_dist_dev, int_id/32,
            1<<(int_id % 32));
    }
    return SYS_ERR_OK;
}

errval_t platform_init_ic_bsp(void)
{
    gic_init();
    gic_cpu_interface_enable();
    return SYS_ERR_OK;
}

errval_t platform_init_ic_app(void)
{
    gic_cpu_interface_enable();
    return SYS_ERR_OK;
}
