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
#include <dev/gic_v2_cpu_dev.h>
#include <paging_kernel_arch.h>
#include <arch/arm/gic.h>
#include <arch/arm/platform.h>
#include <getopt/getopt.h>

static gic_v3_dist_t gic_v3_dev;
static gic_v2_cpu_t gic_v2_cpu_dev;

extern lpaddr_t platform_gic_cpu_interface_base;
extern lpaddr_t platform_gic_distributor_base;

// Command line arguments
static struct cmdarg cmdargs[] = {
    {"gic", ArgType_ULong, { .ulonginteger = &platform_gic_cpu_interface_base }},
    {"gicdist", ArgType_ULong, { .ulonginteger = &platform_gic_distributor_base }}
};

/*
 * This should return 1<<my_core_id
 */
static uint8_t gic_get_cpumask(void)
{
    uint32_t mask = gic_v3_dist_GICD_ITARGETSR_rd(&gic_v3_dev , 0);
    mask |= mask >> 16;
    mask |= mask >> 8;
    return mask;
}

/*
 * Reads th STATUSR register, prints error on error condition
 */
static void check_cpu_if_statusr(void)
{
    // gic_v2_cpu_STATUSR_t raw =  gic_v2_cpu_STATUSR_rawrd(&gic_v2_cpu_dev);
    // if (raw) {
    //     char buf[512];
    //     gic_v2_cpu_STATUSR_pr(buf,sizeof(buf),&gic_v2_cpu_dev);
    //     printk(LOG_NOTE, "gic_v2: Error condition! Status: %s\n", buf);
    // }
}

/*
 * Initialize the global interrupt controller
 *
 * There are three types of interrupts
 * 1) Software generated Interrupts (SGI) - IDs 0-15
 * 2) Private Peripheral Interrupts (PPI) - IDs 16-31
 * 3) Shared Peripheral Interrups (SPI) - IDs 32...
 */
void gic_init(void)
{
    printk(LOG_NOTE, "GICv2: Initializing\n");
    parse_commandline(kernel_command_line, cmdargs);

    lvaddr_t gic_dist = local_phys_to_mem(platform_gic_distributor_base);
    gic_v3_dist_initialize(&gic_v3_dev, (char *)gic_dist);

    lvaddr_t gic_cpu = local_phys_to_mem(platform_gic_cpu_interface_base);
    gic_v2_cpu_initialize(&gic_v2_cpu_dev, (char *)gic_cpu);

    if (gic_v3_dist_GICD_TYPER_SecurityExtn_rdf(&gic_v3_dev)) {
        printk(LOG_NOTE, "gic_v2: In init. GIC supports secure mode\n");
    } else {
        printk(LOG_NOTE, "gic_v2: In init. GIC does not support secure mode\n");
    }
}

/*
 * Returns active interrupt of group 1
 */
uint32_t platform_get_active_irq(void)
{
    uint32_t res = gic_v2_cpu_IAR_intid_rdf(&gic_v2_cpu_dev);
    check_cpu_if_statusr();
    return res;
}

/*
 * ACKs group 1 interrupt
 */
void platform_acknowledge_irq(uint32_t irq)
{
    gic_v2_cpu_EOIR_t reg = 0;
    reg = gic_v2_cpu_EOIR_intid_insert(reg, irq);
    gic_v2_cpu_EOIR_rawwr(&gic_v2_cpu_dev, irq);
    check_cpu_if_statusr();
}

/*
 * Raise an SGI on a core.
 */
void gic_raise_softirq(coreid_t cpuid, uint8_t irq)
{
    // assuming affinity routing DISABLED
    assert(irq <= 15);
    gic_v3_dist_GICD_SGIR_t reg = 0;
    reg = gic_v3_dist_GICD_SGIR_INTID_insert(reg, irq);
    reg = gic_v3_dist_GICD_SGIR_CPUTargetList_insert(reg, 1<<(cpuid));
    gic_v3_dist_GICD_SGIR_wr(&gic_v3_dev, reg);
    check_cpu_if_statusr();
}

/*
 * Enable GIC CPU-IF and local distributor
 */
void gic_cpu_interface_enable(void)
{
    printk(LOG_NOTE, "gic_v2: GICC IIDR "
            "implementer=0x%x, revision=0x%x, variant=0x%x, prodid=0x%x, raw=0x%x\n",
            gic_v2_cpu_IIDR_Implementer_rdf(&gic_v2_cpu_dev),
            gic_v2_cpu_IIDR_Revision_rdf(&gic_v2_cpu_dev),
            gic_v2_cpu_IIDR_Variant_rdf(&gic_v2_cpu_dev),
            gic_v2_cpu_IIDR_ProductID_rdf(&gic_v2_cpu_dev),
            gic_v2_cpu_IIDR_rawrd(&gic_v2_cpu_dev)
            );

    // Do as Linux does:
    // set priority mode: PMR to 0xf0
    gic_v2_cpu_PMR_wr(&gic_v2_cpu_dev, 0xf0);
    check_cpu_if_statusr();
    // Set binary point to 1: 6 group priority bits, 2 subpriority bits
    gic_v2_cpu_BPR_wr(&gic_v2_cpu_dev, 1);
    check_cpu_if_statusr();

    //We enable both group 0 and 1, but let both trigger IRQs (and not FIQs)
    gic_v2_cpu_CTLR_NS_rawwr(&gic_v2_cpu_dev, 3);  // code for non-secure
    gic_v2_cpu_CTLR_FIQEn_wrf(&gic_v2_cpu_dev, 0); // route both groups to IRQ

    // Disable all GIC bypassing (no wake-up interrupts). This does not
    // seem to have any effect?
    gic_v2_cpu_CTLR_FIQBypDisGrp1_wrf(&gic_v2_cpu_dev, 1);
    gic_v2_cpu_CTLR_FIQBypDisGrp0_wrf(&gic_v2_cpu_dev, 1);
    gic_v2_cpu_CTLR_IRQBypDisGrp1_wrf(&gic_v2_cpu_dev, 1);
    gic_v2_cpu_CTLR_IRQBypDisGrp0_wrf(&gic_v2_cpu_dev, 1);
    check_cpu_if_statusr();

    gic_v3_dist_GICD_CTLR_secure_t ctrl = 0;
    // Set affinity routing (redundant on CN88xx)
    ctrl = gic_v3_dist_GICD_CTLR_secure_ARE_NS_insert(ctrl, 1);
    // Enable group 1 interrupts
    ctrl = gic_v3_dist_GICD_CTLR_secure_EnableGrp1NS_insert(ctrl, 1);
    gic_v3_dist_GICD_CTLR_secure_wr(&gic_v3_dev, ctrl);

    check_cpu_if_statusr();

    printk(LOG_NOTE, "gic_v2: GICD IIDR "
            "implementer=0x%x, revision=0x%x, variant=0x%x, prodid=0x%x, raw=0x%x\n",
            gic_v3_dist_GICD_IIDR_Implementer_rdf(&gic_v3_dev),
            gic_v3_dist_GICD_IIDR_Revision_rdf(&gic_v3_dev),
            gic_v3_dist_GICD_IIDR_Variant_rdf(&gic_v3_dev),
            gic_v3_dist_GICD_IIDR_ProductID_rdf(&gic_v3_dev),
            gic_v3_dist_GICD_IIDR_rawrd(&gic_v3_dev)
            );


    uint32_t itlines = gic_v3_dist_GICD_TYPER_ITLinesNumber_rdf(&gic_v3_dev);
    itlines = (itlines+1)*32;
    if(itlines > 1020)
        itlines = 1020;
    printk(LOG_NOTE, "gic_v2: #INTIDs supported: %" PRIu32 "\n", itlines);

    uint32_t lspi = gic_v3_dist_GICD_TYPER_LSPI_rdf(&gic_v3_dev);
    printk(LOG_NOTE, "gic_v2: #LSPIs supported: %" PRIu32 "\n", lspi);


    // Setup distributor so it forwards all interrupts to this CPU.
    uint8_t my_cpumask = gic_get_cpumask();
    uint32_t dest_cpumask = my_cpumask;
    dest_cpumask = dest_cpumask | dest_cpumask << 8 | dest_cpumask << 16 | dest_cpumask << 24;
    for(int i=8; i*4 < itlines; i++)
        gic_v3_dist_GICD_ITARGETSR_wr(&gic_v3_dev, i, dest_cpumask);

    // Put all interrupts into Group 0 and enable them
    #define MASK_32     0xffffffff
    for (int i = 0; i * 32 < itlines; i++) {
        // Clear
        gic_v3_dist_GICD_ICACTIVER_wr(&gic_v3_dev, i, MASK_32);
        // Enable
        gic_v3_dist_GICD_ISENABLER_wr(&gic_v3_dev, i, MASK_32);
        // And put in group 0
        gic_v3_dist_GICD_IGROUPR_rawwr(&gic_v3_dev, i, 0);
    }

    // Disable interrupt FIQ Bypass interrupt 28
    gic_v3_dist_GICD_ICENABLER_wr(&gic_v3_dev, 0, (1<<28));


    gic_v3_dist_GICD_CTLR_rawwr(&gic_v3_dev, 0x1); // Enable Distributor
    check_cpu_if_statusr();
}

errval_t platform_init_ic_bsp(void) {
    gic_init();
    gic_cpu_interface_enable();
    return SYS_ERR_OK;
}

errval_t platform_init_ic_app(void) {
    gic_cpu_interface_enable();
    return SYS_ERR_OK;
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
    return SYS_ERR_OK;
}
