/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <kernel.h>

#include <dev/pl390_gic_cpuif_dev.h>
#include <dev/pl390_gic_dist_dev.h>
#include <arch/arm/gic.h>
#include <arch/arm/platform.h>
#include <paging_kernel_arch.h>
#include <irq.h>
#include <getopt/getopt.h>

static pl390_gic_cpuif_t gic;
static pl390_gic_dist_t gic_dist;

#define MSG(format, ...) printk( LOG_NOTE, "GIC: "format, ## __VA_ARGS__ )

extern lpaddr_t platform_gic_cpu_interface_base;
extern lpaddr_t platform_gic_distributor_base;

// Command line arguments
static struct cmdarg cmdargs[] = {
    {"gic", ArgType_UInt, { .uinteger = &platform_gic_cpu_interface_base }},
    {"gicdist", ArgType_UInt, { .uinteger = &platform_gic_distributor_base }}
};

enum IrqType {
    IrqType_SGI,
    IrqType_PPI,
    IrqType_SPI
};

#define CPU_SIZE  0x1000
#define DIST_SIZE  0x1000

/**
 * \brief Returns the IRQ type based on the interrupt ID
 *
 * We have three types of interrupts
 * 1) Software generated Interrupts (SGI): IDs 0-15
 * 2) Private Peripheral Interrupts (PPI): IDs 16-31
 * 3) Shared Peripheral Interrups (SPI): IDs 32-
 *
 * \return The type of the interrupt.
 */
static enum IrqType get_irq_type(uint32_t int_id)
{
    if (int_id < 16) {
        return IrqType_SGI;
    } else if (int_id < 32) {
        return IrqType_PPI;
    } else {
        return IrqType_SPI;
    }
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
    parse_commandline(kernel_command_line, cmdargs);
    printk(LOG_NOTE, "GICv1: Initializing. cpu_if=%p, dist=%p\n",
            platform_gic_cpu_interface_base, platform_gic_distributor_base);
    lvaddr_t gic_cpu_base =
        paging_map_device(platform_gic_cpu_interface_base, CPU_SIZE );
    pl390_gic_cpuif_initialize(&gic, (mackerel_addr_t)gic_cpu_base );

    lvaddr_t gic_dist_base =
        paging_map_device(platform_gic_distributor_base, DIST_SIZE );
    pl390_gic_dist_initialize(&gic_dist, (mackerel_addr_t)gic_dist_base );

    // set priority mask of cpu interface, currently set to lowest priority
    // to accept all interrupts
    pl390_gic_cpuif_ICCPMR_wr(&gic, 0xff);

    // set binary point to define split of group- and subpriority
    // currently we allow for 8 subpriorities
    pl390_gic_cpuif_ICCBPR_wr(&gic, 0x2);

    // enable interrupt forwarding to processor
    pl390_gic_cpuif_ICCICR_enable_wrf(&gic, 0x1);

    MSG("gic_init done\n");
}

void  __attribute__((noreturn)) gic_disable_all_irqs(void)
{
    panic("gic_disable_all_irqs NYI for armv7");
    // XXX Rewrite according to pl390 interface changes!
    // ALSO remove noreturn option

    /* //disable PPI interrupts */
    /* pl390_gic_cpuif_PPI_ICDICER_wr(&gic, (uint16_t)0xffff); */

    /* //disable SPI interrupts */
    /* for(uint8_t i=0; i < it_num_lines; i++) { */
    /*     pl390_gic_cpuif_SPI_ICDICER_wr(&gic, i, (uint32_t)0xffffffff); */
    /* } */
}

uint32_t platform_get_active_irq(void)
{
    return pl390_gic_cpuif_ICCIAR_rd(&gic);
}

uint32_t gic_cpu_count(void){
    return 1;
    /*
     * LH: The CPU count can be determined using the gic distributor, but
     * it is called before the distributor is initialized. However,
     * with the new coreboot it seems to be unnecessary anyway.
     *
     * return pl390_gic_dist_ICDICTR_cpu_number_rdf(&gic_dist);
     */
}

void gic_raise_softirq(uint8_t cpumask, uint8_t irq)
{
    assert(!"NYI"); // This needs distributor access
}

void platform_acknowledge_irq(uint32_t irq)
{
    pl390_gic_cpuif_ICCEOIR_rawwr(&gic, irq);
}

//enable interrupt forwarding to processor
void gic_cpu_interface_enable(void)
{
    pl390_gic_cpuif_ICCICR_wr(&gic, 0x1);
}

errval_t platform_init_ic_bsp(void) {
    gic_init();
    gic_cpu_interface_enable();
    return SYS_ERR_OK;
}

errval_t platform_init_ic_app(void) {
    gic_init();
    gic_cpu_interface_enable();
    return SYS_ERR_OK;
}

/**
 * \brief Enable an PPI interrupt
 *
 * \see ARM Generic Interrupt Controller Architecture Specification v1.0
 *
 * \param int_id
 * \param prio Priority of the interrupt (lower is higher). We allow 0..15.
 *    The number of priority bits is implementation specific, but at least 16
 *    (using bits [7:4] of the priority field, chapter 3.3)
 * \param 0 is level-sensitive, 1 is edge-triggered
 * \param 0 is N-to-N, 1 is 1-N
 */
errval_t platform_enable_interrupt(uint32_t int_id, uint16_t prio,
                               bool edge_triggered, bool one_to_n)
{
    // Set Interrupt Set-Enable Register
    uint32_t ind = int_id / 32;
    uint32_t bit_mask = (1U << (int_id % 32));

    MSG("platform_enable_interrupt for id=0x%"PRIx32", "
           "bit_mask=0x%"PRIx32", index=0x%"PRIx32"\n",
           int_id, bit_mask, ind);

    enum IrqType irq_type = get_irq_type(int_id);
    // We only allow enableing PPI interrupts. 
    if(irq_type != IrqType_PPI) {
        return SYS_ERR_IRQ_INVALID;
    }

    // Enable
    // 1 Bit per interrupt
    uint32_t regval = pl390_gic_dist_ICDISER_rd(&gic_dist, ind);
    regval |= bit_mask;
    pl390_gic_dist_ICDISER_wr(&gic_dist, ind, regval);

    // TODO: cleanup pl390 mackerel file so that we don't need bit magic
    // here.  -SG, 2012/12/13

    // Priority
    // 8 Bit per interrupt
    // chp 4.3.10
    ind = int_id/4;
    // XXX: check that priorities work properly, -SG, 2012/12/13
    prio = (prio & 0xF)<<4;
    switch(int_id % 4) {
    case 0:
        pl390_gic_dist_ICDIPR_prio_off0_wrf(&gic_dist, ind, prio);
        break;
    case 1:
        pl390_gic_dist_ICDIPR_prio_off1_wrf(&gic_dist, ind, prio);
        break;
    case 2:
        pl390_gic_dist_ICDIPR_prio_off2_wrf(&gic_dist, ind, prio);
        break;
    case 3:
        pl390_gic_dist_ICDIPR_prio_off3_wrf(&gic_dist, ind, prio);
        break;
    }

    // Configuration registers
    // 2 Bit per IRQ
    ind = int_id/16;
    uint8_t val = ((edge_triggered&0x1) << 1) | (one_to_n&0x1);
    switch (int_id % 16) {
    case 0:
        pl390_gic_dist_ICDICR_conf0_wrf(&gic_dist, ind, val);
        break;
    case 1:
        pl390_gic_dist_ICDICR_conf1_wrf(&gic_dist, ind, val);
        break;
    case 2:
        pl390_gic_dist_ICDICR_conf2_wrf(&gic_dist, ind, val);
        break;
    case 3:
        pl390_gic_dist_ICDICR_conf3_wrf(&gic_dist, ind, val);
        break;
    case 4:
        pl390_gic_dist_ICDICR_conf4_wrf(&gic_dist, ind, val);
        break;
    case 5:
        pl390_gic_dist_ICDICR_conf5_wrf(&gic_dist, ind, val);
        break;
    case 6:
        pl390_gic_dist_ICDICR_conf6_wrf(&gic_dist, ind, val);
        break;
    case 7:
        pl390_gic_dist_ICDICR_conf7_wrf(&gic_dist, ind, val);
        break;
    case 8:
        pl390_gic_dist_ICDICR_conf8_wrf(&gic_dist, ind, val);
        break;
    case 9:
        pl390_gic_dist_ICDICR_conf9_wrf(&gic_dist, ind, val);
        break;
    case 10:
        pl390_gic_dist_ICDICR_conf10_wrf(&gic_dist, ind, val);
        break;
    case 11:
        pl390_gic_dist_ICDICR_conf11_wrf(&gic_dist, ind, val);
        break;
    case 12:
        pl390_gic_dist_ICDICR_conf12_wrf(&gic_dist, ind, val);
        break;
    case 13:
        pl390_gic_dist_ICDICR_conf13_wrf(&gic_dist, ind, val);
        break;
    case 14:
        pl390_gic_dist_ICDICR_conf14_wrf(&gic_dist, ind, val);
        break;
    case 15:
        pl390_gic_dist_ICDICR_conf15_wrf(&gic_dist, ind, val);
        break;
    }
    return SYS_ERR_OK;
}
