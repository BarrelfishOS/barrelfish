/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <kernel.h>

#include <dev/pl130_gic_dev.h>
#include <arm_hal.h>

extern pl130_gic_t gic;
extern uint32_t it_num_lines;

void  __attribute__((noreturn)) gic_disable_all_irqs(void)
{
    panic("gic_disable_all_irqs NYI for armv7");
    // XXX Rewrite according to pl130 interface changes!
    // ALSO remove noreturn option

    /* //disable PPI interrupts */
    /* pl130_gic_PPI_ICDICER_wr(&gic, (uint16_t)0xffff); */

    /* //disable SPI interrupts */
    /* for(uint8_t i=0; i < it_num_lines; i++) { */
    /*     pl130_gic_SPI_ICDICER_wr(&gic, i, (uint32_t)0xffffffff); */
    /* } */
}


// Helpers for enabling interrupts
#define GIC_IRQ_PRIO_LOWEST       (0xF)
#define GIC_IRQ_CPU_TRG_ALL       (0x3) // For two cores on the PandaBoard
#define GIC_IRQ_CPU_TRG_BSP       (0x1)
#define GIC_IRQ_EDGE_TRIGGERED    (0x1)
#define GIC_IRQ_LEVEL_SENSITIVE   (0x0)
#define GIC_IRQ_1_TO_N            (0x1)
#define GIC_IRQ_N_TO_N            (0x0)

/*
 * \brief Enable an interrupt
 *
 * See ARM Generic Interrupt Controller Architecture Specification v1.0
 *
 * \param prio Priority of the interrupt (lower is higher). We allow 0..15.
 *    The number of priority bits is implementation specific, but at least 16
 *    (using bits [7:4] of the priority field, chapter 3.3)
 * \param cpu_targets 8 Bit mask. One bit for each core in the system. 
 *    (chapter 4.3.11)
 * \param 0 is level-sensitive, 1 is edge-triggered
 * \param 0 is N-to-N, 1 is 1-N
 */
void gic_enable_interrupt(uint32_t int_id, uint8_t cpu_targets, uint16_t prio,
                          uint8_t edge_triggered, uint8_t one_to_n)
{
    // Set Interrupt Set-Enable Register
    uint32_t ind = int_id / 32;
    uint32_t bit_mask = (1U << (int_id % 32));
    uint32_t regval;

    // Interrupt type
    // XXX: shouldn't this be somewhere else? -SG
    enum {
        SGI,
        SPI,
        PPI
    } irq_type;

    /*
     * There are three types of interrupts
     * 1) Software generated Interrupts (SGI) - IDs 0-15
     * 2) Private Peripheral Interrupts (PPI) - IDs 16-31
     * 3) Shared Peripheral Interrups (SPI) - IDs 32...
     */
    if (int_id<16) {
        irq_type = SGI;
    } else if (int_id<32) {
        irq_type = PPI;
    } else {
        irq_type = SPI;
    }

    printf("gic_enable_interrupt for id=0x%"PRIx32", "
           "offset=0x%"PRIx32", index=0x%"PRIx32"\n",
           int_id, bit_mask, ind);

    // Set the Interrupt Set Enable register to enable the interupt
    // See ARM GIC TRM
    if (irq_type==SGI)
        return; // Do nothing for SGI interrupts
    // XXX: check what we need to do if int_id > it_num_lines
    //  -SG, 2012/12/13

    // Enable
    // 1 Bit per interrupt
    regval = pl130_gic_ICDISER_rd(&gic, ind);
    regval |= bit_mask;
    pl130_gic_ICDISER_wr(&gic, ind, regval);

    // TODO: cleanup pl130 mackerel file so that we don't need bit magic
    // here.  -SG, 2012/12/13

    // Priority
    // 8 Bit per interrupt
    // chp 4.3.10
    ind = int_id/4;
    // XXX: check that priorities work properly, -SG, 2012/12/13
    prio = (prio & 0xF)<<4;
    switch(int_id % 4) {
    case 0:
        pl130_gic_ICDIPR_prio_off0_wrf(&gic, ind, prio);
        break;
    case 1:
        pl130_gic_ICDIPR_prio_off1_wrf(&gic, ind, prio);
        break;
    case 2:
        pl130_gic_ICDIPR_prio_off2_wrf(&gic, ind, prio);
        break;
    case 3:
        pl130_gic_ICDIPR_prio_off3_wrf(&gic, ind, prio);
        break;
    }

    // Target processors (only SPIs)
    // 8 Bit per interrupt
    ind = int_id/4;
    if (irq_type==SPI) { // rest is ro
        switch (int_id % 4) {
        case 0:
            pl130_gic_ICDIPTR_targets_off0_wrf(&gic, ind, cpu_targets);
            break;
        case 1:
            pl130_gic_ICDIPTR_targets_off1_wrf(&gic, ind, cpu_targets);
            break;
        case 2:
            pl130_gic_ICDIPTR_targets_off2_wrf(&gic, ind, cpu_targets);
            break;
        case 3:
            pl130_gic_ICDIPTR_targets_off3_wrf(&gic, ind, cpu_targets);
            break;
        }
    }

    // Configuration registers
    // 2 Bit per IRQ
    ind = int_id/16;
    uint8_t val = ((edge_triggered&0x1) << 1) | (one_to_n&0x1);
    switch (int_id % 16) {
    case 0:
        pl130_gic_ICDICR_conf0_wrf(&gic, ind, val);
        break;
    case 1:
        pl130_gic_ICDICR_conf1_wrf(&gic, ind, val);
        break;
    case 2:
        pl130_gic_ICDICR_conf2_wrf(&gic, ind, val);
        break;
    case 3:
        pl130_gic_ICDICR_conf3_wrf(&gic, ind, val);
        break;
    case 4:
        pl130_gic_ICDICR_conf4_wrf(&gic, ind, val);
        break;
    case 5:
        pl130_gic_ICDICR_conf5_wrf(&gic, ind, val);
        break;
    case 6:
        pl130_gic_ICDICR_conf6_wrf(&gic, ind, val);
        break;
    case 7:
        pl130_gic_ICDICR_conf7_wrf(&gic, ind, val);
        break;
    case 8:
        pl130_gic_ICDICR_conf8_wrf(&gic, ind, val);
        break;
    case 9:
        pl130_gic_ICDICR_conf9_wrf(&gic, ind, val);
        break;
    case 10:
        pl130_gic_ICDICR_conf10_wrf(&gic, ind, val);
        break;
    case 11:
        pl130_gic_ICDICR_conf11_wrf(&gic, ind, val);
        break;
    case 12:
        pl130_gic_ICDICR_conf12_wrf(&gic, ind, val);
        break;
    case 13:
        pl130_gic_ICDICR_conf13_wrf(&gic, ind, val);
        break;
    case 14:
        pl130_gic_ICDICR_conf14_wrf(&gic, ind, val);
        break;
    case 15:
        pl130_gic_ICDICR_conf15_wrf(&gic, ind, val);
        break;
    }
}

