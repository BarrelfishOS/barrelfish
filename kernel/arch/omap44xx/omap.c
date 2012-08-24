/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <paging_kernel_arch.h>

#include <dev/pl130_gic_dev.h>
#include <dev/sp804_pit_dev.h>
#include <dev/arm_icp_pit_dev.h>
#include <dev/cortex_a9_pit_dev.h>
#include <dev/a9scu_dev.h>

#include <arm_hal.h>
#include <cp15.h>

//hardcoded bc gem5 doesn't set board id in ID_Register
#define VEXPRESS_ELT_BOARD_ID		0x8e0
uint32_t hal_get_board_id(void)
{
    return VEXPRESS_ELT_BOARD_ID;
}

uint8_t hal_get_cpu_id(void)
{
    return cp15_get_cpu_id();
}

//Gem5 ensures that cpu0 is always BSP, this probably also holds in general
bool hal_cpu_is_bsp(void)
{
    return cp15_get_cpu_id() == 0;
}

// clock rate hardcoded to 2GHz
static uint32_t tsc_hz = 2000000000;

//
// Interrupt controller
// Offsets taken from ARM Cortex A9 MPCore TRM Table 1-3
//
#define DIST_OFFSET     0x1000 // Interrupt Distributor
#define CPU_OFFSET 	0x0100 // Interrupt controller interface

static pl130_gic_t pic;
static pl130_gic_ICDICTR_t pic_config;

static uint32_t it_num_lines;
static uint8_t cpu_number;
static uint8_t sec_extn_implemented;

/*
 * This is the private memory region, which starts at address PERIPHBASE.
 */
lvaddr_t private_memory_region = 0;

void private_mem_test(void);
void private_mem_test(void)
{
    // Make sure both pages are mapped
    uint32_t *p1 = (uint32_t*) private_memory_region;
    uint32_t *p2 = (uint32_t*) (private_memory_region+DIST_OFFSET+0x100);
    uint32_t test1 = *p1;
    uint32_t test2 = *p2;
    printf("test-reading private memory regions "
           "at %p=%"PRIx32" and "
           "at %p=%"PRIx32"\n",
           p1, test1,
           p2, test2);
}

/*
 * Map the private memory region described in ARM Cortex A9 TRM - Table 1-3
 */
void map_private_memory_region(void);
void map_private_memory_region(void)
{
    lpaddr_t periphbase = cp15_read_cbar();
    printf("map_private_memory_region: PERIPHBASE=%x\n", periphbase);

    // According to ARM Cortex A9 MPCore TRM, this is two contiguous 4KB pages
    // We map a section (1MB) anyway
    private_memory_region = paging_map_device(periphbase, ARM_L1_SECTION_BYTES);

    // paging_map_device returns an address pointing to the beginning of
    // a section, need to add the offset for within the section again
    private_memory_region += (periphbase & ARM_L1_SECTION_MASK);

    printf("private memory region (phy) %x, private memory region (virt) %x\n",
           periphbase, private_memory_region);

    private_mem_test();
}

/*
 * There are three types of interrupts
 * 1) Software generated Interrupts (SGI) - IDs 0-15
 * 2) Private Peripheral Interrupts (PPI) - IDs 16-31
 * 3) Shared Peripheral Interrups (SPI) - IDs 32...
 */
void pic_init(void)
{
    map_private_memory_region();

    pl130_gic_initialize(&pic,
                         (mackerel_addr_t) private_memory_region + DIST_OFFSET,
                         (mackerel_addr_t) private_memory_region + CPU_OFFSET);

    // read GIC configuration
    pic_config = pl130_gic_ICDICTR_rd(&pic);

    // ARM GIC TRM, 3.1.2
    // This is the number of ICDISERs, i.e. #SPIs
    // Number of SIGs (0-15) and PPIs (16-31) is fixed
    // Why (x+1)*32
    uint32_t it_num_lines_tmp = pl130_gic_ICDICTR_it_lines_num_extract(pic_config);
    it_num_lines = 32*(it_num_lines_tmp + 1);

    printf("GIC: %d interrupt lines detected\n", it_num_lines_tmp);

    cpu_number = pl130_gic_ICDICTR_cpu_number_extract(pic_config);
    sec_extn_implemented = pl130_gic_ICDICTR_TZ_extract(pic_config);

    // set priority mask of cpu interface, currently set to lowest priority
    // to accept all interrupts
    pl130_gic_ICCPMR_wr(&pic, 0xff);

    // set binary point to define split of group- and subpriority
    // currently we allow for 8 subpriorities
    pl130_gic_ICCBPR_wr(&pic, 0x2);

    // enable interrupt forwarding to processor
    pl130_gic_ICCICR_enable_wrf(&pic, 0x1);

    // Distributor:
    // enable interrupt forwarding from distributor to cpu interface
    pl130_gic_ICDDCR_enable_wrf(&pic, 0x1);
    printf("pic_init: done\n");
}

void pic_disable_all_irqs(void)
{
    // Rerite according to pl130 interface changes!

    /* //disable PPI interrupts */
    /* pl130_gic_PPI_ICDICER_wr(&pic, (uint16_t)0xffff); */

    /* //disable SPI interrupts */
    /* for(uint8_t i=0; i < it_num_lines; i++) { */
    /*     pl130_gic_SPI_ICDICER_wr(&pic, i, (uint32_t)0xffffffff); */
    /* } */
}


/*
 * Enable an interrupt
 *
 * \param prio Priority of the interrupt (lower is higher)
 */
void pic_enable_interrupt(uint32_t int_id, uint8_t cpu_targets, uint16_t prio,
                          uint8_t edge_triggered, uint8_t one_to_n)
{
    // Set Interrupt Set-Enable Register
    uint32_t ind = int_id / 32;
    uint32_t bit_mask = (1U << (int_id % 32));
    uint32_t regval;

    printf("pic_enable_interrupt for id=%"PRIx32", "
           "offset=%"PRIx32", index=%"PRIx32"\n",
           int_id, bit_mask, ind);

    // Set the Interrupt Set Enable register to enable the interupt
    // See ARM GIC TRM
    if (int_id < 16)
        return; // Do nothing for SGI interrupts
    else if (int_id >= it_num_lines) // XXX Verify this
        panic("Interrupt ID %"PRIu32" not supported", int_id);

    // Set the corresponding interrupt bit
    regval = pl130_gic_ICDISER_rd(&pic, ind);
    regval |= bit_mask;
    pl130_gic_ICDISER_wr(&pic, ind, regval);

    // Set Interrupt Priority Register
    // We have four of them in the same fields (hence: /4)
    ind = int_id/4;

    switch(int_id % 4) {
    case 0:
        pl130_gic_ICDIPR_prio_off0_wrf(&pic, ind, prio);
        break;
    case 1:
        pl130_gic_ICDIPR_prio_off1_wrf(&pic, ind, prio);
        break;
    case 2:
        pl130_gic_ICDIPR_prio_off2_wrf(&pic, ind, prio);
        break;
    case 3:
        pl130_gic_ICDIPR_prio_off3_wrf(&pic, ind, prio);
        break;
    }

    if (int_id >= 32) {

        // Need to update the following code to the new Mackerel interface
        panic("NYI");
    }

    return;

    /* // Set the target core for the interrupt */
    /* // only SPIs can be targeted manually */
    /* ind = int_id/4; */
    /* if(int_id >= 32) */
    /*     { */
    /*         panic("Not yet tested"); */
    /*         switch(int_id % 4) */
    /*     	{ */
    /*             case 0: */
    /*                 pl130_gic_SPI_ICDIPTR_targets_off0_wrf(&pic, ind-8, cpu_targets); */
    /*                 break; */
    /*             case 1: */
    /*                 pl130_gic_SPI_ICDIPTR_targets_off1_wrf(&pic, ind-8, cpu_targets); */
    /*                 break; */
    /*             case 2: */
    /*                 pl130_gic_SPI_ICDIPTR_targets_off2_wrf(&pic, ind-8, cpu_targets); */
    /*                 break; */
    /*             case 3: */
    /*                 pl130_gic_SPI_ICDIPTR_targets_off3_wrf(&pic, ind-8, cpu_targets); */
    /*                 break; */
    /*     	} */
    /*     } */



    /* // Set Interrupt Configuration Register */
    /* if(int_id >= 32) */
    /*     { */
    /*         panic("Not yet tested"); */
    /*         ind = int_id/16; */
    /*         uint8_t val = (edge_triggered << 1) | one_to_n; */
    /*         switch(int_id % 16) */
    /*     	{ */
    /*             case 0: */
    /*                 pl130_gic_SPI_ICDICR_spi0_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 1: */
    /*                 pl130_gic_SPI_ICDICR_spi1_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 2: */
    /*                 pl130_gic_SPI_ICDICR_spi2_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 3: */
    /*                 pl130_gic_SPI_ICDICR_spi3_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 4: */
    /*                 pl130_gic_SPI_ICDICR_spi4_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 5: */
    /*                 pl130_gic_SPI_ICDICR_spi5_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 6: */
    /*                 pl130_gic_SPI_ICDICR_spi6_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 7: */
    /*                 pl130_gic_SPI_ICDICR_spi7_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 8: */
    /*                 pl130_gic_SPI_ICDICR_spi8_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 9: */
    /*                 pl130_gic_SPI_ICDICR_spi9_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 10: */
    /*                 pl130_gic_SPI_ICDICR_spi10_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 11: */
    /*                 pl130_gic_SPI_ICDICR_spi11_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 12: */
    /*                 pl130_gic_SPI_ICDICR_spi12_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 13: */
    /*                 pl130_gic_SPI_ICDICR_spi13_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 14: */
    /*                 pl130_gic_SPI_ICDICR_spi14_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*             case 15: */
    /*                 pl130_gic_SPI_ICDICR_spi15_wrf(&pic, ind-2, val); */
    /*                 break; */
    /*     	} */
    /*     } */
}

uint32_t pic_get_active_irq(void)
{
	uint32_t regval = pl130_gic_ICCIAR_rd(&pic);

	return regval;
}

void pic_raise_softirq(uint8_t cpumask, uint8_t irq)
{
	uint32_t regval = (cpumask << 16) | irq;
	pl130_gic_ICDSGIR_wr(&pic, regval);
}

/*
uint32_t pic_get_active_irq(void)
{
    uint32_t status = arm_icp_pic0_PIC_IRQ_STATUS_rd_raw(&pic);
    uint32_t irq;

    for (irq = 0; irq < 32; irq++) {
        if (0 != (status & (1u << irq))) {
            return irq;
        }
    }
    return ~0ul;
}
*/

void pic_ack_irq(uint32_t irq)
{
    pl130_gic_ICCEOIR_rawwr(&pic, irq);
}

//
// Kernel timer and tsc
//

// Global Timer, ARM Cortex A9 MPCore TRM Table 1-3
#define PIT_OFFSET      0x0200

// XXX check if we need these
#define PIT_BASE 	0xE0000000
#define PIT0_OFFSET	0x11000
#define PIT_DIFF	0x1000

#define LOCAL_TIMER_IRQ 29
#define PIT0_IRQ	36
#define PIT1_IRQ	37

#define PIT0_ID		0
#define PIT1_ID		1

static sp804_pit_t pit0;
static sp804_pit_t pit1;

void __attribute__((noreturn)) pit_init(uint32_t timeslice, uint8_t pit_id)
{
    // Private memory was already activated by pic_init
    assert(private_memory_region!=0);
    panic("Don't use (or have) sp804 on PandaBoard");

    sp804_pit_t *pit;
        if(pit_id == PIT0_ID)
        	pit = &pit0;
        else if(pit_id == PIT1_ID)
        	pit = &pit1;
        else
        	panic("Unsupported PIT ID: %"PRIu32, pit_id);

    printf("pit_init: step 1\n");

    sp804_pit_initialize(pit, (mackerel_addr_t)(private_memory_region + PIT_OFFSET));

	// PIT timer
    uint32_t load1 = timeslice * tsc_hz / 1000;
    uint32_t load2 = timeslice * tsc_hz / 1000;

    sp804_pit_Timer1Load_wr(pit, load1);
    sp804_pit_Timer2Load_wr(pit, load2);

    //configure timer 1
    sp804_pit_Timer1Control_one_shot_wrf(pit, 0);
    sp804_pit_Timer1Control_timer_size_wrf(pit, sp804_pit_size_32bit);
    sp804_pit_Timer1Control_timer_pre_wrf(pit, sp804_pit_prescale0);
    sp804_pit_Timer1Control_int_enable_wrf(pit, 0);
    sp804_pit_Timer1Control_timer_mode_wrf(pit, sp804_pit_periodic);
    sp804_pit_Timer1Control_timer_en_wrf(pit, 0);

    //configure timer 2
    sp804_pit_Timer2Control_one_shot_wrf(pit, 0);
    sp804_pit_Timer2Control_timer_size_wrf(pit, sp804_pit_size_32bit);
    sp804_pit_Timer2Control_timer_pre_wrf(pit, sp804_pit_prescale0);
    sp804_pit_Timer2Control_int_enable_wrf(pit, 0);
    sp804_pit_Timer2Control_timer_mode_wrf(pit, sp804_pit_periodic);
    sp804_pit_Timer2Control_timer_en_wrf(pit, 0);

    //enable PIT interrupt
    uint32_t int_id = pit_id ? PIT1_IRQ : PIT0_IRQ;
    pic_enable_interrupt(int_id, 0x1, 0xf, 0x1, 0);

    //pic_set_irq_enabled(PIT_IRQ, 1);
}

void pit_start(uint8_t pit_id)
{
	 sp804_pit_t *pit;
	 if(pit_id == PIT0_ID)
		 pit = &pit0;
	 else if(pit_id == PIT1_ID)
		 pit = &pit1;
	 else
		 panic("Unsupported PIT ID: %"PRIu32, pit_id);


         // No PIT for now ..
	/* sp804_pit_Timer1Control_int_enable_wrf(pit, 1); */
	/* sp804_pit_Timer1Control_timer_en_wrf(pit, 1); */
}

static uint32_t local_timer_counter = 0;
bool pit_handle_irq(uint32_t irq)
{
//    printf("pit_handle_irq %d\n", irq);

    switch(irq) {

    case PIT0_IRQ:
        sp804_pit_Timer1IntClr_wr(&pit0, ~0ul);
        pic_ack_irq(irq);
        return 1;

    case PIT1_IRQ:
        sp804_pit_Timer1IntClr_wr(&pit1, ~0ul);
        pic_ack_irq(irq);
        return 1;

    case LOCAL_TIMER_IRQ:
//        printf("local timer IRQ, number=%d\n", local_timer_counter);
        pic_ack_irq(irq);
        local_timer_counter++;
        return 1;

    default:
        return 0;
    }
}

void pit_mask_irq(bool masked, uint8_t pit_id)
{
	 sp804_pit_t *pit;
	 if(pit_id == PIT0_ID)
		 pit = &pit0;
	 else if(pit_id == PIT1_ID)
		 pit = &pit1;
	 else
		 panic("Unsupported PIT ID: %"PRIu32, pit_id);

    if (masked) {
        sp804_pit_Timer1Control_int_enable_wrf(pit, 0);
    }
    else {
        sp804_pit_Timer1Control_int_enable_wrf(pit, 1);
    }

    if (masked) {
        // Clear interrupt if pending.
        pit_handle_irq(pit_id ? PIT1_IRQ : PIT0_IRQ);
    }
}

//
// TSC uses cpu private timer
//

#define TSC_OFFSET	0x600

static cortex_a9_pit_t tsc;

void tsc_init(void)
{
    cortex_a9_pit_initialize(&tsc, (mackerel_addr_t) private_memory_region + TSC_OFFSET);

    // write load
    uint32_t load = (100000000); // in cycles [ should be around 10 per second ]
    cortex_a9_pit_TimerLoad_wr(&tsc, load);

    //configure tsc
    cortex_a9_pit_TimerControl_prescale_wrf(&tsc, 0);
    cortex_a9_pit_TimerControl_int_enable_wrf(&tsc, 1);
    // XXX Disable interrupts, to ease debugging init startup
    //cortex_a9_pit_TimerControl_int_enable_wrf(&tsc, 0);
    cortex_a9_pit_TimerControl_auto_reload_wrf(&tsc, 1);
    cortex_a9_pit_TimerControl_timer_enable_wrf(&tsc, 1);

    pic_enable_interrupt(29, 0, 0, 0, 0);
    printf("pic_enable_interrupt done\n");
}

uint32_t tsc_read(void)
{
    // Timers count down so invert it.
    return ~cortex_a9_pit_TimerCounter_rd(&tsc);
}

uint32_t tsc_get_hz(void)
{
    return tsc_hz;
}


//
// Snoop Control Unit
//

static a9scu_t scu;

void scu_initialize(void)
{
    // SCU is at PERIPHBASE + 0
    a9scu_initialize(&scu, (mackerel_addr_t) private_memory_region);
}

void scu_enable(void)
{
    //enable SCU
    a9scu_SCUControl_t ctrl_reg = a9scu_SCUControl_rd(&scu);
    ctrl_reg |= 0x1;
    a9scu_SCUControl_wr(&scu, ctrl_reg);

    //(should invalidate d-cache here)
}

int scu_get_core_count(void)
{
    // b00 for one core
    // b01 for two cores
    // b10 for tree cores
    // b11 for four cores
    return a9scu_SCUConfig_cpu_number_rdf(&scu) + 1;
}
