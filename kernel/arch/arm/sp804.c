/**
 * \file
 * \brief SP804 ARM dual timer module driver for the arm_hal 'pit'
 * interface. 
 */
/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <dev/sp804_pit_dev.h>
#include <sp804.h>
#include <gic.h>
#include <arm_hal.h>
#include <paging_kernel_arch.h>


/*
 * Size of the SP804 device in memory 
 */
#define PIT_SIZE 0x1000

/*
 * The timers themselves
 */
#define MAX_TIMERS 4
struct pit { 
    lpaddr_t	paddr;
    uint32_t	irq;
    sp804_pit_t pit;
};
static struct pit pits[MAX_TIMERS/2];
static size_t num_timers = 0;
static uint32_t freq = 0;

/** 
 * \brief Configure the timer chips.
 * 
 * We expose the timers individually, and simply trust that the rest
 * of the CPU driver knows that each pair of timers shares a single
 * interrupt line.   We also require that the MMU is turned on at this
 * point. 
 * 
 * \param n the number of timers
 * \param hz the clock frequency of the timers in Hertz
 * \param addrs an array of n physical addresses of sp804 devices
 * \param irqs an array of n irq numbers for the devices
 */
void sp804_configure(size_t n, uint32_t hz, const lpaddr_t addrs[], const uint32_t irqs[] )
{
    // assert( paging_mmu_enabled() );
    assert( n*2 <= MAX_TIMERS );
    num_timers = n;
    freq = hz;
    for( int i=0; i < n; i++ ) {
	lvaddr_t pit_vaddr;
	struct pit *p = &pits[i];
	p->paddr = addrs[i];
	p->irq  = irqs[i];
	pit_vaddr = paging_map_device( pits[i].paddr, PIT_SIZE );
	sp804_pit_initialize(&p->pit, (mackerel_addr_t)pit_vaddr);
    }
}

/**
 * \brief Initialize a timer to run periodically
 *
 * \param timeslice the frequency of the desired tick in hertz.
 * \param timer the number of the timer to program.  
 *
 * Note that each sp804 has two timers, and so this will program the
 * timer <timer%2> of sp804 <timer/2>. 
 */
void pit_init(uint32_t timeslice, size_t timer)
{
    // We must have called sp804_configure by this point.  If we
    // haven't, num_timers will be 0 and this assertion will fail.
    assert(timer < num_timers);
    struct pit *p  = &pits[timer/2];
    sp804_pit_t *sp = &p->pit;
    // XXX Why this 1000 factor?
    uint32_t loadval = timeslice * freq / 1000;

    // Program the timer
    if ( (timer % 2) == 0 ) {
	sp804_pit_Timer1Load_wr(sp, loadval);
	sp804_pit_Timer1Control_one_shot_wrf(sp, 0);
	sp804_pit_Timer1Control_timer_size_wrf(sp, sp804_pit_size_32bit);
	sp804_pit_Timer1Control_timer_pre_wrf(sp, sp804_pit_prescale0);
	sp804_pit_Timer1Control_int_enable_wrf(sp, 0);
	sp804_pit_Timer1Control_timer_mode_wrf(sp, sp804_pit_periodic);
	sp804_pit_Timer1Control_timer_en_wrf(sp, 0);
    } else {
	sp804_pit_Timer2Load_wr(sp, loadval);
	sp804_pit_Timer2Control_one_shot_wrf(sp, 0);
	sp804_pit_Timer2Control_timer_size_wrf(sp, sp804_pit_size_32bit);
	sp804_pit_Timer2Control_timer_pre_wrf(sp, sp804_pit_prescale0);
	sp804_pit_Timer2Control_int_enable_wrf(sp, 0);
	sp804_pit_Timer2Control_timer_mode_wrf(sp, sp804_pit_periodic);
	sp804_pit_Timer2Control_timer_en_wrf(sp, 0);
    }
    
    // Enable PIT interrupt
    gic_enable_interrupt(p->irq, 0x1, 0xf, 0x1, 0);
}

/**
 * \brief Start a timer running after configuring it. 
 * 
 * \param timer a timer number. 
 *
 * This enables interrupts and starts the timer running.
 */
void pit_start(size_t timer)
{
    assert(timer < num_timers);
    sp804_pit_t *sp  = &pits[timer/2].pit;

    if ( (timer % 2) == 0 ) {
	sp804_pit_Timer1Control_int_enable_wrf(sp, 1);
	sp804_pit_Timer1Control_timer_en_wrf(sp, 1);
    } else {
	sp804_pit_Timer2Control_int_enable_wrf(sp, 1);
	sp804_pit_Timer2Control_timer_en_wrf(sp, 1);
    }
}

/** 
 * \brief Handle an interrupt
 * 
 * \param irq an IRQ number of the interrupt
 *
 * Currently, clears the interrupt condition on both timers on the
 * relevant SP804.
 */
bool pit_handle_irq(uint32_t irq)
{
    // XXX could be more efficient
    for( int i = 0; i < num_timers / 2; i++ ) { 
	if (pits[i].irq == irq) {
	    sp804_pit_Timer1IntClr_wr(&pits[i].pit, ~0ul);
	    sp804_pit_Timer2IntClr_wr(&pits[i].pit, ~0ul);
	    gic_ack_irq(irq);
	    return 1;
	}
    }
    return 0;
}

/** 
 * \brief Mask the interrupts from a given timer.
 * 
 * \param masked whether to set or clear the mask.
 * \param timer the timer whose interrupt to clear
 *
 * Currently, clears the interrupt condition on both timers on the
 * relevant SP804.
 */
void pit_mask_irq(bool masked, size_t timer)
{
    assert(timer < num_timers);
    struct pit *p  = &pits[timer/2];
    sp804_pit_t *sp = &p->pit;
    if (masked) {
        sp804_pit_Timer1Control_int_enable_wrf(sp, 0);
    }
    else {
        sp804_pit_Timer1Control_int_enable_wrf(sp, 1);
    }
    if (masked) {
        // Clear interrupt if pending.
        pit_handle_irq(p->irq);
    }
}
