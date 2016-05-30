/**
 * \file
 * \brief Platform code for ARMv7-A VersatileExpress EMM board
 */

/*
 * Copyright (c) 2009-2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <global.h>
#include <init.h>
#include <paging_kernel_arch.h>
#include <platform.h>
#include <serial.h>
#include <pl011.h>
#include <sp804.h>
#include <assert.h>
#include <errors/errno.h>
#include <vexpress_map.h>
#include <dev/cortex_a9_pit_dev.h>
#include <arm_hal.h>
#include <gic.h>

/********************************************************************************
 *
 * Implementation of serial.h
 *
 *******************************************************************************/

/*
 * Where they?
 */
#define NUM_UARTS 5
static const lpaddr_t uarts[] = { 
    VEXPRESS_MAP_UART0, 
    VEXPRESS_MAP_UART1, 
    VEXPRESS_MAP_UART2, 
    VEXPRESS_MAP_UART3, 
    VEXPRESS_MAP_UART4
};

unsigned serial_num_physical_ports = 0;
unsigned serial_console_port = 0;
unsigned serial_debug_port = 0;

/*
 * Initialize the serial ports
 */
errval_t serial_early_init(unsigned port)
{
    assert(!mmu_is_enabled());
    assert(port < NUM_UARTS);
    if (port >= serial_num_physical_ports) { 
	serial_num_physical_ports = port + 1;
    }
    pl011_configure(port, uarts[port]);
    return SYS_ERR_OK;
}

errval_t serial_init(unsigned port, bool initialize_hw)
{
    assert(mmu_is_enabled());
    assert(port < serial_num_physical_ports);
    pl011_init(port, initialize_hw);
    return SYS_ERR_OK;
};

void serial_putchar(unsigned port, char c)
{
    assert(port < serial_num_physical_ports);
    pl011_putchar(port, c);
}

char serial_getchar(unsigned port)
{
    assert(port < serial_num_physical_ports);
    return pl011_getchar(port);
}


/********************************************************************************
 *
 * Where's the GIC?
 *
 *******************************************************************************/
//
// Found in the class VExpress_EMM defn. in RealView.py in the GEM5
// sources...
//

lpaddr_t platform_get_distributor_address(void)
{
    assert(mmu_is_enabled());
    return ( VEXPRESS_MAP_GIC_DIST );
}

lpaddr_t platform_get_gic_cpu_address(void)
{
    assert(mmu_is_enabled());
    return ( VEXPRESS_MAP_GIC_CPU );
}

/********************************************************************************
 *
 * We don't seem to have a Snoop Control Unit.
 *
 *******************************************************************************/

/*
 * Initialize and enable the Snoop Control Unit
 */
void platform_init_scu(void) 
{
    assert(mmu_is_enabled());
}

/*
 * Return the core count
 */
size_t platform_get_core_count(void)
{
    assert(mmu_is_enabled());
    return 2;
}


/*
 * Print system identification.   MMU is NOT yet enabled.
 * Use Mackerel to print the identification from the system
 * configuration block.  Documentation in the OMAP4460 TRM p. 18.6.2 
 */
void platform_print_id(void)
{
    printf("Device: this is a VersatileExpress EMM board, probably emulated in GEM5\n");
}

/*
 * Return RAM size.  But how?  Let's just return a hardwired value for
 * now...
 */
size_t platform_get_ram_size(void)
{
    assert(!mmu_is_enabled());
    return 0x2000000;
}

/*
 * \brief Boot an arm app core
 *
 * \param core_id   APIC ID of the core to try booting
 * \param entry     Entry address for new kernel in the destination
 *                  architecture's lvaddr_t
 *
 * \returns Zero on successful boot, non-zero (error code) on failure
 */

//
// Pages in memory to use for posting information.  Must be in RAM.
//
#define AP_WAIT_PHYS    ((lpaddr_t)0x80020000)
#define AP_GLOBAL_PHYS  ((lpaddr_t)0x80021000)
#define AP_STARTING_UP  4422
#define AP_STARTED      6633

static void write_sysflags_reg(uint32_t regval);

int platform_boot_aps(coreid_t core_id, genvaddr_t gen_entry)
{
    assert(mmu_is_enabled());
    volatile uint32_t *ap_wait = (uint32_t*)local_phys_to_mem(AP_WAIT_PHYS);
    *ap_wait = AP_STARTING_UP;
    
    // write entry address of new kernel to SYSFLAG reg
    write_sysflags_reg(gen_entry);

    // raise SWI to signal app core to start
    gic_raise_softirq((1 << core_id), 1);
    return 0;
}
void platform_notify_bsp(void)
{
    assert(mmu_is_enabled());
    volatile uint32_t *ap_wait = (uint32_t*)local_phys_to_mem(AP_WAIT_PHYS);
    __atomic_store_n((lvaddr_t *)ap_wait, AP_STARTED, __ATOMIC_SEQ_CST );
}


//
// Sys Flag Register
//
static lvaddr_t sysflags = 0;
static void write_sysflags_reg(uint32_t regval)
{
    if (sysflags == 0) {
	sysflags = paging_map_device(VEXPRESS_MAP_SYSREG, VEXPRESS_MAP_SYSREG_SIZE);
    }
    *((uint32_t *)sysflags)= regval;
}




//
// TSC uses cpu private timer
//

#define STARTUP_TIMEOUT         0xffffff
static const uint32_t tsc_hz = 2000000000;
static cortex_a9_pit_t tsc;

void tsc_init(int timeslice)
{
    lvaddr_t tbase = paging_map_device( VEXPRESS_MAP_LOCAL_CPU_TIMER, 
					VEXPRESS_MAP_LOCAL_CPU_TIMER_SIZE );
    cortex_a9_pit_initialize(&tsc, (mackerel_addr_t)tbase );

    // write load
    uint32_t load = ~0ul;
    cortex_a9_pit_TimerLoad_wr(&tsc, load);

    //configure tsc
    cortex_a9_pit_TimerControl_prescale_wrf(&tsc, 0);
    cortex_a9_pit_TimerControl_int_enable_wrf(&tsc, 0);
    cortex_a9_pit_TimerControl_auto_reload_wrf(&tsc, 1);
    cortex_a9_pit_TimerControl_timer_enable_wrf(&tsc, 1);

    //
    // And now, a different timer... ??  
    //
    lpaddr_t addrs[2] = { VEXPRESS_MAP_TIMER01, VEXPRESS_MAP_TIMER23 };
    uint32_t irqs[2] = { 34, 35 };
    sp804_configure(2, tsc_hz, addrs, irqs);
    pit_init(timeslice, 0);
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
void gt_init(void) { return; }
uint32_t gt_read_low(void) { return 0; }
uint32_t gt_read_high(void) { return 0; }
