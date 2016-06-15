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

#include <a9_scu.h>
#include <global.h>
#include <init.h>
#include <paging_kernel_arch.h>
#include <platform.h>
#include <serial.h>
#include <pl011.h>
#include <assert.h>
#include <errors/errno.h>
#include <a9mpcore_map.h>
#include <vexpress_map.h>
#include <dev/cortex_a9_pit_dev.h>
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

/*
 * Print system identification.   MMU is NOT yet enabled.
 * TODO - Use Mackerel to print the identification from the system
 * configuration block.
 */
void platform_print_id(void)
{
    assert(!mmu_is_enabled());
    
    uint32_t id=
        *((uint32_t *)(VEXPRESS_MAP_SYSREG + VEXPRESS_SYS_ID));
    uint32_t procid0=
        *((uint32_t *)(VEXPRESS_MAP_SYSREG + VEXPRESS_SYS_PROCID0));
    uint32_t procid1=
        *((uint32_t *)(VEXPRESS_MAP_SYSREG + VEXPRESS_SYS_PROCID1));

    printf("Device: This is a VersatileExpress EMM board. "
           "ID=%08x PROCID0=%08x PROCID1=%08x\n",
           id, procid0, procid1);
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

int
platform_boot_aps(coreid_t core_id, genvaddr_t gen_entry) {
    assert(mmu_is_enabled());
    volatile uint32_t *ap_wait = (uint32_t*)local_phys_to_mem(AP_WAIT_PHYS);
    *ap_wait = AP_STARTING_UP;
    
    // write entry address of new kernel to SYSFLAG reg
    write_sysflags_reg(gen_entry);

    // raise SWI to signal app core to start
    gic_raise_softirq((1 << core_id), 1);
    return 0;
}

void
platform_notify_bsp(void) {
    assert(mmu_is_enabled());
    volatile uint32_t *ap_wait = (uint32_t*)local_phys_to_mem(AP_WAIT_PHYS);
    __atomic_store_n((lvaddr_t *)ap_wait, AP_STARTED, __ATOMIC_SEQ_CST);
}

//
// Sys Flag Register
//
static lvaddr_t sysregs = 0;
static void
write_sysflags_reg(uint32_t regval) {
    if (sysregs == 0) {
	    sysregs=
            paging_map_device(VEXPRESS_MAP_SYSREG, VEXPRESS_MAP_SYSREG_SIZE);
    }
    *((uint32_t *)sysregs + VEXPRESS_SYS_FLAGS)= regval;
}
