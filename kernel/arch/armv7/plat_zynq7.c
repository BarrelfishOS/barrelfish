/**
 * \file
 * \brief Platform code for the Xilinx Zynq7000-series SoCs
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <a9_gt.h>
#include <a9_scu.h>
#include <a9mpcore_map.h>
#include <assert.h>
#include <cp15.h>
#include <dev/cortex_a9_pit_dev.h>
#include <dev/zynq7/zynq_slcr_dev.h>
#include <errors/errno.h>
#include <gic.h>
#include <global.h>
#include <init.h>
#include <paging_kernel_arch.h>
#include <platform.h>
#include <serial.h>
#include <zynq7_map.h>
#include <zynq_uart.h>

#define MSG(format, ...) printk( LOG_NOTE, "ZYNQ7: "format, ## __VA_ARGS__ )

/*****************************************************************************
 *
 * Implementation of serial.h
 *
 *****************************************************************************/

unsigned int serial_console_port = 1;
unsigned int serial_debug_port = 1;
unsigned int serial_num_physical_ports = 2;

static lpaddr_t
zynq_port_addrs[ZYNQ_UART_MAX_PORTS]= {
    ZINQ7_UART0_BASEADDR,
    ZINQ7_UART1_BASEADDR
};

/*
 * Initialize the serial ports
 */
errval_t
serial_early_init(unsigned port) {
    spinlock_init(&global->locks.print);
    zynq_uart_early_init(port, zynq_port_addrs[port]);
    return SYS_ERR_OK;
}

errval_t
serial_init(unsigned port, bool initialize_hw) {
    assert(port < serial_num_physical_ports);
    assert(mmu_is_enabled());
    zynq_uart_init(port, initialize_hw);
    return SYS_ERR_OK;
};

void
serial_putchar(unsigned port, char c) {
    assert(port < serial_num_physical_ports);
    zynq_uart_putchar(port, c);
}

char
serial_getchar(unsigned port) {
    assert(port < serial_num_physical_ports);
    return zynq_uart_getchar(port);
}

/* Print system identification. MMU is NOT yet enabled. */
void
platform_print_id(void) {
    assert(!mmu_is_enabled());

    zynq_slcr_t slcr;
    zynq_slcr_initialize(&slcr, (mackerel_addr_t)ZINQ7_SYS_CTRL_BASEADDR);

#if 0
    int family=       zynq_slcr_PSS_IDCODE_FAMILY_rdf(&slcr);
    int subfamily=    zynq_slcr_PSS_IDCODE_SUBFAMILY_rdf(&slcr);
    int manufacturer= zynq_slcr_PSS_IDCODE_MANUFACTURER_ID_rdf(&slcr);
    zynq_slcr_devcode_t device= zynq_slcr_PSS_IDCODE_DEVICE_CODE_rdf(&slcr);

    /* See Zynq 7000 TRM p1631. */
    if(family != 0x1b || subfamily != 0x9 || manufacturer != 0x49) {
        panic("This doesn't look like a Zynq\n");
    }
#endif

    char buf[1024];
    zynq_slcr_PSS_IDCODE_pr(buf, 1023, &slcr);

    printf("This is a Zynq.\n");
    printf("%s", buf);
}

void
platform_get_info(struct platform_info *pi) {
    pi->arch     = PI_ARCH_ARMV7A;
    pi->platform = PI_PLATFORM_ZYNQ7;
}

/* The zc706 has 2GB of RAM beginning at address 0. */
size_t
platform_get_ram_size(void) {
    return (ZYNQ7_DDR_MEM_HIGHADDR - ZYNQ7_DDR_MEM_BASEADDR) + 1;
}

/**
 * Notify the BSP that this AP has booted. 
 */

/**
 * \brief Boot an arm app core
 *
 * \param core_id   ID of the core to try booting
 * \param entry     Entry address for new kernel in the destination
 *                  architecture's lvaddr_t
 *
 * \returns Zero on successful boot, non-zero (error code) on failure
 */
int
platform_boot_aps(coreid_t core_id, genvaddr_t gen_entry) {
    panic("Unimplemented.\n");
    return 0;
}

void
platform_notify_bsp(void) {
    panic("Unimplemented.\n");
}

uint32_t tsc_hz = 0;
uint32_t sys_clk;

void
a9_probe_tsc(void) {
    panic("Unimplemented.\n");
}
