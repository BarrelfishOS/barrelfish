/**
 * \file plat_apm88xxxx.c
 * \brief
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <offsets.h>
#include <platform.h>
#include <serial.h>
#include <dev/apm88xxxx/apm88xxxx_pc16550_dev.h>

/* the maximum number of UARTS supported */
#define MAX_NUM_UARTS 4

static apm88xxxx_pc16550_t ports[MAX_NUM_UARTS];


errval_t serial_init(unsigned port, bool initialize_hw)
{
    if (port >= MAX_NUM_UARTS) {
        return SYS_ERR_SERIAL_PORT_INVALID;
    }

    if ((lpaddr_t)ports[port].base == (uart_base[port] + KERNEL_OFFSET)) {
        return SYS_ERR_OK;
    }

    apm88xxxx_pc16550_t *uart = &ports[port];
    apm88xxxx_pc16550_initialize(uart, (mackerel_addr_t)(uart_base[port] + KERNEL_OFFSET));

    if (!initialize_hw) {
        // hw initialized, this is for non-bsp cores, where hw has been
        // initialized by bsp core and we come through here just to setup our
        // local apm88xxxx_pc16550 struct for the port.
        return SYS_ERR_OK;
    }

    panic("device init NYI");
    return SYS_ERR_OK;
}

errval_t serial_early_init(unsigned port)
{
    if (port >= MAX_NUM_UARTS) {
        return SYS_ERR_SERIAL_PORT_INVALID;
    }

    if ((lpaddr_t)ports[port].base == uart_base[port]) {
        return SYS_ERR_OK;
    }

    apm88xxxx_pc16550_t *uart = &ports[port];
    apm88xxxx_pc16550_initialize(uart, (mackerel_addr_t)uart_base[port]);
    return SYS_ERR_OK;
}

errval_t serial_early_init_mmu_enabled(unsigned port)
{
    return serial_early_init(port);
}


/**
 * \brief Prints a single character to a serial port.
 */
void serial_putchar(unsigned port, char c)
{
    assert(port < MAX_NUM_UARTS);
    assert(ports[port].base != 0);
    // Wait until FIFO can hold more characters
    while(!apm88xxxx_pc16550_LSR_thre_rdf(&ports[port]));
    // Write character
    apm88xxxx_pc16550_THR_thr_wrf(&ports[port], c);
}

/**
 * \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
char serial_getchar(unsigned port)
{
    assert(port < MAX_NUM_UARTS);
    assert(ports[port].base != 0);

    // Wait until character available
    while(!apm88xxxx_pc16550_LSR_dr_rdf(&ports[port]));
    // Read a character from FIFO
    return apm88xxxx_pc16550_RBR_rbr_rdf(&ports[port]);
}


void platform_get_info(struct platform_info *pi)
{

}

void armv8_get_info(struct arch_info_armv8 *ai)
{

}

/*
 * Timers
 */
void     timers_init(int timeslice)
{
    uint64_t CNTKCTL_EL1;
     __asm volatile("mrs %[CNTKCTL_EL1], CNTKCTL_EL1" : [CNTKCTL_EL1] "=r" (CNTKCTL_EL1));
     CNTKCTL_EL1 |= (1 << 9); // Dont trap access to CNTP_* to EL1
     CNTKCTL_EL1 |= (1 << 8); // Dont trap access to CNTV_* to EL1
     CNTKCTL_EL1 |= (1 << 1); // Dont trap access to CNTFRQ* to EL1
     CNTKCTL_EL1 |= (1 << 0); // Dont trap access to CNTFRQ* to EL1
     __asm volatile("msr CNTKCTL_EL1, %[CNTKCTL_EL1]" : : [CNTKCTL_EL1] "r" (CNTKCTL_EL1));


     uint32_t PMCR_EL0  = 0;

     PMCR_EL0 |= (1 << 0); /* All counters are enabled.*/
     PMCR_EL0 |= (1 << 1); /* reset all event counters */
     PMCR_EL0 |= (1 << 2); /* reset all clock counters */
     PMCR_EL0 |= (0 << 3); /* set counter to tick every clock cycle (1=ever 64th) */
     PMCR_EL0 |= (1 << 4); /* enable event support */
     PMCR_EL0 |= (0 << 5); /* don't disable cycle counte r*/

     PMCR_EL0 |= (6 << 11); /* Six counters */

     __asm volatile("msr PMCR_EL0, %[PMCR_EL0]" : : [PMCR_EL0] "r" (PMCR_EL0));


     uint32_t PMUSERENR_EL0  = 0;

     PMUSERENR_EL0 |= (1 << 0);  /* don't trap access to PM registers to EL 1 */
     PMUSERENR_EL0 |= (1 << 1);  /* don't trap software increment wrap to EL 1 */
     PMUSERENR_EL0 |= (1 << 2);  /* don't trap cycle counter to EL 1 */
     PMUSERENR_EL0 |= (1 << 3);  /* don't trap event counter read to EL 1*/

     __asm volatile("msr PMUSERENR_EL0, %[PMUSERENR_EL0]" : : [PMUSERENR_EL0] "r" (PMUSERENR_EL0));
}

uint64_t timestamp_read(void)
{

    return 0;
}

uint32_t timestamp_freq(void)
{
    return 1;
}

bool     timer_interrupt(uint32_t irq)
{
    return 0;
}
