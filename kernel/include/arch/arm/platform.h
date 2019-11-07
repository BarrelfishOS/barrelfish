/**
 * \file
 * \brief Platform interface for ARM boards.
 *
 * This file defines the hardware abstraction layer for ARM targets. Each
 * board is expected to have an implementation that corresponds to this
 * interface.
 *
 * This interface is expected to change as new boards are added.
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ARM_PLATFORM_H__
#define __ARM_PLATFORM_H__

#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/platform.h>

/*
 * Do any extra initialisation for this particular CPU (e.g. A9/A15).
 */
void platform_revision_init(void);

/*
 * Return the core count
 */
size_t platform_get_core_count(void);

/*
 * Print system identification. MMU is NOT yet enabled.
 */
void platform_print_id(void);

/*
 * Return the base address of the private peripheral region.
 */
lpaddr_t platform_get_private_region(void);

/*
 * Fill out provided `struct platform_info`
 */
void platform_get_info(struct platform_info *pi);
void armv7_get_info(struct arch_info_armv7 *ai);
void armv8_get_info(struct arch_info_armv8 *ai);

/*
 * Figure out how much RAM we have
 */
size_t platform_get_ram_size(void);

/*
 * Boot secondary processors
 */
errval_t platform_boot_core(hwid_t target, genpaddr_t gen_entry, genpaddr_t context);
void platform_notify_bsp(lpaddr_t *mailbox);

/*
 * UART locations
 */
extern lpaddr_t platform_uart_base[];
extern size_t platform_uart_size[];

// Helpers for enabling interrupts
#define IRQ_PRIO_LOWEST       (0xF)
#define IRQ_CPU_TRG_ALL       (0x3) // For two cores on the PandaBoard
#define IRQ_CPU_TRG_BSP       (0x1)
#define IRQ_EDGE_TRIGGERED    (0x1)
#define IRQ_LEVEL_SENSITIVE   (0x0)
#define IRQ_1_TO_N            (0x1)
#define IRQ_N_TO_N            (0x0)

/*
 * Interrupt controller interface
 */
errval_t platform_init_ic_bsp(void);
errval_t platform_init_ic_app(void);
uint32_t platform_get_active_irq(void);
void     platform_acknowledge_irq(uint32_t irq);
errval_t  platform_enable_interrupt(uint32_t int_id, uint16_t prio,
                              bool edge_triggered, bool one_to_n);

/*
* Timers
*/

/**
* @brief initialize the timer
*/
void platform_timer_init(int timeslice);

/**
* @brief check if the interrupt is a timer interrupt
*
* @param interrupt number
* @return TRUE if it's the timer interrupt
*/
bool platform_is_timer_interrupt(uint32_t irq);

/**
* @brief Return the IRQ to be used for the cpu driver timer
* @return The IRQ number of the timer interrupt.
*/
uint32_t platform_get_timer_interrupt(void);


#endif // __ARM_PLATFORM_H__
