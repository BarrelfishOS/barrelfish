/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __GIC_H__
#define __GIC_H__

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

// Helpers for enabling interrupts
#define GIC_IRQ_PRIO_LOWEST       (0xF)
#define GIC_IRQ_CPU_TRG_ALL       (0x3) // For two cores on the PandaBoard
#define GIC_IRQ_CPU_TRG_BSP       (0x1)
#define GIC_IRQ_EDGE_TRIGGERED    (0x1)
#define GIC_IRQ_LEVEL_SENSITIVE   (0x0)
#define GIC_IRQ_1_TO_N            (0x1)
#define GIC_IRQ_N_TO_N            (0x0)

/*
 * generic interrupt controller functionality
 */
void     gic_init(void);
void     gic_distributor_init(void);
void     gic_cpu_interface_init(void);
void     gic_cpu_interface_enable(void);
void     gic_cpu_interface_disable(void);
void     gic_disable_all_irqs(void);
void     gic_raise_softirq(uint8_t cpumask, uint8_t irq);
size_t   gic_cpu_count(void);

#endif // __GIC_H__
