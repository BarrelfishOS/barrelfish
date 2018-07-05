/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __GIC_V3_H__
#define __GIC_V3_H__

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <barrelfish_kpi/types.h>

/*
 * generic interrupt controller functionality
 */
void     gic_init(void); //
void     gic_distributor_init(void);
void     gic_cpu_interface_init(void);
void     gic_cpu_interface_enable(void); //
void     gic_cpu_interface_disable(void);
void     gic_enable_interrupt(uint32_t int_id, uint8_t cpu_targets, uint16_t prio,
                              bool edge_triggered, bool one_to_n);
void     gic_disable_all_irqs(void);
uint32_t gic_get_active_irq(void); //
void     gic_ack_irq(uint32_t irq); //
void     gic_raise_softirq(uint8_t cpumask, uint8_t irq); //
size_t   gic_cpu_count(void);


#endif // __GIC_V3_H__
