/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __GIC_H__
#define __GIC_H__

// Helpers for enabling interrupts
#define GIC_IRQ_PRIO_LOWEST       (0xF)
#define GIC_IRQ_CPU_TRG_ALL       (0x3) // For two cores on the PandaBoard
#define GIC_IRQ_CPU_TRG_BSP       (0x1)
#define GIC_IRQ_EDGE_TRIGGERED    (0x1)
#define GIC_IRQ_LEVEL_SENSITIVE   (0x0)
#define GIC_IRQ_1_TO_N            (0x1)
#define GIC_IRQ_N_TO_N            (0x0)

 #endif // __GIC_H__