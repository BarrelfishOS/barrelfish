/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_ARCH_ARM_IRQ_H
#define KERNEL_ARCH_ARM_IRQ_H

///< Dummy definition, size of dispatch table in case someone adds IRQs for ARMv5
#define NDISPATCH               1

struct capability;
struct idc_recv_msg;
struct sysret irq_table_set(struct capability *to, struct idc_recv_msg *msg);
struct sysret irq_table_delete(struct capability *to, struct idc_recv_msg *msg);

struct kcb;
inline errval_t irq_table_notify_domains(struct kcb *kcb);
inline errval_t irq_table_notify_domains(struct kcb *kcb)
{
    return SYS_ERR_OK;
}

#endif // KERNEL_ARCH_ARM_IRQ_H
