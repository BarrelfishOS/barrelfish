/*
 * Copyright (c) 2016, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.  If
 * you do not find this file, copies can be found by writing to: ETH Zurich
 * D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich, Attn: Systems Group.
 */

#ifndef __ARMV7_BOOT_PROTOCOL_H
#define __ARMV7_BOOT_PROTOCOL_H

/* Inter-core boot protocol for ARMv7 CPUs. */

#include <kernel.h>

#include <barrelfish_kpi/spinlocks_arch.h>

struct armv7_boot_record {
    /* Several cores may share this record. */
    spinlock_t lock;

    /* The kernel-virtual address of the core data structure. */
    lvaddr_t core_data;

    /* Completion notification from the APP core. */
    uint32_t done;

    /* The MPID value (hardware core ID) of the core to boot. */
    uint32_t target_mpid;
};

void plat_advance_aps(void);

#endif /* __ARMV7_BOOT_PROTOCOL_H */
