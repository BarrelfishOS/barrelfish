/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_DISTCAPS_H
#define BARRELFISH_DISTCAPS_H

#include <stdbool.h>
#include <stdint.h>
#include <barrelfish_kpi/capabilities.h>

/*
 * capstate: locking and ownership
 */

#define DISTCAP_STATE_FOREIGN (1 << 0)
#define DISTCAP_STATE_BUSY (1 << 1)

typedef uint8_t distcap_state_t;

static inline bool
distcap_state_is_busy(distcap_state_t state)
{
    return state & DISTCAP_STATE_BUSY;
}

static inline bool
distcap_state_is_foreign(distcap_state_t state)
{
    return state & DISTCAP_STATE_FOREIGN;
}

/*
 * Predicates related to sharing capabilities
 */

STATIC_ASSERT(ObjType_Num == 27, "Knowledge of all cap types");
static inline bool
distcap_needs_locality(enum objtype type)
{
    switch (type) {
    case ObjType_PhysAddr:
    case ObjType_RAM:
    case ObjType_CNode:
    case ObjType_FCNode:
    case ObjType_Dispatcher:
    case ObjType_EndPoint:
    case ObjType_Frame:
    case ObjType_DevFrame:
    case ObjType_VNode_x86_64_pml4:
    case ObjType_VNode_x86_64_pdpt:
    case ObjType_VNode_x86_64_pdir:
    case ObjType_VNode_x86_64_ptable:
    case ObjType_VNode_x86_32_pdpt:
    case ObjType_VNode_x86_32_pdir:
    case ObjType_VNode_x86_32_ptable:
    case ObjType_VNode_ARM_l1:
    case ObjType_VNode_ARM_l2:
    // XXX: KCB should need locality?
    //case ObjType_KernelControlBlock:
        return true;
    default:
        return false;
    }
}

STATIC_ASSERT(ObjType_Num == 27, "Knowledge of all cap types");
static inline bool
distcap_is_moveable(enum objtype type)
{
    switch (type) {
    case ObjType_PhysAddr:
    case ObjType_RAM:
    case ObjType_Frame:
        return true;
    default:
        return false;
    }
}

/*
 * Caching remote relations
 */

#define RRELS_COPY_BIT (1<<0)
#define RRELS_ANCS_BIT (1<<1)
#define RRELS_DESC_BIT (1<<2)

#endif
