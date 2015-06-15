/**
 * \file
 * \brief Arch-generic debug system calls implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdio.h>
#include <string.h>
#include <syscall.h>
#include <barrelfish_kpi/init.h>
#include <barrelfish_kpi/syscalls.h>
#include <capabilities.h>
#include <cap_predicates.h>
#include <coreboot.h>
#include <mdb/mdb.h>
#include <mdb/mdb_tree.h>
#include <cap_predicates.h>
#include <dispatch.h>
#include <distcaps.h>
#include <wakeup.h>
#include <paging_kernel_helper.h>
#include <paging_kernel_arch.h>
#include <exec.h>
#include <irq.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <kcb.h>



// If you create more capability types you need to deal with them
// in the table below.
STATIC_ASSERT(27 == ObjType_Num, "Knowledge of all cap types");


errval_t
caps_debug_print(struct cte *c, struct cap_visited_list *parent)
{
    genpaddr_t base;
    uint8_t bits;
    char* name = NULL;

//    struct cap_visited_list this = { .parent = parent, .c = c };

    switch (c->cap.type) {
    // all cap types
    case ObjType_CNode: {
//        struct CNode *cn = &(c->cap.u.cnode);
//        for (cslot_t slot = 0; slot < 1UL << cn->bits; slot++) {
//
//            struct cte *child = (struct cte *) local_phys_to_mem(
//                    cn->cnode + slot * sizeof(*c));
//
//            uint8_t recurse = 1;
//
//            struct cap_visited_list *p = parent;
//            int depth = 0;
//            while (p && recurse) {
//                recurse = p->c != child;
//                p = p->parent;
//                depth++;
//            }
//            if (recurse && depth < 4) {
//                errval_t err = caps_debug_print(child, &this);
//                if (err_is_fail(err)) {
//                    return err;
//                }
//            }
//        }
        break;
    }
    case ObjType_PhysAddr: {
        base = c->cap.u.physaddr.base;
        bits = c->cap.u.physaddr.bits;
        name = "PhysAddr";
        break;
    }
    case ObjType_RAM: {
        base = c->cap.u.ram.base;
        bits = c->cap.u.ram.bits;
        name = "RAM";
        break;
    }
    case ObjType_Frame: {
        base = c->cap.u.frame.base;
        bits = c->cap.u.frame.bits;
        name = "Frame";
        break;
    }
    case ObjType_DevFrame: {
        base = c->cap.u.devframe.base;
        bits = c->cap.u.devframe.bits;
        name = "DevFrame";
        break;
    }
    case ObjType_Null: {
        break;
    }
    default: {
        // Unhandled source type for mint
        printk(LOG_NOTE, "Unknown capability type: %d\n", c->cap.type);
        break;
    }
    }

    if (name) {
        printk(LOG_NOTE,
                "%s: base=0x%"PRIxGENPADDR" bits=%d end=0x%"PRIxGENPADDR"\n",
                name, base, bits, base + (1UL << bits));
    }

    return SYS_ERR_OK;
}


static bool sys_debug_print_capabilities_check_cnode(struct cte *cte, struct cte **dispatcher) {
    enum objtype type = cte->cap.type;

    if (type == ObjType_Dispatcher) {
        *dispatcher = cte;
        return true;
    }

    if (type == ObjType_KernelControlBlock) {
        // We are looking at init's rootcn, and found the reference to it in
        // the KCB.
        struct kcb *kcb = (struct kcb*) local_phys_to_mem(get_address(&cte->cap));
        struct cte *cn = (struct cte*) local_phys_to_mem(get_address(&kcb->init_rootcn.cap));
        struct cte *cn_task = (struct cte*) local_phys_to_mem(get_address(&cn[ROOTCN_SLOT_TASKCN].cap));
        *dispatcher = &cn_task[TASKCN_SLOT_DISPATCHER];
        return true;
    }

    struct cte *cn = (struct cte*) local_phys_to_mem(get_address(&cte->cap));

    if (type == ObjType_CNode) {
        // are we task cnode?
        if (cn[TASKCN_SLOT_DISPATCHER].cap.type == ObjType_Dispatcher) {
            *dispatcher = &cn[TASKCN_SLOT_DISPATCHER];
            return true;
        }

        if (cn[ROOTCN_SLOT_TASKCN].cap.type == ObjType_CNode) {
            struct cte *cn_task = (struct cte*) local_phys_to_mem(get_address(&cn[ROOTCN_SLOT_TASKCN].cap));

            if (cn_task[TASKCN_SLOT_DISPATCHER].cap.type == ObjType_Dispatcher) {
                *dispatcher = &cn_task[TASKCN_SLOT_DISPATCHER];
                return true;
            }
        }
    }

    return false;
}

static errval_t sys_debug_print_capabilities_cb(struct cte *cte, void *data) {
//    printk(LOG_NOTE, "cte=%p\n", cte);

    struct dcb *my_dcb = (struct dcb *) data;

    struct cte *result;
    errval_t err = mdb_find_cap_for_address(mem_to_local_phys((lvaddr_t) cte), &result);
    if (err_is_fail(err)) {
        printk(LOG_ERR, "Type of cap: %d, kernel address %p, phys. address 0x%"PRIxLPADDR"\n", cte->cap.type, cte, mem_to_local_phys((lvaddr_t) cte));
        printk(LOG_ERR, "kcb_current = %p\n", kcb_current);
        printk(LOG_ERR, "%s:%s:%d \n", __FILE__, __FUNCTION__, __LINE__);
        mdb_dump_all_the_things();
        return err;
    }

    assert(result->cap.type == ObjType_CNode ||
           result->cap.type == ObjType_Dispatcher ||
           result->cap.type == ObjType_KernelControlBlock);

    struct cte *dispatcher;

    while (!sys_debug_print_capabilities_check_cnode(result, &dispatcher)) {
        err = mdb_find_cap_for_address(mem_to_local_phys((lvaddr_t) result), &result);
        if (err_is_fail(err)) {
            printk(LOG_ERR, "Type of cap: %d\n", cte->cap.type);
            printk(LOG_ERR, "%s:%s:%d \n", __FILE__, __FUNCTION__, __LINE__);
            return err;
        }
    }

    assert(dispatcher->cap.type == ObjType_Dispatcher);

    struct dcb *dcb = dispatcher->cap.u.dispatcher.dcb;
    dispatcher_handle_t handle = dcb->disp;
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);

    if (my_dcb == dcb) {
        printk(LOG_NOTE, "disp->name=%s\n", disp->name);
        caps_debug_print(cte, NULL);
    }

    return SYS_ERR_OK;
}

struct sysret
sys_debug_print_capabilities(void) {

    errval_t err = mdb_traverse(MDB_TRAVERSAL_ORDER_ASCENDING, sys_debug_print_capabilities_cb, dcb_current);

    return SYSRET(err);
}
