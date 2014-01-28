/**
 * \file
 * \brief Kernel control block declarations.
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <kcb.h>
#include <dispatch.h>

errval_t kcb_remove(struct kcb *to_remove)
{
    if (to_remove == kcb_current) {
        printk(LOG_NOTE, "kcb_remove: to_remove == kcb_current... don't delete to_remove->next now so later switch works\n");
        if (to_remove->next->next == to_remove) {
            to_remove->next->next = to_remove->next->prev = NULL;
        } else {
            to_remove->prev->next = to_remove->next;
            to_remove->next->prev = to_remove->prev;
        }
        // intentionally leaving to_remove->next alone, so switch_kcb doesn't
        // break
        to_remove->prev = NULL;
        to_remove->kernel_off = kernel_now;
        return SYS_ERR_OK;
    }

    struct kcb *k = kcb_current;

    do {
        if (k == to_remove) {
            // remove kcb from ring
            k->prev->next = k->next;
            k->next->prev = k->prev;
            if (k->next->next == k->next) {
                // clear ring to disable switching mechanism if only one kcb
                // left
                k->next->next = k->next->prev = NULL;
            }
            // clear next and prev of removed kcb to not leak other kcb addrs
            k->next = k->prev = NULL;

            // update kernel_off & break out if we're done
            k->kernel_off = kernel_now;
            return SYS_ERR_OK;
        }
        k = k->next;
    } while (k != kcb_current);

    return SYS_ERR_KCB_NOT_FOUND;
}

void kcb_update_core_id(struct kcb *kcb)
{
#ifdef CONFIG_SCHEDULER_RBED
    for (struct dcb *d = kcb->queue_head; d; d = d->next) {
        printk(LOG_NOTE, "[sched] updating current core id to %d for %s\n",
                my_core_id, get_disp_name(d));
        struct dispatcher_shared_generic *disp =
            get_dispatcher_shared_generic(d->disp);
        disp->curr_core_id = my_core_id;
    }
#elif CONFIG_SCHEDULER_RR
#error NYI!
#else
#error must define scheduler policy in Config.hs
#endif
    // do it for dcbs in wakeup queue
    for (struct dcb *d = kcb->wakeup_queue_head; d; d=d->wakeup_next) {
        printk(LOG_NOTE, "[wakeup] updating current core id to %d for %s\n",
                my_core_id, get_disp_name(d));
        struct dispatcher_shared_generic *disp =
            get_dispatcher_shared_generic(d->disp);
        disp->curr_core_id = my_core_id;
    }

    for (int i = 0; i < NDISPATCH; i++) {
        struct capability *cap = &kcb->irq_dispatch[i].cap;
        if (cap->type == ObjType_EndPoint) {
            printk(LOG_NOTE, "[irq] updating current core id to %d for %s\n",
                    my_core_id, get_disp_name(cap->u.endpoint.listener));
            struct dispatcher_shared_generic *disp =
                get_dispatcher_shared_generic(cap->u.endpoint.listener->disp);
            disp->curr_core_id = my_core_id;
        }
    }
}
