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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <kcb.h>
#include <dispatch.h>
#include <systime.h>

// this is used to pin a kcb for critical sections
bool kcb_sched_suspended = false;

void kcb_add(struct kcb* new_kcb)
{
    if (kcb_current->next) {
        // Insert new KCB in ring
        assert(kcb_current->prev);

        new_kcb->next = kcb_current->next;
        new_kcb->prev = kcb_current;
        new_kcb->next->prev = new_kcb;
        new_kcb->prev->next = new_kcb;
    } else {
        // Only one KCB currently dispatching
        kcb_current->next = kcb_current->prev = new_kcb;
        new_kcb->next = new_kcb->prev = kcb_current;
    }
}

errval_t kcb_remove(struct kcb *to_remove)
{
    if (to_remove == kcb_current) {
        if (to_remove->next->next == to_remove) {
            assert(to_remove->next->prev == to_remove);
            to_remove->next->next = to_remove->next->prev = NULL;
        }
        else {
            to_remove->prev->next = to_remove->next;
            to_remove->next->prev = to_remove->prev;
        }

        // intentionally leaving to_remove->next alone
        // so switch_kcb doesn't break
        to_remove->prev = NULL;
        to_remove->kernel_off = systime_now();

        return SYS_ERR_OK;
    }

    // This probably failes atm.
    for (struct kcb* k = kcb_current->next; k != kcb_current; k = k->next) {
        if (k == to_remove) {
            // remove KCB from ring
            k->prev->next = k->next;
            k->next->prev = k->prev;

            // Clear ring to disable switching mechanism
            if (k->next->next == k->next) {
                k->next->next = k->next->prev = NULL;
            }

            // Clear next and prev of removed kcb
            k->next = k->prev = NULL;

            // Update kernel_off & break out if we're done
            k->kernel_off = systime_now();
            return SYS_ERR_OK;
        }
    }

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
        if (cap->type == ObjType_EndPointLMP) {
            printk(LOG_NOTE, "[irq] updating current core id to %d for %s\n",
                    my_core_id, get_disp_name(cap->u.endpointlmp.listener));
            struct dispatcher_shared_generic *disp =
                get_dispatcher_shared_generic(cap->u.endpointlmp.listener->disp);
            disp->curr_core_id = my_core_id;
        }
    }
}
