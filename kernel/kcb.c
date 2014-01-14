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

// returns true if we removed `to_remove'
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
        to_remove->prev = NULL;
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

            // break out if we're done
            return SYS_ERR_OK;
        }
        k = k->next;
    } while (k != kcb_current);

    return SYS_ERR_KCB_NOT_FOUND;
}
