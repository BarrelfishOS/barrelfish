/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <bulk_transfer/bulk_sm.h>

errval_t bulk_sm_multiple_event_dispatch_non_block(struct bulk_sm_ws_item *item)
{
    assert(item);
    errval_t err = SYS_ERR_OK;
    bool event_found = false;

    while (item) {
        if (item->ws) {
            err = event_dispatch_non_block(item->ws);
            if (err_is_fail(err)) {
                if (err != LIB_ERR_NO_EVENT) {
                    // should report dispatching problem
                    break;
                }
            } else {
                event_found = true;
            }
        }

        item = item->next;
    }

    if (err_is_fail(err) && err != LIB_ERR_NO_EVENT) {
        return err; // dispatch error
    } else if (!event_found) {
        return LIB_ERR_NO_EVENT;
    } else {
        return SYS_ERR_OK;
    }
}

errval_t bulk_sm_multiple_event_dispatch(struct bulk_sm_ws_item *item)
{
    while (1) {
        errval_t err = bulk_sm_multiple_event_dispatch_non_block(item);

        if (err_is_fail(err) && err != LIB_ERR_NO_EVENT) {
            return err;
        } else if (err == LIB_ERR_NO_EVENT) {
           // debug_printf("No event. yielding....\n");
            thread_yield();
        } else {
            return SYS_ERR_OK;
        }
    }
}
