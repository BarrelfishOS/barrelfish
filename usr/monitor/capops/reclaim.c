/**
 * \file
 * \brief Monitor RAM reclaiming
 */

/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <barrelfish_kpi/init.h>
#include <monitor_invocations.h>
#include <barrelfish/event_queue.h>
#include <barrelfish/slot_alloc.h>
#include <barrelfish/deferred.h>

#include "capops.h"
#include "delete_int.h"

static struct capref reclaim_cap;
static struct deferred_event reclaim_ev;

static void reclaim_ram(void *arg)
{
    errval_t err;
    err = monitor_reclaim_ram(reclaim_cap);
    if (err_is_ok(err)) {
        struct capability cap;
        err = cap_direct_identify(reclaim_cap, &cap);
        assert(err_is_ok(err));
        assert(cap.type == ObjType_RAM);
        debug_printf("reclaimed RAM base %#"PRIxGENVADDR", bytes %#"PRIxGENSIZE"\n",
                cap.u.ram.base, cap.u.ram.bytes);
        // send ram to memserv
        send_new_ram_cap(reclaim_cap);
        // reregister for next cap in 10ms
        delayus_t ten_ms = 10ULL*1000;
        err = deferred_event_register(&reclaim_ev, get_default_waitset(), ten_ms,
                                      MKCLOSURE(reclaim_ram, NULL));
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "%s: registering deferred event", __FUNCTION__);
        }
    } else if (err_no(err) == SYS_ERR_CAP_NOT_FOUND) {
        // reregister for another sweep in 1sec
        delayus_t ten_ms = 1ULL*1000*1000;
        err = deferred_event_register(&reclaim_ev, get_default_waitset(), ten_ms,
                                      MKCLOSURE(reclaim_ram, NULL));
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "%s: registering deferred event", __FUNCTION__);
        }
    } else {
        // something went wrong:
        DEBUG_ERR(err, "while reclaiming RAM from kernel");
        // do not reregister for another sweep
    }
}

errval_t reclaim_ram_init(void)
{
    errval_t err;
    err = slot_alloc(&reclaim_cap);
    if (err_is_fail(err)) {
        return err;
    }
    deferred_event_init(&reclaim_ev);
    // queue first reclaim event with 10ms delay
    delayus_t one_sec = 10ULL*1000;
    return deferred_event_register(&reclaim_ev, get_default_waitset(), one_sec,
                                   MKCLOSURE(reclaim_ram, NULL));
}
