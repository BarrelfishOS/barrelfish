/**
 * \file
 * \brief Domain management
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"

static void handle_notification(void *arg)
{
    struct lmp_endpoint *ep = arg;
    errval_t err;

    do { // consume messages
        struct lmp_recv_msg msg = LMP_RECV_MSG_INIT;
        err = lmp_endpoint_recv(ep, &msg.buf, NULL);

        if (err_is_ok(err)) {
            assert(msg.buf.msglen == 1);
            domainid_t domid = msg.words[0];

            // TODO
            if (domid != 0) {
                debug_printf("Dispatcher with domain ID %"PRIuDOMAINID" exited\n",
                             domid);
            }
        } else if (err_no(err) != LIB_ERR_NO_LMP_MSG) {
            DEBUG_ERR(err, "unexpected error from lmp_endpoint_recv");
        }
    } while(err_is_ok(err));

    // re-register
    struct event_closure cl = {
        .handler = handle_notification,
        .arg = arg,
    };
    err = lmp_endpoint_register(ep, get_default_waitset(), cl);
    assert(err_is_ok(err));
}

void domain_mgmt_init(void)
{
    errval_t err;

    /* Register notification endpoint with kernel */
    struct capref epcap;
    struct lmp_endpoint *notifyep;
    err = endpoint_create(2 * 12, &epcap, &notifyep);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed creating endpoint");
    }

    // register to receive on this endpoint
    struct event_closure cl = {
        .handler = handle_notification,
        .arg = notifyep,
    };
    err = lmp_endpoint_register(notifyep, get_default_waitset(), cl);
    assert(err_is_ok(err));

    err = invoke_monitor_register(epcap);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not register with kernel");
    }
}
