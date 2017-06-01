/**
 * \file
 * \brief Client for interacting with the process management server.
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/proc_mgmt_client.h>
#include <if/proc_mgmt_defs.h>

static void error_handler(struct proc_mgmt_binding *b, errval_t err)
{
#if defined(__x86_64__) || defined(__i386__)
    debug_printf("%p %p %p %p\n", __builtin_return_address(0), __builtin_return_address(1),__builtin_return_address(2),__builtin_return_address(3));
#endif
    debug_err(__FILE__, __func__, __LINE__, err,
              "error in proc_mgmt binding");
    abort();
}

static void proc_mgmt_accept_recv_handler(void *arg)
{
    struct proc_mgmt_lmp_binding *b = arg;
    struct lmp_recv_msg msg = LMP_RECV_MSG_INIT;
    struct capref cap;
    errval_t err;

    // try to retrieve a message from the channel
    err = lmp_chan_recv(&b->chan, &msg, &cap);
    if (err_is_fail(err)) {
        if (err_no(err) == LIB_ERR_NO_LMP_MSG) {
            // nothing there, re-register
            struct event_closure recv_handler = {
                .handler = proc_mgmt_accept_recv_handler,
                .arg = b,
            };
            err = lmp_chan_register_recv(&b->chan, b->b.waitset, recv_handler);
            b->b.error_handler(&b->b,
                    err_push(err, LIB_ERR_CHAN_REGISTER_RECV));
        } else {
            // real error, report to user
            b->b.error_handler(&b->b, err_push(err, LIB_ERR_LMP_CHAN_RECV));
        }
        return;
    }

    // if we're the monitor, we might be waiting for the other side's cap
    // TODO(razvan): Or the process manager, which for uses LMP_MONITOR_ACCEPT
    // as well.
    assert(b->chan.connstate == LMP_MONITOR_ACCEPT);
    assert(!capref_is_null(cap));
    b->chan.remote_cap = cap;
    b->chan.connstate = LMP_CONNECTED;

    // Allocate a new receive slot.
    // TODO(razvan): Whom is the receive slot for? Is it one of the spawnds?
    // Or is it some arbitrary non-spawnd client domain? Need a way to tell.
    err = lmp_chan_alloc_recv_slot(&b->chan);
    if (err_is_fail(err)) {
        // XXX: report the error, but continue
        b->b.error_handler(&b->b, err_push(err, LIB_ERR_LMP_ALLOC_RECV_SLOT));
    }

    // Run the RX handler; has a side-effect of registering for receive events.
    proc_mgmt_lmp_rx_handler(b);
}

static errval_t init_lmp_binding(struct proc_mgmt_lmp_binding *lmpb,
                                 struct waitset *ws, size_t buflen_words)
{
    errval_t err;

    proc_mgmt_lmp_init(lmpb, ws);

    /* allocate a cap slot for the new endpoint cap */
    err = slot_alloc(&lmpb->chan.local_cap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    /* allocate a local endpoint */
    err = lmp_endpoint_create_in_slot(buflen_words, lmpb->chan.local_cap,
            &lmpb->chan.endpoint);
    if (err_is_fail(err)) {
        /* TODO: free cap slot */
        return err_push(err, LIB_ERR_ENDPOINT_CREATE);
    }

    /* allocate an initial receive slot */
    err = lmp_chan_alloc_recv_slot(&lmpb->chan);
    if (err_is_fail(err)) {
        return err;
    }

    /* setup error handler */
    lmpb->b.error_handler = error_handler;

    /* setup initial receive handlers */
    // lmpb->b.rx_vtbl = proc_mgmt_rx_vtbl;

    // connect handlers
    lmpb->b.change_waitset(&lmpb->b, lmpb->b.waitset);
    return SYS_ERR_OK;
}

/**
 * \brief Accept a new LMP binding in a client from the process manager.
 *
 * Should only be used in the process manager.
 *
 * \param pmcb         Storage for binding state
 * \param ws           Waitset for handling incoming messages
 * \param buflen_words Size of incoming buffer, in number of words
 */
errval_t proc_mgmt_client_lmp_accept(struct proc_mgmt_lmp_binding *lmpb,
        struct waitset *ws, size_t lmp_buflen_words)
{
    errval_t err = init_lmp_binding(lmpb, ws, lmp_buflen_words);
    if (err_is_fail(err)) {
        return err;
    }

    // TODO(razvan): This should probably be something like LMP_PROC_MGMT_ACCEPT
    lmpb->chan.connstate = LMP_MONITOR_ACCEPT;
    lmpb->chan.remote_cap = NULL_CAP; // will be sent to us by the client

    // Register for receive notification on our special handler.
    struct event_closure receive_handler = {
        .handler = proc_mgmt_accept_recv_handler,
        .arg = lmpb,
    };
    err = lmp_chan_register_recv(&lmpb->chan, ws, receive_handler);
    if (err_is_fail(err)) {
        return err; // TODO: cleanup?
    }

    return SYS_ERR_OK;
}
