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
#include <barrelfish/nameservice_client.h>
#include <barrelfish/proc_mgmt_client.h>
#include <if/octopus_defs.h>
#include <if/proc_mgmt_defs.h>

struct proc_mgmt_bind_retst {
    errval_t err;
    struct proc_mgmt_binding *b;
    bool present;
};

static void error_handler(struct proc_mgmt_binding *b, errval_t err)
{
#if defined(__x86_64__) || defined(__i386__)
    debug_printf("%p %p %p %p\n",
                 __builtin_return_address(0),
                 __builtin_return_address(1),
                 __builtin_return_address(2),
                 __builtin_return_address(3));
#endif
    debug_err(__FILE__, __func__, __LINE__, err,
              "asynchronous error in proc_mgmt binding");
    abort();
}

static void proc_mgmt_bind_cont(void *st, errval_t err,
        struct proc_mgmt_binding *b)
{
    struct proc_mgmt_bind_retst *retst = (struct proc_mgmt_bind_retst*) st;
    assert(retst != NULL);
    assert(!retst->present);
    retst->err = err;
    retst->b = b;
    retst->present = true;
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
            b->b.error_handler(&b->b, err_push(err, LIB_ERR_CHAN_REGISTER_RECV));
        } else {
            // real error, report to user
            b->b.error_handler(&b->b, err_push(err, LIB_ERR_LMP_CHAN_RECV));
        }
        return;
    }

    // TODO(razvan): LMP_PROC_MGMT_ACCEPT ?
    assert(b->chan.connstate == LMP_MONITOR_ACCEPT);
    assert(!capref_is_null(cap));
    b->chan.remote_cap = cap;
    b->chan.connstate = LMP_CONNECTED;

    /* allocate a new receive slot */
    err = lmp_chan_alloc_recv_slot(&b->chan);
    if (err_is_fail(err)) {
        // XXX: report the error, but continue
        b->b.error_handler(&b->b, err_push(err, LIB_ERR_LMP_ALLOC_RECV_SLOT));
    }

    /* Run the RX handler; has a side-effect of registering for receive events */
    proc_mgmt_lmp_rx_handler(b);
}

static errval_t init_lmp_binding(struct proc_mgmt_lmp_binding *lmpb,
                                 struct waitset *ws,
                                 size_t buflen_words)
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
        // TODO(razvan): Free cap slot.
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
    // TODO(razvan): Don't think this is needed, but dunno for sure yet.
    // lmpb->b.rx_vtbl = monitor_rx_vtbl;

    // connect handlers
    lmpb->b.change_waitset(&lmpb->b, lmpb->b.waitset);
    return SYS_ERR_OK;
}

/**
 * \brief Accept a new LMP binding to a proc mgmt client.
 *
 * Should only be used in the process manager.
 *
 * \param lmpb         Storage for binding state
 * \param ws           Waitset for handling incoming messages
 * \param buflen_words Size of incoming buffer, in number of words
 */
errval_t proc_mgmt_client_lmp_accept(struct proc_mgmt_lmp_binding *lmpb,
                                     struct waitset *ws,
                                     size_t lmp_buflen_words)
{
    errval_t err = init_lmp_binding(lmpb, ws, lmp_buflen_words);
    if (err_is_fail(err)) {
        return err;
    }

    lmpb->chan.connstate = LMP_MONITOR_ACCEPT;  // TODO(razvan): LMP_PROC_MGMT_ACCEPT?
    lmpb->chan.remote_cap = NULL_CAP; // will be sent to us by the client

    /* Register for receive notification on our special handler */
    struct event_closure receive_handler = {
        .handler = proc_mgmt_accept_recv_handler,
        .arg = lmpb,
    };
    err = lmp_chan_register_recv(&lmpb->chan, ws, receive_handler);
    if (err_is_fail(err)) {
        return err;  // TODO(razvan): cleanup?
    }

    return SYS_ERR_OK;
}


/**
 * \brief Initiate a new LMP binding to the process manager
 *
 * To be used by the monitor for setting up the privileged channel used for
 * spawnd discovery.
 * Requires an explicit remote endpoint cap allocated by the process manager.
 *
 * \param lmpb         Storage for binding state
 * \param ep           Remote endpoint of the process manager
 * \param ws           Waitset for handling incoming messages
 * \param cont         Continuation for when binding completes or fails
 * \param st           State passed to continuation function
 * \param buflen_words Size of incoming buffer, in number of words
 */
errval_t proc_mgmt_client_lmp_bind(struct proc_mgmt_lmp_binding *lmpb,
                                   struct capref ep,
                                   proc_mgmt_bind_continuation_fn *cont,
                                   void *st,
                                   struct waitset *ws,
                                   size_t lmp_buflen_words)
{
    errval_t err = init_lmp_binding(lmpb, ws, lmp_buflen_words);
    if (err_is_fail(err)) {
        return err;
    }

    lmpb->chan.remote_cap = ep;

    // Send the local endpoint cap to the process manager.
    lmpb->chan.connstate = LMP_CONNECTED; /* pre-established */
    err = lmp_chan_send0(&lmpb->chan, 0, lmpb->chan.local_cap);
    if (err_is_fail(err)) {
        // TODO(razvan): This, below.
        /* XXX: I'm lazily assuming this can never fail with a transient error,
         * since we only do it once at dispatcher startup. If not, we need to
         * register and retry here */
        assert(!lmp_err_is_transient(err));
        return err;
    }

    /* Run the RX handler; has a side-effect of registering for receive events */
    proc_mgmt_lmp_rx_handler(lmpb);

    /* Run the continuation */
    cont(st, SYS_ERR_OK, &lmpb->b);

    return SYS_ERR_OK;
}

errval_t proc_mgmt_bind_client(void)
{
    struct proc_mgmt_binding *b = get_proc_mgmt_binding();
    if (b != NULL) {
        return SYS_ERR_OK;
    }

    errval_t err;
    iref_t iref;
    // Try using nameserver to retrievew the proc mgmt iref.
    err = nameservice_blocking_lookup("proc_mgmt", &iref);
    if (err_is_fail(err)) {
        return err;
    }
    
    // Initiate bind.
    struct proc_mgmt_bind_retst bindst = {
        .present = false
    };

    err = proc_mgmt_bind(iref, proc_mgmt_bind_cont, &bindst,
            get_default_waitset(), /*IDC_BIND_FLAG_RPC_CAP_TRANSFER*/IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "proc_mgmt_bind");
    }

    // Wait for bind completion.
    while (!bindst.present) {
        messages_wait_and_handle_next();
    }

    if (err_is_fail(bindst.err)) {
        return bindst.err;
    }

    proc_mgmt_rpc_client_init(bindst.b);

    set_proc_mgmt_binding(bindst.b);

    return SYS_ERR_OK;
}

errval_t proc_mgmt_add_spawnd(iref_t iref, coreid_t core_id)
{
    errval_t err = proc_mgmt_bind_client();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "proc_mgmt_bind_client");
        return err;
    }

    struct proc_mgmt_binding *b = get_proc_mgmt_binding();
    assert(b != NULL);

    err = b->tx_vtbl.add_spawnd(b, NOP_CONT, core_id, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "add_spawnd");
    }

    return err;
}
