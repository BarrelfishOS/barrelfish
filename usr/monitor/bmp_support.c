/**
 * \file
 * \brief BMP channel support
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <inttypes.h>
#include "monitor.h"

/* FIXME: why isn't there a header file for these prototypes? */
extern errval_t beehive_create_cap(coreid_t coreid, int chanid,
                                   struct capref *retcap);
extern errval_t beehive_chan_allocate(struct capref ep, int *chanid);

/******* stack-ripped monitor_bind_bmp_client_request *******/

static void monitor_bind_bmp_client_request_error(struct monitor_binding *st,
                                                  uintptr_t conn_id,
                                                  uintptr_t domain_id,
                                                  errval_t err)
{
    errval_t err2;

    if (conn_id != 0) {
        err2 = remote_conn_free(conn_id);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err2, "remote_conn_free failed");
        }
    }

    err2 = st->tx_vtbl.bind_bmp_reply_client(st, NOP_CONT, 0, domain_id,
                                             NULL_CAP, 0, err);
    if (err_is_fail(err2)) {
        DEBUG_ERR(err2, "error reply failed");
    }
}

static void bind_bmp_request_handler(struct intermon_binding *b,
                                     struct intermon_msg_queue_elem *e);

struct bind_bmp_request_state {
    struct intermon_msg_queue_elem elem;
    struct intermon_bind_bmp_request__args args;
    struct monitor_binding *st;
    uintptr_t domain_id;
};

static void bind_bmp_request_cont(struct intermon_binding *mon_closure,
                                  iref_t iref, uintptr_t conn_id,
                                  uint32_t chanid, uint32_t eplen,
                                  struct monitor_binding *st,
                                  uintptr_t domain_id)
{
    errval_t err;

    /* Send the request to the monitor on the server's core */
    err = mon_closure->tx_vtbl.
        bind_bmp_request(mon_closure, NOP_CONT, iref, conn_id, chanid, eplen);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_bmp_request_state *me =
                malloc(sizeof(struct bind_bmp_request_state));
            struct intermon_state *ist = mon_closure->st;
            me->args.iref = iref;
            me->args.mon_id = conn_id;
            me->args.chanid = chanid;
            me->args.ep_len = eplen;
            me->st = st;
            me->domain_id = domain_id;
            me->elem.cont = bind_bmp_request_handler;

            err = intermon_enqueue_send(mon_closure, &ist->queue,
                                        get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        DEBUG_ERR(err, "failed forwarding BMP bind request");
        monitor_bind_bmp_client_request_error(st, conn_id, domain_id, err);
    }
}

static void bind_bmp_request_handler(struct intermon_binding *b,
                                     struct intermon_msg_queue_elem *e)
{
    struct bind_bmp_request_state *st = (struct bind_bmp_request_state *)e;
    bind_bmp_request_cont(b, st->args.iref, st->args.mon_id, st->args.chanid,
                          st->args.ep_len, st->st, st->domain_id);
    free(e);
}

static void monitor_bind_bmp_client_request(struct monitor_binding *st,
                                            iref_t iref, uintptr_t domain_id,
                                            struct capref lmp_ep)
{
    uint8_t core_id;
    uintptr_t conn_id = 0;
    errval_t err;
    struct remote_conn_state *conn = NULL;

    // Get the core id
    err = iref_get_core_id(iref, &core_id);
    if (err_is_fail(err)) {
        cap_destroy(lmp_ep);
        monitor_bind_bmp_client_request_error(st, conn_id, domain_id, err);
        return;
    }

    if (core_id == my_core_id) {
        cap_destroy(lmp_ep);
        err = MON_ERR_SAME_CORE;
        monitor_bind_bmp_client_request_error(st, conn_id, domain_id, err);
        return;
    }

    // Identify endpoint cap
    struct capability capability;
    err = monitor_cap_identify(lmp_ep, &capability);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "monitor_cap_identify failed, ignored");
        return; // XXX
    }
    assert(capability.type == ObjType_EndPoint); // XXX: shouldn't assert

    // Allocate Beehive endpoint locally
    int chanid;
    err = beehive_chan_allocate(lmp_ep, &chanid);
    assert(err_is_ok(err)); // XXX

    /* destroy our copy of the endpoint, we don't need it any more */
    errval_t err2 = cap_destroy(lmp_ep);
    if (err_is_fail(err2)) {
        DEBUG_ERR(err, "cap_destroy failed");
    }

    /* Forward request to the corresponding monitor */
    // Create local state
    err = remote_conn_alloc(&conn, &conn_id, REMOTE_CONN_BMP);
    if (err_is_fail(err)) {
        monitor_bind_bmp_client_request_error(st, conn_id, domain_id, err);
        return;
    }

    // Track data
    conn->domain_id = domain_id;
    conn->domain_closure = st;

    // Get connection to the monitor to forward request to
    struct intermon_binding *mon_closure;
    err = intern_get_closure(core_id, &mon_closure);
    if (err_is_fail(err)) {
        monitor_bind_bmp_client_request_error(st, conn_id, domain_id, err);
        return;
    }

    bind_bmp_request_cont(mon_closure, iref, conn_id, chanid,
                          capability.u.endpoint.epbuflen, st, domain_id);
}

/******* stack-ripped monitor_bind_bmp_reply *******/

static void bind_bmp_reply_handler(struct intermon_binding *b,
                                     struct intermon_msg_queue_elem *e);

struct bind_bmp_reply_state {
    struct intermon_msg_queue_elem elem;
    struct intermon_bind_bmp_reply__args args;
    struct capability capability;
};

static void bind_bmp_reply_cont(struct intermon_binding *mon_closure,
                                uintptr_t your_mon_id, uintptr_t my_mon_id,
                                uintptr_t msgerr, uint32_t chanid, uint32_t eplen)
{
    errval_t err;

    err = mon_closure->tx_vtbl.
        bind_bmp_reply(mon_closure, NOP_CONT, your_mon_id, my_mon_id, msgerr,
                       chanid, eplen);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_bmp_reply_state *me =
                malloc(sizeof(struct bind_bmp_reply_state));
            struct intermon_state *ist = mon_closure->st;
            me->args.con_id = your_mon_id;
            me->args.mon_id = my_mon_id;
            me->args.err = msgerr;
            me->args.chanid = chanid;
            me->args.ep_len = eplen;
            me->elem.cont = bind_bmp_reply_handler;

            err = intermon_enqueue_send(mon_closure, &ist->queue,
                                        get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        DEBUG_ERR(err, "failed forwarding BMP bind reply");
        // cleanup
        if (err_is_ok(msgerr)) {
            err = remote_conn_free(my_mon_id);
            assert(err_is_ok(err));
        }
    }
}

static void bind_bmp_reply_handler(struct intermon_binding *b,
                                   struct intermon_msg_queue_elem *e)
{
    struct bind_bmp_reply_state *st = (struct bind_bmp_reply_state *)e;
    bind_bmp_reply_cont(b, st->args.con_id, st->args.mon_id, st->args.err,
                        st->args.chanid, st->args.ep_len);
    free(e);
}

static void monitor_bind_bmp_reply(struct monitor_binding *dom_closure,
                                   uintptr_t my_mon_id, uintptr_t domain_id,
                                   struct capref lmp_ep, errval_t msgerr)
{
    errval_t err;

    struct remote_conn_state *conn = remote_conn_lookup(my_mon_id);
    if (conn == NULL) {
        DEBUG_ERR(0, "invalid mon_id in BMP bind reply");
        return;
    }

    uintptr_t your_mon_id = conn->mon_id;
    struct intermon_binding *mon_closure = conn->mon_closure;

    if (err_is_ok(msgerr)) {
        /* Connection accepted */
        conn->domain_id = domain_id;
        conn->domain_closure = dom_closure;
    } else {
//error:
        err = remote_conn_free(my_mon_id);
        assert(err_is_ok(err));
    }

    // Identify endpoint cap
    struct capability capability;
    err = monitor_cap_identify(lmp_ep, &capability);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "monitor_cap_identify failed, ignored");
        return;
    }
    assert(capability.type == ObjType_EndPoint); // XXX: shouldn't assert

    // Allocate Beehive endpoint locally
    int chanid;
    err = beehive_chan_allocate(lmp_ep, &chanid);
    assert(err_is_ok(err)); // XXX

    /* destroy our copy of the endpoint, we don't need it any more */
    errval_t err2 = cap_destroy(lmp_ep);
    if (err_is_fail(err2)) {
        DEBUG_ERR(err, "cap_destroy failed");
    }

    bind_bmp_reply_cont(mon_closure, your_mon_id, my_mon_id, msgerr, chanid,
                        capability.u.endpoint.epbuflen);
}

/******* stack-ripped intermon_bind_bmp_request *******/

static void bind_bmp_service_request_handler(struct monitor_binding *b,
                                             struct monitor_msg_queue_elem *e);

struct bind_bmp_service_request_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_bind_bmp_service_request__args args;
    struct intermon_binding *closure;
    uintptr_t your_mon_id;
};

static void bind_bmp_service_request_cont(struct monitor_binding *domain_closure,
                                          uintptr_t service_id,
                                          con_id_t my_mon_id,
                                          struct capref bee_ep,
                                          uint32_t ep_len,
                                          struct intermon_binding *closure,
                                          con_id_t your_mon_id)
{
    errval_t err, err2;

    /* Proxy the request */
    err = domain_closure->tx_vtbl.
        bind_bmp_service_request(domain_closure, NOP_CONT, service_id,
                                 my_mon_id, bee_ep, ep_len);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_bmp_service_request_state *me =
                malloc(sizeof(struct bind_bmp_service_request_state));
            struct monitor_state *ist = domain_closure->st;
            me->args.service_id = service_id;
            me->args.mon_id = my_mon_id;
            me->args.bee_ep = bee_ep;
            me->args.remote_ep_len = ep_len;
            me->closure = closure;
            me->your_mon_id = your_mon_id;
            me->elem.cont = bind_bmp_service_request_handler;

            err = monitor_enqueue_send(domain_closure, &ist->queue,
                                       get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        err2 = cap_destroy(bee_ep);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err2, "cap_destroy failed");
        }
        err2 = remote_conn_free(my_mon_id);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err2, "remote_conn_free failed");
        }
        err2 = closure->tx_vtbl.bind_bmp_reply(closure, NOP_CONT, your_mon_id, 0,
                                               err, 0, 0);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err2, "Sending bind_bmp_reply failed");
        }
    }
}

static void bind_bmp_service_request_handler(struct monitor_binding *b,
                                             struct monitor_msg_queue_elem *e)
{
    struct bind_bmp_service_request_state *st = (struct bind_bmp_service_request_state *)e;
    bind_bmp_service_request_cont(b, st->args.service_id, st->args.mon_id,
                                  st->args.bee_ep, st->args.remote_ep_len,
                                  st->closure, st->your_mon_id);
    free(e);
}

static void intermon_bind_bmp_request(struct intermon_binding *binding,
                                      iref_t iref, con_id_t your_mon_id,
                                      uint32_t chanid, uint32_t eplen)
{
    errval_t err;

    /* Get client's core_id from the closure */
    coreid_t core_id = 0;
    err = intern_get_core_id(closure, &core_id);
    assert(err_is_ok(err));

    // Construct the beehive ep cap
    struct capref bee_ep;
    err = beehive_create_cap(core_id, chanid, &bee_ep);
    assert(err_is_ok(err)); // XXX

    /* Get the server's connection */
    struct monitor_binding *domain_binding = NULL;
    err = iref_get_binding(iref, &domain_binding);
    assert(err_is_ok(err));

    /* Get the service id */
    uintptr_t service_id = 0;
    err = iref_get_service_id(iref, &service_id);
    assert(err_is_ok(err));

    /* Create a new connection state */
    uintptr_t my_mon_id;
    struct remote_conn_state *con;
    err = remote_conn_alloc(&con, &my_mon_id, REMOTE_CONN_BMP);
    assert(err_is_ok(err));

    // Set the monitor portion of it
    con->mon_id = your_mon_id;
    con->mon_closure = binding;

    bind_bmp_service_request_cont(domain_binding, service_id, my_mon_id,
                                  bee_ep, eplen, binding, your_mon_id);
}

/******* stack-ripped intermon_bind_bmp_reply *******/

static void bind_bmp_reply_client_handler(struct monitor_binding *b,
                                          struct monitor_msg_queue_elem *e);

struct bind_bmp_reply_client_state {
    struct monitor_msg_queue_elem elem;
    struct monitor_bind_bmp_reply_client__args args;
};

static void bind_bmp_reply_client_cont(struct monitor_binding *domain_closure,
                                       uintptr_t my_mon_id, uintptr_t domain_id,
                                       struct capref bee_ep, uint32_t eplen,
                                       errval_t msgerr)
{
    errval_t err;

    err = domain_closure->tx_vtbl.
        bind_bmp_reply_client(domain_closure, NOP_CONT, my_mon_id, domain_id,
                              bee_ep, eplen, msgerr);
    if (err_is_fail(err)) {
        if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct bind_bmp_reply_client_state *me =
                malloc(sizeof(struct bind_bmp_reply_client_state));
            struct monitor_state *ist = domain_closure->st;
            me->args.mon_id = my_mon_id;
            me->args.conn_id = domain_id;
            me->args.bee_ep = bee_ep;
            me->args.remote_ep_len = eplen;
            me->args.err = msgerr;
            me->elem.cont = bind_bmp_reply_client_handler;

            err = monitor_enqueue_send(domain_closure, &ist->queue,
                                       get_default_waitset(), &me->elem.queue);
            assert(err_is_ok(err));
            return;
        }

        DEBUG_ERR(err, "BMP bind reply failed");
        // cleanup
        err = remote_conn_free(my_mon_id);
        assert(err_is_ok(err));
    }
}

static void bind_bmp_reply_client_handler(struct monitor_binding *b,
                                             struct monitor_msg_queue_elem *e)
{
    struct bind_bmp_reply_client_state *st = (struct bind_bmp_reply_client_state *)e;
    bind_bmp_reply_client_cont(b, st->args.mon_id, st->args.conn_id,
                               st->args.bee_ep, st->args.remote_ep_len,
                               st->args.err);
    free(e);
}

static void intermon_bind_bmp_reply(struct intermon_binding *closure, 
                                    con_id_t my_mon_id, con_id_t your_mon_id,
                                    errval_t msgerr, uint32_t chanid,
                                    uint32_t eplen)
{
    errval_t err;
    struct remote_conn_state *con = remote_conn_lookup(my_mon_id);
    if (con == NULL) {
        DEBUG_ERR(0, "unknown mon_id in BMP bind reply");
        return;
    }

    uintptr_t domain_id = con->domain_id;
    struct monitor_binding *domain_closure = con->domain_closure;
    struct capref bee_ep = NULL_CAP;

    if (err_is_ok(msgerr)) { /* bind succeeded */
        con->mon_id = your_mon_id;
        con->mon_closure = closure;

        // Get core id of sender
        coreid_t core_id = 0;
        err = intern_get_core_id(closure, &core_id);
        assert(err_is_ok(err));

        // Construct the beehive ep cap
        err = beehive_create_cap(core_id, chanid, &bee_ep);
        assert(err_is_ok(err)); // XXX
    } else { /* bind refused */
        err = remote_conn_free(my_mon_id);
        assert(err_is_ok(err));
    }

    bind_bmp_reply_client_cont(domain_closure, my_mon_id, domain_id, bee_ep,
                               eplen, msgerr);
}

errval_t bmp_intermon_init(struct intermon_binding *ib)
{
    ib->rx_vtbl.bind_bmp_request = intermon_bind_bmp_request;
    ib->rx_vtbl.bind_bmp_reply = intermon_bind_bmp_reply;
    return SYS_ERR_OK;
}

errval_t bmp_monitor_init(struct monitor_binding *mb)
{
    mb->rx_vtbl.bind_bmp_client_request = monitor_bind_bmp_client_request;
    mb->rx_vtbl.bind_bmp_reply_monitor = monitor_bind_bmp_reply;
    return SYS_ERR_OK;
}
