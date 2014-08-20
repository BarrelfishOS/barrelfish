/**
 * \file
 * \brief block_server client process.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_sm.h>

#include "bs_connector.h"
#include "benchmark.h"

enum bs_connect_stage
{
    CSTAGE_START,
    CSTAGE_SERVICE_BOUND,
    CSTAGE_SERVICE_SETUP,
    CSTAGE_ERR
};

static uint32_t seq_number = 0;

volatile enum bs_connect_stage connection_stage = CSTAGE_START;

struct waitset *bs_service_ws    = NULL;
struct waitset *bs_service_tx_ws = NULL;
struct waitset *bs_service_rx_ws = NULL;

static inline void wait_for_condition(enum bs_connect_stage stage)
{
    struct bulk_sm_ws_item ws_list[3];

    ws_list[0].ws   = bs_service_ws;
    ws_list[1].ws   = bs_service_tx_ws;
    ws_list[2].ws   = bs_service_rx_ws;
    ws_list[0].next = &ws_list[1];
    ws_list[1].next = &ws_list[2];
    ws_list[2].next = NULL;

    while (connection_stage != stage) {
        errval_t err = bulk_sm_multiple_event_dispatch(ws_list);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "wait_for_condition: event_dispach");
        }
    }
}


static void bs_service_status_cb(struct block_service_binding *b,
                                 errval_t err,
                                 uint32_t seqn,
                                 uint32_t req)
{
    BS_CONN_DEBUG("status message: error=%s, seqn=%u, req=%u",
            err_getstring(err), seqn, req);
    bench_signal(err, seqn, req);
}

static struct block_service_rx_vtbl bs_rx_vtbl = {
    .status = bs_service_status_cb };

static void bs_service_bind_cb(void *st,
                               errval_t err,
                               struct block_service_binding *b)
{
    struct bs_connection *conn = (struct bs_connection *) st;

    b->st = conn;
    conn->service = b;

    conn->state = BS_CONN_SERVICE_BOUND;

    b->rx_vtbl = bs_rx_vtbl;

    connection_stage = CSTAGE_SERVICE_BOUND;
}

static void bs_service_setup_cb(void *a)
{
    /* DUMMY */
}

static errval_t bs_bulk_bind_cb(struct bulk_channel *channel)
{
    struct bs_connection *conn = (struct bs_connection *) channel->user_state;
    if (conn->state == BS_CONN_SERVICE_BOUND) {
        BS_CONN_DEBUG("%s", "first bulk channel bound.");
        conn->state = BS_CONN_BULK_BINDING;
        return SYS_ERR_OK;
    } else if (conn->state == BS_CONN_BULK_BINDING) {
        BS_CONN_DEBUG("%s", "second channel bound.");
        conn->state = BS_CONN_CONNECTED;
        connection_stage = CSTAGE_SERVICE_SETUP;
        return SYS_ERR_OK;
    }
    assert(!"This should not happen...");
    return SYS_ERR_OK;
}

static void bs_service_setup(void *st)
{
    errval_t err;
    BS_CONN_DEBUG("sending setup message to %s.", BLOCK_SERVICE_NAME);

    struct bs_connection *conn = (struct bs_connection *) st;

    struct event_closure txcont = MKCONT(bs_service_setup_cb, conn);

    BS_CONN_DEBUG("tx_iref=%i, rx_iref=%i", (uint32_t )conn->tx_ep.iref,
                  (uint32_t )conn->rx_ep.iref);

    err = block_service_setup__tx(conn->service, txcont, conn->rx_ep.iref,
                                  conn->tx_ep.iref);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(bs_service_setup, conn);
            struct waitset *ws = get_default_waitset();
            err = conn->service->register_send(conn->service, ws, txcont);
            if (err_is_fail(err)) {
                connection_stage = CSTAGE_ERR;
                conn->state = BS_CONN_ERR;
                USER_PANIC_ERR(err, "could not register send\n");
            }
        } else {
            BS_CONN_DEBUG("sending setup message %s", BLOCK_SERVICE_NAME);
            connection_stage = CSTAGE_ERR;
            conn->state = BS_CONN_ERR;
            USER_PANIC_ERR(err, "flounder failure\n");
        }
    }
}

errval_t bs_service_connect(struct bs_connection *conn,
                            struct bulk_channel_callbacks *rx_cb,
                            struct bulk_channel_callbacks *tx_cb)
{
    errval_t err;
    iref_t service_iref;

    BS_CONN_DEBUG("looking up service: %s", BLOCK_SERVICE_NAME);
    /* connect to the block service */
    err = nameservice_blocking_lookup(BLOCK_SERVICE_NAME, &service_iref);
    if (err_is_fail(err)) {
        return err;
    }

    BS_CONN_DEBUG("binding to iref %i", (uint32_t )service_iref);
    err = block_service_bind(service_iref, bs_service_bind_cb, conn,
                             get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    bs_service_ws = get_default_waitset();

    wait_for_condition(CSTAGE_SERVICE_BOUND);

    BS_CONN_DEBUG("%s", "setting up bulk endpoints and channels");

    /* setting up bulk endpoints */
    err = bulk_sm_ep_create(&conn->rx_ep);
    assert(!err_is_fail(err));
    err = bulk_sm_ep_create(&conn->tx_ep);
    assert(!err_is_fail(err));

    // need exclusive waitset per channel
    struct waitset *rx_ws = malloc(sizeof(*rx_ws));
    struct waitset *tx_ws = malloc(sizeof(*tx_ws));
    assert(rx_ws);
    assert(tx_ws);
    waitset_init(rx_ws);
    waitset_init(tx_ws);

    bs_service_tx_ws = tx_ws;
    bs_service_rx_ws = rx_ws;

    struct bulk_channel_setup setup = {
        .role = BULK_ROLE_MASTER,
        .trust = BULK_TRUST_FULL,
        .direction = BULK_DIRECTION_RX,
        .meta_size = sizeof(struct bs_meta_data),
        .waitset = rx_ws,
        .user_state = conn };

    rx_cb->bind_received = bs_bulk_bind_cb;

    err = bulk_channel_create(&conn->rx_channel,
                              (struct bulk_endpoint_descriptor *) &conn->rx_ep,
                              rx_cb, &setup);
    if (err_is_fail(err)) {
        return err;
    }

    setup.direction = BULK_DIRECTION_TX;
    setup.waitset   = tx_ws;

    tx_cb->bind_received = bs_bulk_bind_cb;

    err = bulk_channel_create(&conn->tx_channel,
                              (struct bulk_endpoint_descriptor *) &conn->tx_ep,
                              tx_cb, &setup);
    if (err_is_fail(err)) {
        return err;
    }

    bs_service_setup(conn);

    wait_for_condition(CSTAGE_SERVICE_SETUP);

    BS_CONN_DEBUG("%s", "Service and bulk channels setup.");

    return SYS_ERR_OK;
}

struct bs_read_request {
    struct bs_connection *conn;
    uint32_t block_id;
    uint32_t block_count;
    struct bulk_continuation cont;
};

static void bs_service_read_sent_cb(void *a)
{
    free(a);
}

static errval_t bs_service_read_send(void *a)
{
    errval_t err;

    struct bs_read_request *req = (struct bs_read_request*)a;

    struct event_closure txcont = MKCONT(bs_service_read_sent_cb, req);
    err = block_service_read__tx(req->conn->service, txcont, req->block_id, req->block_count, seq_number);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            // cannot do this: would need to register multiple resend functions.
            // struct waitset *ws = get_default_waitset();
            // txcont = MKCONT(bs_service_read_send, req);
            // err = req->conn->service->register_send(req->conn->service, ws, txcont);
            // if (err_is_fail(err)) {
            //     // note that only one continuation may be registered at a time
            //     DEBUG_ERR(err, "register_send on binding failed!");
            // }
        }
    } else {
        seq_number++;
    }

    return err;
}

errval_t bs_service_read(struct bs_connection *conn,
                         uint32_t block_id,
                         uint32_t block_count,
                         struct bulk_continuation cont)
{
    struct bs_read_request *req = malloc(sizeof(struct bs_read_request));

    req->block_count = block_count;
    req->conn = conn;
    req->block_id = block_id;
    req->cont = cont;

    errval_t err;
    do { // busy wait because no send queue available.
        err = bs_service_read_send(req);
        if (err_is_fail(err) && err != FLOUNDER_ERR_TX_BUSY) {
            USER_PANIC_ERR(err, "bs_service_read");
        }

        errval_t other = event_dispatch(get_default_waitset());
        // maybe need dispatch service->tx/rx_channel.waitset as well?
        assert(err_is_ok(other));
    } while (err_is_fail(err));

    return SYS_ERR_OK;
}
