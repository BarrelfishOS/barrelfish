/**
 * \file
 * \brief Forwards bulk_transfer traffic
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_sm.h>

// ------------------------------------------------------------------------
// Global State
// ------------------------------------------------------------------------

enum forward_state {
    STARTUP,
    BIND_BOUND,
    EXPORT_BOUND,
    RUNNING,
    UNREACHABLE,
    ERROR,
};
volatile enum forward_state state;

enum bulk_trust_level fw_trust;

const uint8_t       bind = 0;
char                *bind_name;
struct waitset      bind_waitset;
struct bulk_channel bind_channel;

const uint8_t       export = 1;
char                *export_name;
struct waitset      export_waitset;
struct bulk_channel export_channel;

/**
 * Wait for given state and dispatch waitsets on the go.
 */
static void wait_state_or_error(enum forward_state cond)
{
    struct bulk_sm_ws_item ws_list[3];

    ws_list[0].ws   = get_default_waitset();
    ws_list[1].ws   = &bind_waitset;
    ws_list[2].ws   = &export_waitset;
    ws_list[0].next = &ws_list[1];
    ws_list[1].next = &ws_list[2];
    ws_list[2].next = NULL;

    while (state != cond && state != ERROR) {
        errval_t err = bulk_sm_multiple_event_dispatch(ws_list);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "wait_state: event_dispach");
        }
    }
}

/**
 * Wait for boolean to become true.
 */
static void wait_cond(volatile bool *cond)
{
    struct bulk_sm_ws_item ws_list[3];

    ws_list[0].ws   = get_default_waitset();
    ws_list[1].ws   = &bind_waitset;
    ws_list[2].ws   = &export_waitset;
    ws_list[0].next = &ws_list[1];
    ws_list[1].next = &ws_list[2];
    ws_list[2].next = NULL;

    while (!*cond) {
        errval_t err = bulk_sm_multiple_event_dispatch(ws_list);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "wait_cond: event_dispach");
        }
    }
}

// ------------------------------------------------------------------------
// Traffic Passthrough Helper Functions
// ------------------------------------------------------------------------

static inline void wait_enabled(void)
{
    if (state != RUNNING) {
        wait_state_or_error(RUNNING);
    }
}

static inline struct bulk_channel *other_channel(struct bulk_channel *channel)
{
    if (channel == &bind_channel) {
        return &export_channel;
    } else {
        return &bind_channel;
    }
}

/**
 * Holds reply for future response.
 */
struct future_reply {
    volatile bool valid;
    errval_t      err;
};

#define REPLY_INIT ((struct future_reply) { false, SYS_ERR_OK })

/**
 * Fill struct future_reply with result.
 */
static void fill_reply_cont(void *arg, errval_t err,
                            struct bulk_channel *channel)
{
    struct future_reply *r = arg;
    r->err   = err;
    r->valid = true;
}

/**
 * Wait for a reply to be filled and return error code.
 */
static errval_t wait_reply(struct future_reply *r)
{
    wait_cond(&r->valid);
    return r->err;
}

// ------------------------------------------------------------------------
// Traffic Passthrough Callbacks
// ------------------------------------------------------------------------

/**
 * The other endpoint requests to assign a new pool to this channel.
 * @return If an error value is returned, the pool is not assigned and the
 *         error code is sent to the other side (veto).
 */
static errval_t pool_assigned_fw(struct bulk_channel *channel,
                                 struct bulk_pool    *pool)
{
    wait_enabled();

    struct bulk_channel *other = other_channel(channel);

    struct future_reply r = REPLY_INIT;
    errval_t err =  bulk_channel_assign_pool(other, pool,
            MK_BULK_CONT(fill_reply_cont, &r));

    if (err_is_fail(err)) {
        return err;
    } else {
        return wait_reply(&r);
    }
}

/**
 * The other endpoint wants to remove a pool from this channel
 */
static errval_t pool_removed_fw(struct bulk_channel *channel,
                                struct bulk_pool    *pool)
{
    wait_enabled();

    struct bulk_channel *other = other_channel(channel);

    struct future_reply r = REPLY_INIT;
    errval_t err =  bulk_channel_remove_pool(other, pool,
            MK_BULK_CONT(fill_reply_cont, &r));

    if (err_is_fail(err)) {
        return err;
    } else {
        return wait_reply(&r);
    }
}

/** Incoming moved buffer (sink) */
static void move_received_fw(struct bulk_channel *channel,
                             struct bulk_buffer  *buffer,
                             void                *meta)
{
    wait_enabled();

    struct bulk_channel *other = other_channel(channel);

    struct future_reply r = REPLY_INIT;
    errval_t err =  bulk_channel_move(other, buffer, meta,
            MK_BULK_CONT(fill_reply_cont, &r));

    if (err_is_ok(err)) {
        wait_reply(&r);
    }
}

/** Incoming passed buffer (source) */
static void buffer_received_fw(struct bulk_channel *channel,
                               struct bulk_buffer  *buffer,
                               void                *meta)
{
    wait_enabled();

    struct bulk_channel *other = other_channel(channel);

    struct future_reply r = REPLY_INIT;
    errval_t err =  bulk_channel_pass(other, buffer, meta,
            MK_BULK_CONT(fill_reply_cont, &r));

    if (err_is_ok(err)) {
        wait_reply(&r);
    }
}

/** Incoming copied buffer (sink) */
static void copy_received_fw(struct bulk_channel *channel,
                             struct bulk_buffer  *buffer,
                             void                *meta)
{
    wait_enabled();

    struct bulk_channel *other = other_channel(channel);

    struct future_reply r = REPLY_INIT;
    errval_t err =  bulk_channel_copy(other, buffer, meta,
            MK_BULK_CONT(fill_reply_cont, &r));

    if (err_is_ok(err)) {
        wait_reply(&r);
    }
}

/** Released copied buffer (source) */
static void copy_released_fw(struct bulk_channel *channel,
                             struct bulk_buffer  *buffer)
{
    wait_enabled();

    struct bulk_channel *other = other_channel(channel);

    struct future_reply r = REPLY_INIT;
    errval_t err =  bulk_channel_release(other, buffer,
            MK_BULK_CONT(fill_reply_cont, &r));

    if (err_is_ok(err)) {
        wait_reply(&r);
    }
}

// ------------------------------------------------------------------------
// Communication Setup
// ------------------------------------------------------------------------

static errval_t export_bound(struct bulk_channel *channel)
{
    assert(state == BIND_BOUND);
    state = EXPORT_BOUND;
    return SYS_ERR_OK;
}

static void bind_bound_cont(void *arg, errval_t err,
                            struct bulk_channel *channel)
{
    assert(state == STARTUP);

    if (err_is_ok(err)) {
        state = BIND_BOUND;
    } else {
        DEBUG_ERR(err, "bind continuation");
        state = ERROR;
    }
}

struct bulk_channel_callbacks bind_callbacks = {
    .bind_received     = NULL,
    .teardown_received = NULL,
    .pool_assigned     = pool_assigned_fw,
    .pool_removed      = pool_removed_fw,
    .move_received     = move_received_fw,
    .buffer_received   = buffer_received_fw,
    .copy_received     = copy_received_fw,
    .copy_released     = copy_released_fw,
};

struct bulk_channel_callbacks export_callbacks = {
    .bind_received     = export_bound,
    .teardown_received = NULL,
    .pool_assigned     = pool_assigned_fw,
    .pool_removed      = pool_removed_fw,
    .move_received     = move_received_fw,
    .buffer_received   = buffer_received_fw,
    .copy_received     = copy_received_fw,
    .copy_released     = copy_released_fw,
};

/**
 * This application takes two commandline arguments. It binds to the bulk
 * transfer interface given as first argument. It exports a bulk transfer
 * interface as given in the second argument. Traffic is mediated between
 * the two ports.
 */
int main(int argc, char *argv[])
{
    if (argc != 4) {
        debug_printf("bulk_transfer passthrough\n");
        debug_printf("  Usage: %s bind export trust\n", argv[0]);
        return EXIT_FAILURE;
    }

    bind_name   = argv[1];
    export_name = argv[2];
    fw_trust    = ((argv[3][0] == '0') ? BULK_TRUST_NONE : BULK_TRUST_FULL);

    debug_printf("bulk_transfer passthrough: bind=%s, export=%s trust=%s\n",
            bind_name, export_name,
            (fw_trust==BULK_TRUST_NONE ? "NONE" : "FULL"));

    errval_t err;

    // Data Initializtaion ------------------------------------------------
    state = STARTUP;
    memset(&bind_channel, 0, sizeof(bind_channel));
    memset(&export_channel, 0, sizeof(bind_channel));
    waitset_init(&bind_waitset);
    waitset_init(&export_waitset);

    // Bind ---------------------------------------------------------------
    debug_printf("Connecting to %s...\n", bind_name);

    iref_t bind_iref;
    err = nameservice_blocking_lookup(bind_name, &bind_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind: nameservice_blocking_lookup");
    }

    struct bulk_sm_endpoint_descriptor bind_ep;
    err = bulk_sm_ep_create_remote(&bind_ep, bind_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind: bulk_sm_ep_create_remote");
    }

    struct bulk_channel_bind_params bind_params = {
        .role    = BULK_ROLE_GENERIC,
        .trust   = fw_trust,
        .waitset = &bind_waitset
    };
    err = bulk_channel_bind(&bind_channel,
                            (struct bulk_endpoint_descriptor*)&bind_ep,
                            &bind_callbacks, &bind_params,
                            MK_BULK_CONT(bind_bound_cont, NULL));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind: bulk_channel_bind");
    }

    wait_state_or_error(BIND_BOUND);
    if (state == ERROR) {
        debug_printf("Error during bind. Exiting.\n");
        return EXIT_FAILURE;
    }

    debug_printf("Bound to %s.\n", bind_name);

    // Export -------------------------------------------------------------
    debug_printf("Exporting port to %s...\n", export_name);

    struct bulk_sm_endpoint_descriptor export_ep;
    err = bulk_sm_ep_create(&export_ep);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export: bulk_sm_ep_create");
    }

    enum bulk_channel_direction export_direction = BULK_DIRECTION_TX;
    if (bind_channel.direction == BULK_DIRECTION_TX) {
        export_direction = BULK_DIRECTION_RX;
    }

    enum bulk_channel_role export_role = BULK_ROLE_MASTER;
    if (bind_channel.role == BULK_ROLE_MASTER) {
        export_role = BULK_ROLE_SLAVE;
    }

    struct bulk_channel_setup export_setup = {
        .direction   = export_direction,
        .role        = export_role,
        .trust       = fw_trust,
        .constraints = bind_channel.constraints,
        .meta_size   = bind_channel.meta_size,
        .waitset     = &export_waitset,
        .user_state  = NULL
    };

    err = bulk_channel_create(&export_channel,
                              (struct bulk_endpoint_descriptor*)&export_ep,
                              &export_callbacks, &export_setup);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export: bulk_sm_ep_create");
    }

    err = nameservice_register(export_name, export_ep.iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export: nameservice_register");
    }

    debug_printf("Exported %s.\n", export_name);

    // Wait for domain to connect on exported port ------------------------
    debug_printf("Waiting for connection on %s...\n", export_name);

    wait_state_or_error(EXPORT_BOUND);
    if (state == ERROR) {
        debug_printf("Error during export. Exiting.\n");
        return EXIT_FAILURE;
    }

    debug_printf("Connected on %s.\n", export_name);

    // Forward messages ---------------------------------------------------
    debug_printf("Connection %s <-> %s established. Enabeling passthrough.\n",
            bind_name, export_name);

    assert(state == EXPORT_BOUND);
    state = RUNNING;

    wait_state_or_error(UNREACHABLE);

    return EXIT_SUCCESS;
}
