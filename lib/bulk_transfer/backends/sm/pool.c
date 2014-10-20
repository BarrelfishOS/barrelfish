/*
 * Copyright (c) 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_sm.h>

//#include "../../helpers.h"
#include "../../bulk_pool.h"
#include "bulk_sm_impl.h"

// Pool assignment --------------------------------------------------------

/**
 * Data to identify this pool_assign transaction on RPC client side.
 */
struct c_assign_pool_data {
    struct bulk_channel      *channel;      ///< channel under consideration
    struct bulk_pool         *pool;         ///< pool to be added to channel
    struct bulk_continuation continuation;  ///< continuation after completion
    bulk_ctrl_pool_t         flounder_pool; ///< flounder representation of pool
};

/**
 * Data to identify this pool_assign transaction on RPC server side.
 */
struct s_assign_pool_data {
    struct bulk_channel *channel;
    uint64_t            id;
    errval_t            err;
};

/**
 * Reply handler for pool_assign RPC call. On success, pool is added to channel.
 */
void bulk_sm_assign_pool_rx_response(
        struct bulk_ctrl_binding *b,
        uint64_t                 error,
        uint64_t                 id)
{
    struct bulk_channel *channel     = VOID2CHANNEL(b->st);
    struct c_assign_pool_data *cdata = (struct c_assign_pool_data*) id;

    // if accepted, add pool to channel
    errval_t r_error = (errval_t) error;
    if (err_is_ok(r_error)) {
        errval_t err = bulk_pool_assign(cdata->pool, channel);
        assert(err_is_ok(err));
    }

    // cleanup storage
    struct bulk_continuation cont = cdata->continuation;
    free(cdata);

    // notify user
    bulk_continuation_call(cont, r_error, channel);
}

/**
 * Reply handler for pool_assign RPC call.
 */

static void bulk_sm_assign_pool_reply_sent(void *a)
{
    struct s_assign_pool_data *sdata = a;
    free(sdata);
}

static errval_t bulk_sm_assign_pool_send_reply(void *a)
{
    struct s_assign_pool_data *sdata   = a;
    struct bulk_channel       *channel = sdata->channel;
    struct bulk_ctrl_binding  *b       = CHANNEL_BINDING(channel);

    struct event_closure txcont = MKCONT(bulk_sm_assign_pool_reply_sent, sdata);

    errval_t err = bulk_ctrl_assign_pool_response__tx(b, txcont,
            sdata->err, sdata->id);

    return err;
}

/**
 * Receive handler for pool_assign RPC call on peer side. Asks user for
 * confirmation of pool and replies to the original side.
 */
void bulk_sm_assign_pool_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_pool_t         pool,
        uint64_t                 id)
{
    struct bulk_channel *channel = VOID2CHANNEL(b->st);
    errval_t err;

    // Allocte transaction data
    struct s_assign_pool_data *sdata =
        malloc(sizeof(struct s_assign_pool_data));
    if (!sdata) {
        // not much we can do now.
        USER_PANIC("No memory to serve assign_pool request.\n");
    }

    sdata->channel = channel;
    sdata->id      = id;

    // Create library representation from pool information recevied by flounder.
    struct bulk_pool *new_pool;

    //first look up if we already know this pool from some other channel
    bool first_assignment = 0;
    struct bulk_pool_id pool_id = {
        .machine = pool.pool_id_machine,
        .dom     = pool.pool_id_dom,
        .local   = pool.pool_id_local
    };
    new_pool = bulk_pool_domain_list_get(&pool_id);

    if (new_pool != NULL){
        //we already know that pool -> do nothing
    } else {
        first_assignment = 1;
        err = create_pool_from_flounder(&new_pool, &pool);
        if (err_is_fail(err)) {
            sdata->err = err;
            bulk_sm_assign_pool_send_reply(sdata);
        }
        //map the pool into our vspace
        err = bulk_pool_map(new_pool);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "bulk_pool_map failed in bulk_sm_assign_pool_rx_call");
            debug_printf("err: %s\n", err_getstring(err));
            sdata->err = err_push(err, BULK_TRANSFER_POOL_MAP);
            bulk_sm_assign_pool_send_reply(sdata);
        }
    }


    // request permission to assign pool from application
    assert(channel->callbacks->pool_assigned);

    err = channel->callbacks->pool_assigned(channel, new_pool);
    if (err_is_fail(err)) {
        // application doesn't accept this pool. that's fine.
        sdata->err = err;
        if (first_assignment){
            err = bulk_pool_unmap(new_pool);
            err = bulk_pool_dealloc(new_pool);
            bulk_pool_domain_list_remove(new_pool);
        }

    } else {
        sdata->err = SYS_ERR_OK;

        err = bulk_pool_assign(new_pool, channel);
        assert(err_is_ok(err));
        if (first_assignment){
            err = bulk_pool_domain_list_insert(new_pool);
            assert(err_is_ok(err));
        }
    }

    // done
    bulk_sm_flounder_send_fifo_msg_with_arg(channel,
                                            bulk_sm_assign_pool_send_reply,
                                            sdata);
}

/**
 * Send handler for pool_assign RPC call.
 */
static errval_t bulk_sm_assign_pool_send_request(void *a)
{
    struct c_assign_pool_data  *cdata   = a;
    struct bulk_channel        *channel = cdata->channel;
    struct bulk_ctrl_binding   *b       = CHANNEL_BINDING(channel);

    struct event_closure txcont = MKCONT(bulk_sm_flounder_msg_sent_debug_cb,
            "bulk_sm_assign_pool sent");

    errval_t err = bulk_ctrl_assign_pool_call__tx(b, txcont,
            cdata->flounder_pool, (uint64_t)cdata);

    return err;
}

/**
 * Entry point from the public interface bulk_channel_assign_pool function.
 * Reqeusts confirmation from the other endpoint.
 */
errval_t bulk_sm_assign_pool(
        struct bulk_channel      *channel,
        struct bulk_pool         *pool,
        struct bulk_continuation cont)
{
    struct c_assign_pool_data *cdata =
        malloc(sizeof(struct c_assign_pool_data));
    if (!cdata) {
        return BULK_TRANSFER_MEM;
    }

    cdata->channel      = channel;
    cdata->continuation = cont;
    cdata->pool         = pool;
    generate_pool_for_flounder(pool, &cdata->flounder_pool);

    bulk_sm_flounder_send_fifo_msg_with_arg(channel,
                                            bulk_sm_assign_pool_send_request,
                                            cdata);

    return SYS_ERR_OK;
}

// Pool removal -----------------------------------------------------------

errval_t bulk_sm_remove_pool(
        struct bulk_channel      *channel,
        struct bulk_pool         *pool,
        struct bulk_continuation cont)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}
