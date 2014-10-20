/*
 * Copyright (c) 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_SM_IMPL_H
#define BULK_SM_IMPL_H

#include <barrelfish/barrelfish.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_sm.h>

#define VOID2CHANNEL(a)    ((struct bulk_channel*)(a))
#define CHANNEL_EP(c)      ((struct bulk_sm_endpoint_descriptor*)(c)->ep)
#define CHANNEL_DATA(c)    ((struct bulk_sm_impl_data*)(c)->impl_data)
#define CHANNEL_BINDING(c) (CHANNEL_DATA(c)->b)

// Flounder call/receive handler ------------------------------------------

void bulk_sm_channel_negotiate_rx_call(
        struct bulk_ctrl_binding  *b,
        enum bulk_ctrl_role_t     role,
        enum bulk_ctrl_trust_t    trust);

void bulk_sm_channel_negotiate_rx_reply(
        struct bulk_ctrl_binding  *b,
        uint64_t                   error,
        enum bulk_ctrl_direction_t match_direction,
        enum bulk_ctrl_role_t      match_role,
        uint64_t                   meta_size);

void bulk_sm_assign_pool_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_pool_t         pool,
        uint64_t                 id);

void bulk_sm_assign_pool_rx_response(
        struct bulk_ctrl_binding *b,
        uint64_t                 error,
        uint64_t                 id);


void bulk_sm_move_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_poolid_t       poolid,
        uint32_t                 bufferid,
        uint32_t                 tid,
        struct capref            cap,
        uint8_t                  *meta,
        size_t                   metasize);

void bulk_sm_move_trusted_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_poolid_t       poolid,
        uint32_t                 bufferid,
        uint32_t                 tid,
        uint8_t                  *meta,
        size_t                   metasize);

void bulk_sm_move_rx_response(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_error_t        error,
        uint32_t                 tid);

void bulk_sm_copy_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_poolid_t       poolid,
        uint32_t                 bufferid,
        uint32_t                 tid,
        struct capref            cap,
        uint8_t                  *meta,
        size_t                   metasize);

void bulk_sm_copy_trusted_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_poolid_t       poolid,
        uint32_t                 bufferid,
        uint32_t                 tid,
        uint8_t                  *meta,
        size_t                   metasize);

void bulk_sm_copy_rx_response(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_error_t        error,
        uint32_t                 tid);

void bulk_sm_pass_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_poolid_t       poolid,
        uint32_t                 bufferid,
        uint32_t                 tid,
        struct capref            cap,
        uint8_t                  *meta,
        size_t                   metasize);

void bulk_sm_pass_trusted_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_poolid_t       poolid,
        uint32_t                 bufferid,
        uint32_t                 tid,
        uint8_t                  *meta,
        size_t                   metasize);

void bulk_sm_pass_rx_response(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_error_t        error,
        uint32_t                 tid);

void bulk_sm_release_rx_call(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_poolid_t       poolid,
        uint32_t                 bufferid,
        uint32_t                 tid);

void bulk_sm_release_rx_response(
        struct bulk_ctrl_binding *b,
        bulk_ctrl_error_t        error,
        uint32_t                 tid);

// Flounder generic callbacks ---------------------------------------------

//printing handler for asynchronous errors
void bulk_sm_error_handler_debug(struct bulk_ctrl_binding *_binding, errval_t err);

/**
 * Callback for use with flounder. Prints to string provided by (void *a).
 */
void bulk_sm_flounder_msg_sent_debug_cb(void *a);

/**
 * Function used as generic send that preserves fifo order.
 * enqueues the message and registers resend handler if necessary
 *
 * @param channel: Channel of interest. Is used as argument for the callback.
 * @param send_fn: send function to be called. this should be a very simple
 *                  function that just reads arg, tries to send it and
 *                  returns the error code.
 */
void bulk_sm_flounder_send_fifo_msg(struct bulk_channel *channel,
                                 errval_t (*send_fn)(void *arg));

void bulk_sm_flounder_send_fifo_msg_with_arg(struct bulk_channel *channel,
                                 errval_t (*send_fn)(void *arg),
                                 void *arg);



// Flounder type conversion helpers ---------------------------------------

static inline enum bulk_channel_direction flounder2bulk_direction(
        enum bulk_ctrl_direction_t direction)
{
    return ((direction == bulk_ctrl_SOURCE) ? BULK_DIRECTION_TX :
                                              BULK_DIRECTION_RX );
}

static inline enum bulk_channel_role flounder2bulk_role(
        enum bulk_ctrl_role_t role)
{
    return ((role == bulk_ctrl_GENERIC) ? BULK_ROLE_GENERIC :
            (role == bulk_ctrl_MASTER)  ? BULK_ROLE_MASTER  :
                                          BULK_ROLE_SLAVE   );
}

static inline enum bulk_trust_level flounder2bulk_trust(
        enum bulk_ctrl_trust_t trust)
{
    return ((trust == bulk_ctrl_NONE) ? BULK_TRUST_NONE :
            (trust == bulk_ctrl_HALF) ? BULK_TRUST_HALF :
                                        BULK_TRUST_FULL );
}

static inline enum bulk_ctrl_direction_t bulk2flounder_direction(
        enum bulk_channel_direction direction)
{
    return ((direction == BULK_DIRECTION_TX) ? bulk_ctrl_SOURCE :
                                               bulk_ctrl_SINK );
}

static inline enum bulk_ctrl_role_t bulk2flounder_role(
        enum bulk_channel_role role)
{
    return ((role == BULK_ROLE_GENERIC) ? bulk_ctrl_GENERIC :
            (role == BULK_ROLE_MASTER)  ? bulk_ctrl_MASTER  :
                                          bulk_ctrl_SLAVE   );
}

static inline enum bulk_ctrl_trust_t bulk2flounder_trust(
        enum bulk_trust_level trust)
{
    assert(trust != BULK_TRUST_UNINITIALIZED); // what to do with that?

    return ((trust == BULK_TRUST_NONE) ? bulk_ctrl_NONE :
            (trust == BULK_TRUST_HALF) ? bulk_ctrl_HALF :
                                         bulk_ctrl_FULL );
}

/**
 * Allocates and initializes a new bulk_pool struct based on flounder data.
 *
 * @param: pool   Pointer where created and allocated pool is stored, by value.
 * @param: f_pool Flounder data to create pool from.
 */
errval_t create_pool_from_flounder(struct bulk_pool       **pool,
                                   const bulk_ctrl_pool_t *f_pool);

/**
 * Creates a flounder pool struct from a bulk_pool.
 *
 * @param: pool   Pool to be represented
 * @param: f_pool Pointer to unused flounder pool.
 */
void generate_pool_for_flounder(const struct bulk_pool *pool,
                                bulk_ctrl_pool_t       *f_pool);


/**
 * sets the fields of a bulk_pool_id struct from a flounder poolid struct.
 * does not allocate any new memory.
 *
 */
void fill_pool_id_from_flounder(struct bulk_pool_id         *poolid,
                                const bulk_ctrl_poolid_t  *f_poolid);

/**
 * sets the fields of a flounder poolid struct from a bulk_pool_id struct.
 * does not allocate any new memory.
 */
void fill_pool_id_for_flounder(const struct bulk_pool_id    *poolid,
                               bulk_ctrl_poolid_t  *f_poolid);

#endif // BULK_SM_IMPL_H
