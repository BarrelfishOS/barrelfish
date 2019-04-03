/**
 * \file
 * \brief Unidirectional bulk data transfer via shared memory
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_SM_H
#define BULK_SM_H

#include <bulk_transfer/bulk_transfer.h>

#include <if/bulk_ctrl_defs.h>

// Shared memory specific structs -----------------------------------------

enum bulk_sm_endpoint_state {
    BULK_EPSTATE_CREATED,      ///< dummy endpoint after creation
    BULK_EPSTATE_IREF_EXPORTED ///< endpoint's bulk_ctrl is exported on the ep's iref
};

struct bulk_sm_endpoint_descriptor {
    /* generic part */
    struct bulk_endpoint_descriptor ep_generic; ///< generic endpoint part

    /* start of implementation specific part */
    volatile enum bulk_sm_endpoint_state state;
    iref_t                               iref;
    errval_t                             err;
};

struct bulk_sm_resend_item {
    struct bulk_sm_resend_item *next;
    struct event_closure       event;
};

struct bulk_sm_impl_data {
    struct bulk_ctrl_binding    *b;
    struct bulk_sm_pending_msg  *root;
    struct thread_mutex         mutex;//used for changes to pending_msg
    //TODO: remove binding stuff from here, use pending message system instead
    errval_t                    bind_error;
    struct bulk_continuation    bind_cont;

    // resending of flounder messages
    struct thread_mutex         resend_lock;
    struct bulk_sm_resend_item  *resend_closure;
};

// Shared memory implementation callbacks ---------------------------------

errval_t bulk_sm_channel_create(struct bulk_channel *channel);

errval_t bulk_sm_channel_bind(struct bulk_channel     *channel,
                              struct bulk_continuation cont);

errval_t bulk_sm_channel_destroy(struct bulk_channel *channel);

errval_t bulk_sm_assign_pool(struct bulk_channel      *channel,
                             struct bulk_pool         *pool,
                             struct bulk_continuation cont);

errval_t bulk_sm_remove_pool(struct bulk_channel      *channel,
                             struct bulk_pool         *pool,
                             struct bulk_continuation cont);

errval_t bulk_sm_move(struct bulk_channel      *channel,
                      struct bulk_buffer       *buffer,
                      void                     *meta,
                      struct bulk_continuation cont);

errval_t bulk_sm_copy(struct bulk_channel      *channel,
                      struct bulk_buffer       *buffer,
                      void                     *meta,
                      struct bulk_continuation cont);

errval_t bulk_sm_release(struct bulk_channel      *channel,
                         struct bulk_buffer       *buffer,
                         struct bulk_continuation cont);

errval_t bulk_sm_pass(struct bulk_channel      *channel,
                      struct bulk_buffer       *buffer,
                      void                     *meta,
                      struct bulk_continuation cont);

struct bulk_implementation *bulk_sm_get_implementation(void);

// Shared memory support functions ----------------------------------------

/**
 * Creates a new bulk endpoint which uses the shared memory backend
 *
 * @param ep_desc   memory location to create the endpoint in
 *
 * This function is intended to be used by the creator. (exporting side)
 */
errval_t bulk_sm_ep_create(struct bulk_sm_endpoint_descriptor *ep_desc);

/**
 * Creates a new bulk endpoint which uses the shared memory backend
 *
 * @param ep_desc       memory location to create the endpoint in
 * @param remote_iref   the iref of the exported service on the other side
 *
 * This function is intended to be used by the binding side
 */
errval_t bulk_sm_ep_create_remote(struct bulk_sm_endpoint_descriptor *ep_desc,
                                  iref_t remote_iref);

// Helpers to deal with multiple waitsets (each channel requires one) -----

/**
 * List of waitsets to dispatch.
 */
struct bulk_sm_ws_item {
    struct bulk_sm_ws_item *next;
    struct waitset         *ws;   ///< waitset to dispatch. may be NULL.
};

/**
 * Dispatches events on a list of waitsets, non-blocking.
 * @returns: LIB_ERR_NO_EVENT if no event dispatched.
 */
errval_t bulk_sm_multiple_event_dispatch_non_block(struct bulk_sm_ws_item *item);

/**
 * Dispatches events on on a list of waitsets. Retruns if events dispatched.
 */
errval_t bulk_sm_multiple_event_dispatch(struct bulk_sm_ws_item *item);

#endif /* BULK_SM_H */
