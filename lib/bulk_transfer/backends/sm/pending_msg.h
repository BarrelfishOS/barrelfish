/*
 * Copyright (c) 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 *
 * A simple sorted linked list for pending flounder RPC messages in the sm backend.
 * Because there is no obvious correspondence between RPC calls and replies,
 * but we have continuations to call after we get the reply,
 * we keep track of them ourselves.
 * All our flounder RPC calls and replies contain a transaction id, which is used
 * to look up the correct callback on receiving a RPC reply.
 *
 * using a linked list should be more than enough, since we don't expect many concurrent
 * pending messages.
 * the list is sorted to prevent duplicate keys
 */

#ifndef BULK_SM_PENDING_MSG_H
#define BULK_SM_PENDING_MSG_H

#include <barrelfish/barrelfish.h>
#include "bulk_sm_impl.h"

//RPC-specific content
struct pending_assign_pool {
        struct bulk_continuation continuation;  ///< continuation after completion
        struct bulk_channel      *channel;      ///< channel under consideration XXX: probably unnecessary
        struct bulk_pool         *pool;         ///< pool to be added to channel
};

struct pending_bind {
        struct bulk_continuation continuation;  ///< continuation after completion
        errval_t                 bind_error;
};

struct pending_move {
        struct bulk_continuation continuation;  ///< continuation after completion
};

struct pending_copy {
        struct bulk_continuation continuation;  ///< continuation after completion
};

struct pending_pass {
        struct bulk_continuation continuation;  ///< continuation after completion
};

struct pending_release {
        struct bulk_continuation continuation;  ///< continuation after completion
};

union pending_msg_data {
    struct pending_assign_pool  assign_pool;
    struct pending_bind         bind;
    struct pending_move         move;
    struct pending_copy         copy;
    struct pending_pass         pass;
    struct pending_release      release;
};


struct bulk_sm_pending_msg{
    uint32_t                        tid;        //key
    struct bulk_sm_pending_msg      *next;      //rest of list (bigger keys)
    struct bulk_sm_pending_msg      *previous;  //rest of list (smaller keys)
    union pending_msg_data          data;       //payload
};

/**
 * add the data to the tree of pending messages in channel
 * generates tid automatically
 *
 * @param channel:  Channel this message belongs to
 * @param tid:      will be filled in with transaction id
 * @param data:     payload for this message
 */
errval_t pending_msg_add(struct bulk_channel* channel,
                         uint32_t *tid,
                         union pending_msg_data data);

/**
 * reads pending message
 *
 * @param channel:  Channel this message belongs to
 * @param tid:      transaction id to look up
 * @param data:     will be filled in with payload for this message
 * @param remove:   whether item is to be removed from list
 */
errval_t pending_msg_get(struct bulk_channel     *channel,
                         uint32_t                tid,
                         union pending_msg_data  *data,
                         bool                    remove);

#endif // BULK_SM_PENDING_MSG_H
