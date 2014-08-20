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
 */

#include "pending_msg.h"
#include <stdlib.h>

/**
 * dump pending message TID's for given channel
 */
static void pending_msg_dump(struct bulk_channel *channel)
{
    assert(channel);
    thread_mutex_lock_nested(&CHANNEL_DATA(channel)->mutex);

    struct bulk_sm_pending_msg *node = CHANNEL_DATA(channel)->root;
    debug_printf("Dumping pending message TID's for channel %p.\n", channel);
    while (node) {
        debug_printf("  %u\n", node->tid);
        node = node->next;
    }

    thread_mutex_unlock(&CHANNEL_DATA(channel)->mutex);
}


/**
 * add the data to the list of pending messages in channel
 * generates tid automatically
 *
 * @param channel:  Channel this message belongs to
 * @param tid:      will be filled in with transaction id
 * @param data:     payload for this message
 */
errval_t pending_msg_add(struct bulk_channel* channel,
                         uint32_t *tid,
                         union pending_msg_data data)
{
    assert(channel);
    struct bulk_sm_pending_msg *p = malloc(sizeof(struct bulk_sm_pending_msg));
    assert(p);

    p->data = data;
    p->next = NULL;
    p->previous = NULL;

    //seperate variable declared for easier compiler optimization
    uint32_t thistid = (uint32_t) rand();
    p->tid = thistid;

    // debug_printf("PENDING MSG: [new tid=%u]\n", thistid);
    // debug_printf("before:\n");
    // pending_msg_dump(channel);

    thread_mutex_lock(&CHANNEL_DATA(channel)->mutex);
    struct bulk_sm_pending_msg *node = CHANNEL_DATA(channel)->root;

    if (node == NULL){    //no other entries
        CHANNEL_DATA(channel)->root = p;
        *tid = thistid;
        thread_mutex_unlock(&CHANNEL_DATA(channel)->mutex);
        // debug_printf("after:\n");
        // pending_msg_dump(channel);
        return SYS_ERR_OK;
    }  else {
        while(true){
            if (node->tid < thistid){
                if (node->next){
                    node = node->next;
                } else {    //end of list reached
                    p->previous = node;
                    node->next = p;
                    *tid = thistid;
                    thread_mutex_unlock(&CHANNEL_DATA(channel)->mutex);
                    // debug_printf("after:\n");
                    // pending_msg_dump(channel);
                    return SYS_ERR_OK;
                }
            } else if (node->tid > thistid) {
                p->next = node;
                p->previous = node->previous;

                node->previous = p;

                if (p->previous) {
                    p->previous->next = p;
                } else {
                    //become new root
                    CHANNEL_DATA(channel)->root = p;
                }

                *tid = thistid;
                thread_mutex_unlock(&CHANNEL_DATA(channel)->mutex);
                // debug_printf("after:\n");
                // pending_msg_dump(channel);
                return SYS_ERR_OK;
            } else {
                // //tid already taken -> try again with different tid
                // thistid = (uint32_t) rand();
                // p->tid = thistid;
                // node = CHANNEL_DATA(channel)->root; // XXX WRONG. root could be NULL -- jb
                thread_mutex_unlock(&CHANNEL_DATA(channel)->mutex);
                free(p);
                pending_msg_add(channel, tid, data); // XXX does copy of data recursively :-(
            }
        }
    }
    assert(!"should not be reached");
}

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
                         bool                     do_remove)
{
    assert(channel);

    thread_mutex_lock(&CHANNEL_DATA(channel)->mutex);
    struct bulk_sm_pending_msg *p = CHANNEL_DATA(channel)->root;

    // debug_printf("PENDING MSG: [remove tid=%u]\n", tid);
    if (0) pending_msg_dump(channel); // defined but not used :-(

    while(p != NULL){
        if (p->tid < tid){
            p = p->next;
        } else if (p->tid > tid) {
            p = NULL;//abort early (list is sorted)
        } else {
            //tid matches -> found
            *data = p->data;

            if (do_remove) {
                //remove from tree
                if (p->next){
                    p->next->previous = p->previous;
                }
                if (p->previous){
                    p->previous->next = p->next;
                } else {
                    CHANNEL_DATA(channel)->root = p->next;
                }

                free(p);
            }

            thread_mutex_unlock(&CHANNEL_DATA(channel)->mutex);
            return SYS_ERR_OK;
        }
    }
    thread_mutex_unlock(&CHANNEL_DATA(channel)->mutex);
    return BULK_TRANSFER_SM_NO_PENDING_MSG;
}
