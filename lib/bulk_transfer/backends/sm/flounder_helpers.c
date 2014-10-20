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

#include "bulk_sm_impl.h"
#include "../../bulk_pool.h"

#if 0
#define BULK_FH_MSG(fmt, msg...) debug_printf("[%03u: "fmt"]\n", \
        disp_get_domain_id(), msg)
#else
#define BULK_FH_MSG(fmt, msg...)
#endif

void bulk_sm_error_handler_debug(struct bulk_ctrl_binding *_binding, errval_t err)
{
    DEBUG_ERR(err, "BulkTransfer@SHM: async error");
}

void bulk_sm_flounder_msg_sent_debug_cb(void *a)
{
    BULK_FH_MSG("flounder_sent_cb : %s", (char*)a);
}

// called when we can send message again.
static void bulk_sm_flounder_resend_handler(void *a)
{
    // debug_printf("======== RESEND HANDLER ==============================\n");
    struct bulk_channel      *channel = VOID2CHANNEL(a);
    struct waitset           *ws      = channel->waitset;
    struct bulk_sm_impl_data *data    = CHANNEL_DATA(channel);
    struct bulk_ctrl_binding *b       = CHANNEL_BINDING(channel);
    errval_t err = SYS_ERR_OK;

    // <-- BEGIN LOCK
    thread_mutex_lock(&data->resend_lock);

    // dequeue first element
    struct bulk_sm_resend_item *item = data->resend_closure;

    if (!item) {
        USER_PANIC("bulk_sm_flounder_resend_handler called "
                   "but no message to send.\n");
    }
    // Call the registered send function
    BULK_FH_MSG("Dispatching resend item : %p(%p) -> %p",
            item->event.handler, item->event.arg, item);
    struct event_closure ev = item->event;

    if (ev.handler) {
        //XXX: we expect the handler to be a simple function that just tries to
        //send a message and nothing else. we still have the lock while calling
        errval_t (*f)(void*) = (errval_t (*)(void*)) ev.handler;
        err = f(ev.arg);
    }
    if (err_is_ok(err)){
        data->resend_closure = item->next;//remove item from list
        free(item);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY){
        //handler failed, keep item in front of list (to preserve handler order)
    } else {
        DEBUG_ERR(err, "not trying to resend\n");
        data->resend_closure = item->next;//remove item from list
        free(item);
    }

    // if more messages, reregister resend handler
    if (data->resend_closure) {
        err = b->register_send(b, ws,
                            MKCONT(bulk_sm_flounder_resend_handler, channel));
        if (err_is_fail(err)) {
            // somebody else already registered a resend handler.
            // we cannot tolerate this.
            USER_PANIC_ERR(BULK_TRANSFER_SM_EXCLUSIVE_WS,
                    "bulk_sm_flounder_resend_msg_with_arg");
        }
    }

    thread_mutex_unlock(&data->resend_lock);
    // --> END LOCK
}


void bulk_sm_flounder_send_fifo_msg(struct bulk_channel *channel,
                                    errval_t (*send_fn)(void *arg))
{
    bulk_sm_flounder_send_fifo_msg_with_arg(channel, send_fn, channel);
}

void bulk_sm_flounder_send_fifo_msg_with_arg(struct bulk_channel *channel,
                                             errval_t (*send_fn)(void *arg),
                                             void *arg)
{
    struct waitset           *ws   = channel->waitset;
    struct bulk_sm_impl_data *data = CHANNEL_DATA(channel);
    struct bulk_ctrl_binding *b    = CHANNEL_BINDING(channel);
    errval_t err = SYS_ERR_OK;

    // <-- BEGIN LOCK
    thread_mutex_lock(&data->resend_lock);
    struct bulk_sm_resend_item *head = data->resend_closure;

    if (head == NULL) {
        //no other messages in queue -> try sending directly
        err = send_fn(arg);
    }

    if (head != NULL || err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        // insert continuation into waitlist
        struct bulk_sm_resend_item *new_item = malloc(sizeof(*new_item));
        assert(new_item);
        new_item->event = MKCLOSURE((void (*)(void*)) send_fn, arg);
        new_item->next  = NULL;

        if (head == NULL) {
            data->resend_closure = new_item;

            // queue was empty: register resend handler
            err = b->register_send(b, ws,
                    MKCONT(bulk_sm_flounder_resend_handler, channel));
            if (err_is_fail(err)) {
                // somebody else already registered a resend handler.
                // we cannot tolerate this.
                USER_PANIC_ERR(BULK_TRANSFER_SM_EXCLUSIVE_WS,
                        "bulk_sm_flounder_resend_msg_with_arg");
            }
        } else {
            while (head->next) head = head->next;
            head->next = new_item;
        }
        BULK_FH_MSG("Registered resend item  : %p(%p) -> %p",
                new_item->event.handler, new_item->event.arg, new_item);

    } else if (err_is_fail(err)){
        debug_printf("bulk_sm_flounder_send_fifo_msg: sending failed %s\n",
                        err_getstring(err));
    }
    thread_mutex_unlock(&data->resend_lock);
    // --> END LOCK
}



errval_t create_pool_from_flounder(struct bulk_pool       **pool,
                                   const bulk_ctrl_pool_t *f_pool)
{
    assert(pool);
    assert(f_pool);
    errval_t err;

    // allocate pool
    struct bulk_pool_id pool_id = {
        .machine = f_pool->pool_id_machine,
        .dom     = f_pool->pool_id_dom,
        .local   = f_pool->pool_id_local
    };

    struct bulk_pool *p;
    err = bulk_pool_alloc_with_id(&p,
            f_pool->num_buffers, f_pool->buffer_size, pool_id);
    if (err_is_fail(err)) {
        return err;
    }

    // update trust level
    p->trust = flounder2bulk_trust(f_pool->trust);

    // add capability
    if (p->trust != BULK_TRUST_NONE) {
        p->pool_cap = f_pool->cap;
    } else {
        p->pool_cap = NULL_CAP;
    }

    *pool = p;
    return SYS_ERR_OK;
}

void generate_pool_for_flounder(const struct bulk_pool *pool,
                                bulk_ctrl_pool_t       *f_pool)
{
    assert(pool);
    assert(f_pool);

    f_pool->pool_id_machine = pool->id.machine;
    f_pool->pool_id_dom     = pool->id.dom;
    f_pool->pool_id_local   = pool->id.local;
    f_pool->trust           = bulk2flounder_trust(pool->trust);
    f_pool->buffer_size     = pool->buffer_size;
    f_pool->num_buffers     = pool->num_buffers;
    f_pool->cap             = pool->pool_cap;
}


//fills in the poolid fields. does not allocate any new memory
void fill_pool_id_from_flounder(struct bulk_pool_id         *poolid,
                                const bulk_ctrl_poolid_t  *f_poolid)
{
    assert(poolid);
    assert(f_poolid);
    poolid->machine = f_poolid->pool_id_machine;
    poolid->dom     = f_poolid->pool_id_dom;
    poolid->local   = f_poolid->pool_id_local;
}

//fills in the poolid fields. does not allocate any new memory
void fill_pool_id_for_flounder(const struct bulk_pool_id    *poolid,
                               bulk_ctrl_poolid_t  *f_poolid)
{
    assert(poolid);
    assert(f_poolid);
    f_poolid->pool_id_machine = poolid->machine;
    f_poolid->pool_id_dom     = poolid->dom;
    f_poolid->pool_id_local   = poolid->local;
}
