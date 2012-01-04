/**
 * \file
 * \brief Publish/Subscribe client API implementation
 *
 * The individual handler functions are stored in a function table on the
 * client side. The API provides convenience functions for subscribe/
 * unsubscribe and publish.
 *
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/threads.h>
#include <if/dist2_rpcclient_defs.h>

#include <dist2/pubsub.h>

#include "common.h"

static subscription_handler_fn subscriber_table[MAX_SUBSCRIPTIONS] = { NULL };
static struct thread_mutex subscriber_table_lock;

static errval_t get_free_slot(subscription_t *slot)
{
    assert(slot != NULL);

    subscription_t idx = 0;
    for (; idx < MAX_SUBSCRIPTIONS; idx++) {
        if (subscriber_table[idx] == NULL) {
            *slot = idx;
            return SYS_ERR_OK;
        }
    }

    return SYS_ERR_BMP_INVALID; // TODO proper error code
}

void subscribed_message_handler(struct dist2_binding *b, subscription_t id,
        char *record)
{
    assert(subscriber_table[id] != NULL);

    // TODO pass on state
    subscriber_table[id](id, record, NULL);
}

/**
 * \brief Subscribe for a given type of message.
 *
 * \param[in] function Handler function in case a matching record is
 * published.
 * \param[in] state State passed on to handler function.
 * \param[out] id Id of the subscription.
 * \param record What type of records you want to subscribe.
 * \param ... Additional arguments to format the record using vsprintf.
 *
 * \retval SYS_ERR_OK
 */
errval_t dist_subscribe(subscription_handler_fn function, void *state,
        subscription_t *id, char *record, ...)
{
    assert(function != NULL);
    assert(record != NULL);
    assert(id != NULL);
    thread_mutex_lock(&subscriber_table_lock);

    va_list args;
    errval_t err = SYS_ERR_OK;

    err = get_free_slot(id);
    if (err_is_fail(err)) {
        return err;
    }

    size_t length = 0;
    char* buf = NULL;
    va_start(args, record);
    err = allocate_string(record, args, &length, &buf);
    va_end(args);
    if (err_is_fail(err)) {
        return err;
    }

    va_start(args, record);
    size_t bytes_written = vsnprintf(buf, length + 1, record, args);
    va_end(args);
    assert(bytes_written == length);

    // send to skb
    struct dist2_rpc_client *rpc_client = get_dist_rpc_client();

    errval_t error_code = SYS_ERR_OK;
    err = rpc_client->vtbl.subscribe(rpc_client, buf, *id, &error_code);

    // TODO check error_code
    if (err_is_ok(err)) {
        subscriber_table[*id] = function;
    }

    free(buf);

    thread_mutex_unlock(&subscriber_table_lock);
    return err;
}

/**
 * \brief Unsubscribes a subscription.
 *
 * \param id Id of the subscription (as provided by dist_subscribe).
 *
 * \retval SYS_ERR_OK
 */
errval_t dist_unsubscribe(subscription_t id)
{
    assert(id < MAX_SUBSCRIPTIONS);
    thread_mutex_lock(&subscriber_table_lock);

    // send to skb
    struct dist2_rpc_client *rpc_client = get_dist_rpc_client();

    errval_t error_code = SYS_ERR_OK;
    errval_t err = rpc_client->vtbl.unsubscribe(rpc_client, id, &error_code);

    if (err_is_ok(err)) { // TODO check error_code
        subscriber_table[id] = NULL;
    }

    thread_mutex_unlock(&subscriber_table_lock);
    return err;
}

/**
 * \brief Publishes a record.
 *
 * \param record The record to publish.
 * \param ... Additional arguments to format the record using vsprintf.
 *
 * \retval SYS_ERR_OK
 */
errval_t dist_publish(char *record, ...)
{
    assert(record != NULL);

    va_list args;
    errval_t err = SYS_ERR_OK;

    size_t length = 0;
    char *buf = NULL;
    va_start(args, record);
    err = allocate_string(record, args, &length, &buf);
    va_end(args);
    if (err_is_fail(err)) {
        return err;
    }

    va_start(args, record);
    size_t bytes_written = vsnprintf(buf, length + 1, record, args);
    va_end(args);
    assert(bytes_written == length);

    struct dist2_rpc_client *rpc_client = get_dist_rpc_client();

    errval_t error_code = SYS_ERR_OK;
    err = rpc_client->vtbl.publish(rpc_client, buf, &error_code);
    // TODO check error_code

    free(buf);
    return err;
}

/**
 * \brief Initialized publish/subscribe system.
 */
void dist_pubsub_init(void)
{
    thread_mutex_init(&subscriber_table_lock);
}
