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

#include <octopus/init.h>
#include <octopus/pubsub.h>

#include "common.h"
#include "handler.h"

void subscription_handler(struct dist2_binding *b, subscription_t id,
        uint64_t fn, dist2_mode_t mode, char *record,
        uint64_t st)
{
    subscription_handler_fn handler_fn = (subscription_handler_fn) fn;
    void* state = (void*) st;

    if (handler_fn != NULL) {
        handler_fn(mode, record, state);
    }
    else {
        fprintf(stderr, "Incoming subscription(%lu) for %s with unset handler function.",
                id, record);
        free(record);
    }
}

/**
 * \brief Subscribe for a given type of message.
 *
 * \param[in] function Handler function in case a matching record is
 * published.
 * \param[in] state State passed on to handler function.
 * \param[out] id Id of the subscription. In case of the value is undefined.
 * \param query What type of records you want to subscribe.
 * \param ... Additional arguments to format the record using vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval DIST2_ERR_MAX_SUBSCRIPTIONS
 * \retval DIST2_ERR_PARSER_FAIL
 * \retval DIST2_ERR_ENGINE_FAIL
 */
errval_t oct_subscribe(subscription_handler_fn function, const void *state,
        subscription_t *id, const char *query, ...)
{
    assert(function != NULL);
    assert(query != NULL);
    assert(id != NULL);

    va_list args;
    errval_t err = SYS_ERR_OK;

    char* buf = NULL;
    FORMAT_QUERY(query, args, buf);

    // send to skb
    struct dist2_thc_client_binding_t* cl = oct_get_thc_client();
    errval_t error_code;
    err = cl->call_seq.subscribe(cl, buf, (uint64_t)function,
            (uint64_t)state, id, &error_code); // XXX: Sending Pointer as uint64
    if (err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}

/**
 * \brief Unsubscribes a subscription.
 *
 * \param id Id of the subscription (as provided by oct_subscribe).
 *
 * \retval SYS_ERR_OK
 * \retval DIST2_ERR_PARSER_FAIL
 * \retval DIST2_ERR_ENGINE_FAIL
 */
errval_t oct_unsubscribe(subscription_t id)
{
    // send to skb
    struct dist2_thc_client_binding_t* cl = oct_get_thc_client();
    errval_t error_code;
    errval_t err = cl->call_seq.unsubscribe(cl, id, &error_code);
    if (err_is_ok(err)) {
        err = error_code;
    }

    return err;
}

/**
 * \brief Publishes a record.
 *
 * \param record The record to publish.
 * \param ... Additional arguments to format the record using vsprintf.
 *
 * \retval SYS_ERR_OK
 * \retval DIST2_ERR_PARSER_FAIL
 * \retval DIST2_ERR_ENGINE_FAIL
 */
errval_t oct_publish(const char *record, ...)
{
    assert(record != NULL);

    va_list args;
    errval_t err = SYS_ERR_OK;

    char *buf = NULL;
    FORMAT_QUERY(record, args, buf);

    struct dist2_thc_client_binding_t* cl = oct_get_thc_client();
    errval_t error_code;
    err = cl->call_seq.publish(cl, buf, &error_code);
    if(err_is_ok(err)) {
        err = error_code;
    }

    free(buf);
    return err;
}
