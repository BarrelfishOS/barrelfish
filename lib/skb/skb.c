/**
 * \file
 * \brief SKB connection
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
#include <barrelfish/nameservice_client.h>
#include <skb/skb.h>
#include <if/skb_defs.h>
#include <barrelfish/core_state_arch.h>

/* ------------------------- Connecting to skb ------------------------------ */

static void bind_cb(void *st, errval_t err, struct skb_binding *b)
{
    struct skb_state *skb_state = get_skb_state();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed");
        abort();
    }

    skb_state->skb = b;
    skb_rpc_client_init(skb_state->skb);
    assert(!skb_state->request_done);
    skb_state->request_done = true;
}

errval_t skb_client_connect(void)
{
    errval_t err;
    iref_t iref;
    struct skb_state *skb_state = get_skb_state();

    /* check if the RPC client alreay has been initialized */
    if (skb_state->skb != NULL) {
        return SYS_ERR_OK;
    }

    err = nameservice_blocking_lookup("skb", &iref);
    if (err_is_fail(err)) {
        return err;
    }

    skb_state->request_done = false;
    err = skb_bind(iref, bind_cb, NULL, get_default_waitset(),
                   IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, FLOUNDER_ERR_BIND);
    }

    /* XXX: wait for connection to complete */
    while (!skb_state->request_done) {
        messages_wait_and_handle_next();
    }

    return SYS_ERR_OK;
}

/* ------------------------- evaluate ------------------------------ */
errval_t skb_evaluate(char *query, char **ret_result, char **ret_str_error, int32_t *int_error)
{
    errval_t err;
    struct skb_state *skb_state = get_skb_state();

    // allocate memory for holding the response data
    char *result = NULL;
    if (ret_result) {
        result = malloc(skb__run_response_output_MAX_ARGUMENT_SIZE);
        if (result == NULL) {
            return LIB_ERR_MALLOC_FAIL;
        }
    }
    char *str_error = NULL;
    if (ret_str_error) {
        str_error = malloc(skb__run_response_str_error_MAX_ARGUMENT_SIZE);
        if (str_error == NULL) {
            if (result) {
                free(result);
            }
            return LIB_ERR_MALLOC_FAIL;
        }
    }
    err = skb_state->skb->rpc_tx_vtbl.run(skb_state->skb, query, result,
                                   str_error, int_error);
    if (err_is_fail(err)) {
        if (result) {
            free(result);
        }
        if (str_error) {
            free(str_error);
        }
        return err_push(err, SKB_ERR_RUN);
    }

    if (ret_result) {
        *ret_result = result;
    }
    if (ret_str_error) {
        *ret_str_error = str_error;
    }

    return SYS_ERR_OK;
}
