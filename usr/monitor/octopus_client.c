/**
 * \file
 * \brief Monitor octopus client
 *
 * Cannot simply use octopus client library as monitor does not support THC
 * calls.
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <if/octopus_defs.h>
#include <octopus/getset.h> // for oct_read TODO
#include <octopus/trigger.h> // for NOP_TRIGGER
#include "monitor.h"

struct bind_state {
    bool done;
    errval_t err;
};

static void error_handler(struct octopus_binding *b, errval_t err)
{
    USER_PANIC_ERR(err, "asynchronous error in nameservice binding");
}

static void bind_continuation(void *st_arg, errval_t err,
                              struct octopus_binding *b)
{
    struct bind_state *st = st_arg;

    if (err_is_ok(err)) {
        b->error_handler = error_handler;

        octopus_rpc_client_init(b);
        set_octopus_binding(b);
    }

    st->err = err;
    st->done = true;
}

errval_t octopus_client_bind(void)
{
    errval_t err;
    struct bind_state st = { .done = false };
    assert(name_serv_iref != 0);
    err = octopus_bind(name_serv_iref, bind_continuation, &st,
            get_default_waitset(), IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    while (!st.done) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "ev_disp in octopus_client_bind");
        }
    }

    return st.err;
}

errval_t octopus_set_bspkcb(void)
{
    errval_t err, octerr;

    const char *kcb_key = "kcb_id_0";
    const char *mapping = "kcb.0 { kcb_id: 0, barrelfish_id: 0, cap_key: 'kcb_id_0' }";

    struct capref bspkcb = {
        .cnode = cnode_root,
        .slot = ROOTCN_SLOT_BSPKCB,
    };

    debug_printf("%s: storing cap\n", __FUNCTION__);
    struct octopus_binding *orpc = get_octopus_binding();
    err = orpc->rpc_tx_vtbl.put_cap(orpc, kcb_key, bspkcb, &octerr);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "oct_put_capability(bspkcb)");
        return err;
    }
    if (err_is_fail(octerr)) {
        DEBUG_ERR(octerr, "from octopus in oct_put_capability(bspkcb)");
        return err;
    }
    debug_printf("%s: setting mapping\n", __FUNCTION__);
    err = orpc->rpc_tx_vtbl.set(orpc, mapping, SET_DEFAULT, NOP_TRIGGER, false,
                         NULL, NULL, &octerr);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "oct_set(\"kcb.0 { ... }\")");
        return err;
    }
    if (err_is_fail(octerr)) {
        DEBUG_ERR(octerr, "from octopus oct_set(\"kcb.0 { ... }\")");
        return err;
    }

    return SYS_ERR_OK;
}
