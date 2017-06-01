/**
 * \file
 * \brief Process management service.
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/proc_mgmt_client.h>
#include <if/proc_mgmt_defs.h>

#include "internal.h"

static struct proc_mgmt_rpc_rx_vtbl rpc_rx_vtbl;

static errval_t alloc_ep_handler(struct proc_mgmt_binding *b, errval_t *err,
        struct capref *ep)
{
    struct proc_mgmt_lmp_binding *lmpb = (struct proc_mgmt_lmp_binding*) malloc(
            sizeof(struct proc_mgmt_lmp_binding));
    assert(lmpb != NULL);

    *err = proc_mgmt_client_lmp_accept(lmpb, get_default_waitset(),
            DEFAULT_LMP_BUF_WORDS);
    if (err_is_ok(*err)) {
        *ep = lmpb->chan.local_cap;

        // struct proc_mgmt_state *st = (struct proc_mgmt_state*) malloc(
        //         sizeof(struct proc_mgmt_state));
        // assert(st != NULL);
        // st->queue.head = st->queue.tail = NULL;

        lmpb->b.rpc_rx_vtbl = rpc_rx_vtbl;
        // lmpb->b.st = st;
    } else {
        free(lmpb);
    }

    return SYS_ERR_OK;
}

static errval_t spawn_handler(struct proc_mgmt_binding *b,
                          coreid_t core,
                          const char *path,
                          const char *argvbuf,
                          size_t argvbytes,
                          const char *envbuf,
                          size_t envbytes,
                          uint8_t flags,
                          errval_t *err,
                          struct capref *domainid_cap)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}

static errval_t span_handler(struct proc_mgmt_binding *b,
                         struct capref domainid_cap,
                         coreid_t core,
                         struct capref vroot,
                         struct capref disp_mem,
                         errval_t *err)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}

static errval_t kill_handler(struct proc_mgmt_binding *b,
                         struct capref domainid_cap,
                         errval_t *err)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}

static struct proc_mgmt_rpc_rx_vtbl rpc_rx_vtbl = {
    .alloc_ep_call = alloc_ep_handler,
    .spawn_call = spawn_handler,
    .span_call = span_handler,
    .kill_call = kill_handler
};

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    // construct name
    char namebuf[32];
    size_t len = snprintf(namebuf, sizeof(namebuf), "%s.%d", SERVICE_BASENAME,
                          my_core_id);
    assert(len < sizeof(namebuf));
    namebuf[sizeof(namebuf) - 1] = '\0';

    // register this iref with the name service
    err = nameservice_register(namebuf, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static errval_t connect_cb(void *st, struct proc_mgmt_binding *b)
{
    b->rpc_rx_vtbl = rpc_rx_vtbl;
    return SYS_ERR_OK;
}

errval_t start_service(void)
{
    return proc_mgmt_export(NULL, export_cb, connect_cb, get_default_waitset(),
            IDC_EXPORT_FLAGS_DEFAULT);
}
