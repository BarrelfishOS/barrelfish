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
#include <barrelfish/spawn_client.h>
#include <if/monitor_defs.h>
#include <if/proc_mgmt_defs.h>

#include "internal.h"
#include "spawnd_state.h"

static void add_spawnd_handler(struct proc_mgmt_binding *b, coreid_t core_id,
                               iref_t iref)
{
    if (spawnd_state_exists(core_id)) {
        DEBUG_ERR(PROC_MGMT_ERR_SPAWND_EXISTS, "spawnd_state_exists");
        return;
    }

    // Bind with the spawnd.
    struct spawn_binding *spawnb;
    errval_t err = spawn_bind_iref(iref, &spawnb);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "spawn_bind_iref");
        return;
    }

    err = spawnd_state_alloc(core_id, spawnb);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "spawnd_state_alloc");
    }

    debug_printf("Process manager bound with spawnd.%u on iref %u\n", core_id,
            iref);

    err = spawnd_state_get_binding(core_id)->rpc_tx_vtbl.echo(
            spawnd_state_get_binding(core_id),
            SERVICE_BASENAME,
            disp_get_current_core_id());
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "spawnd echo request failed");
    }
}

static void add_spawnd_handler_non_monitor(struct proc_mgmt_binding *b,
                                           coreid_t core_id, iref_t iref)
{
    debug_printf("Ignoring add_spawnd call: %s\n",
                 err_getstring(PROC_MGMT_ERR_NOT_MONITOR));
}

// static errval_t spawn_handler(struct proc_mgmt_binding *b,
//                           coreid_t core,
//                           const char *path,
//                           const char *argvbuf,
//                           size_t argvbytes,
//                           const char *envbuf,
//                           size_t envbytes,
//                           uint8_t flags,
//                           errval_t *err,
//                           struct capref *domainid_cap)
// {
//     return LIB_ERR_NOT_IMPLEMENTED;
// }

// static errval_t span_handler(struct proc_mgmt_binding *b,
//                          struct capref domainid_cap,
//                          coreid_t core,
//                          struct capref vroot,
//                          struct capref disp_mem,
//                          errval_t *err)
// {
//     return LIB_ERR_NOT_IMPLEMENTED;
// }

// static errval_t kill_handler(struct proc_mgmt_binding *b,
//                          struct capref domainid_cap,
//                          errval_t *err)
// {
//     return LIB_ERR_NOT_IMPLEMENTED;
// }

static struct proc_mgmt_rx_vtbl monitor_vtbl = {
    .add_spawnd = add_spawnd_handler
};

static struct proc_mgmt_rx_vtbl non_monitor_vtbl = {
    .add_spawnd = add_spawnd_handler_non_monitor
};

static errval_t alloc_ep_for_monitor(struct capref *ep)
{
    struct proc_mgmt_lmp_binding *lmpb =
        malloc(sizeof(struct proc_mgmt_lmp_binding));
    assert(lmpb != NULL);

    // setup our end of the binding
    errval_t err = proc_mgmt_client_lmp_accept(lmpb, get_default_waitset(),
                                               DEFAULT_LMP_BUF_WORDS);
    if (err_is_fail(err)) {
        free(lmpb);
        return err_push(err, LIB_ERR_PROC_MGMT_CLIENT_ACCEPT);
    }

    *ep = lmpb->chan.local_cap;
    lmpb->b.rx_vtbl = monitor_vtbl;

    return SYS_ERR_OK;
}

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    // Allocate an endpoint for the local monitor, who will use it to inform
    // us about new spawnd irefs on behalf of other monitors.
    struct capref ep;
    err = alloc_ep_for_monitor(&ep);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to allocate LMP EP for local monitor");
    }

    // Send the endpoint to the monitor, so it can finish the handshake.
    struct monitor_binding *mb = get_monitor_binding();
    err = mb->tx_vtbl.set_proc_mgmt_ep_request(mb, NOP_CONT, ep);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send set_proc_mgmt_ep_request to "
                       "monitor");
    }

    // Also register this iref with the name service, for arbitrary client
    // domains to use for spawn-related ops.
    err = nameservice_register(SERVICE_BASENAME, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }

    // Try to create a few domains?
    // TODO(razvan): Remove this.
    size_t num_domains = 5;
    struct capref domain_caps[num_domains];
    for (size_t i = 1; i <= num_domains; ++i) {
        err = slot_alloc(&domain_caps[i]);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "slot_alloc domain_cap");
        }
        err = cap_retype(domain_caps[i], cap_procmng, 0, ObjType_Domain, 0, 1);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "cap_retype domain_cap from cap_procmng");
        }
        struct capability ret;
        err = debug_cap_identify(domain_caps[i], &ret);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "cap identify domain_cap");
        }
        debug_printf("Process manager successfully created domain { .coreid=%u,"
                     " .core_local_id=%u } (%lu/%lu)\n", ret.u.domain.coreid,
                     ret.u.domain.core_local_id, i, num_domains);
    }
}

static errval_t connect_cb(void *st, struct proc_mgmt_binding *b)
{
    b->rx_vtbl = non_monitor_vtbl;
    return SYS_ERR_OK;
}

errval_t start_service(void)
{
    return proc_mgmt_export(NULL, export_cb, connect_cb, get_default_waitset(),
            IDC_EXPORT_FLAGS_DEFAULT);
}
