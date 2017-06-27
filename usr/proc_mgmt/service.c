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

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/proc_mgmt_client.h>
#include <barrelfish/spawn_client.h>
#include <if/monitor_defs.h>
#include <if/proc_mgmt_defs.h>
#include <if/spawn_defs.h>

#include "domain.h"
#include "internal.h"
#include "pending_clients.h"
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
}

static void add_spawnd_handler_non_monitor(struct proc_mgmt_binding *b,
                                           coreid_t core_id, iref_t iref)
{
    // debug_printf("Ignoring add_spawnd call: %s\n",
    //              err_getstring(PROC_MGMT_ERR_NOT_MONITOR));
}

static void spawn_reply_handler(struct spawn_binding *b,
                                struct capref domain_cap, errval_t spawn_err)
{
    struct pending_client *cl;
    errval_t err = pending_clients_release(domain_cap, &cl);
    if (err_is_fail(err)) {
        // This might be a kill request issued after a successful spawn/span
        // followed by a local error in the process manager (see below). If that
        // is the case, then we won't have a client, as it has already been
        // released.
        DEBUG_ERR(err, "failed to retrieve pending client based on domain cap "
                  "returned by spawnd");
        return;
    }

    errval_t resp_err = SYS_ERR_OK;
    struct domain_entry *entry;
    switch (cl->type) {
        case ClientType_Spawn:
            err = spawn_err;
            if (err_is_ok(spawn_err)) {
                err = domain_spawn(domain_cap, cl->core_id);
            }
            resp_err = cl->b->tx_vtbl.spawn_response(cl->b, NOP_CONT, err,
                                                     domain_cap);
            break;

        case ClientType_SpawnWithCaps:
            err = spawn_err;
            if (err_is_ok(spawn_err)) {
                err = domain_spawn(domain_cap, cl->core_id);
            }
            resp_err = cl->b->tx_vtbl.spawn_with_caps_response(cl->b, NOP_CONT,
                                                               err, domain_cap);
            break;

        case ClientType_Span:
            err = spawn_err;
            if (err_is_ok(spawn_err)) {
                err = domain_span(domain_cap, cl->core_id);
            }
            resp_err = cl->b->tx_vtbl.span_response(cl->b, NOP_CONT, err);
            break;

        case ClientType_Kill:
            if (err_is_fail(spawn_err)) {
                // Looks like some spawnd was unable to successfully kill
                // its dispatcher for this domain. Not much the process
                // manager can do about it; return the error to the client.
                resp_err = cl->b->tx_vtbl.kill_response(cl->b, NOP_CONT,
                                                        err);
                break;
            }

            err = domain_get_by_cap(domain_cap, &entry);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "failed to retrieve domain by domain_cap "
                          "returned by spawnd after kill");
                break;
            }

            assert(entry->num_spawnds_resources > 0 ||
                   entry->num_spawnds_running > 0);
            assert(entry->status != DOMAIN_STATUS_CLEANED);

            if (entry->num_spawnds_running > 0) {
                --entry->num_spawnds_running;
                    
                err = pending_clients_add(domain_cap, cl->b,
                                          ClientType_Kill, MAX_COREID);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "pending_clients_add in reply handler");
                }

                if (entry->num_spawnds_running == 0) {
                    entry->status = DOMAIN_STATUS_STOPPED;
                    entry->exit_status = EXIT_STATUS_KILLED;

                    // TODO(razvan): Might it be more sane if we respond back
                    // to the client after the domain has been cleaned up (i.e.
                    // the cspace root has been revoked for all dispatchers)?
                    resp_err = cl->b->tx_vtbl.kill_response(cl->b, NOP_CONT,
                                                           err);
                    
                    // TODO(razvan): Same problem applies to the waiters: would
                    // it be better if we sent them wait_responses after the
                    // cspace root has been revoked, too? (here and in the exit
                    // case).
                    struct domain_waiter *waiter = entry->waiters;
                    while (waiter != NULL) {
                        waiter->b->tx_vtbl.wait_response(waiter->b, NOP_CONT,
                                                         SYS_ERR_OK,
                                                         entry->exit_status);
                        struct domain_waiter *aux = waiter;
                        waiter = waiter->next;
                        free(aux);
                    }

                    for (coreid_t i = 0; i < MAX_COREID; ++i) {
                        if (entry->spawnds[i] == NULL) {
                            continue;
                        }

                        struct spawn_binding *spb = entry->spawnds[i]->b;
                        spb->rx_vtbl.spawn_reply = spawn_reply_handler;
                        errval_t req_err = spb->tx_vtbl.cleanup_request(spb,
                                NOP_CONT, cap_procmng, domain_cap);
                        if (err_is_fail(req_err)) {
                            DEBUG_ERR(req_err, "failed to send cleanup_request "
                                      "to spawnd %u\n", i);
                        }
                    }
                }
            } else {
                --entry->num_spawnds_resources;

                if (entry->num_spawnds_resources == 0) {
                    entry->status = DOMAIN_STATUS_CLEANED;

                    // At this point, the domain exists in state CLEANED for
                    // history reasons. For instance, if some other domain
                    // issues a wait call for this one, the process manager can
                    // return the exit status directly.
                    // At some point, however, we might want to just clean up
                    // the domain entry and recycle the domain cap.
                } else {
                    // Expecting to receive further cleanup replies from other
                    // spawnds for the same domain cap, hence re-add the
                    // pending client.
                    err = pending_clients_add(domain_cap, cl->b,
                                              ClientType_Exit, MAX_COREID);
                    if (err_is_fail(err)) {
                        DEBUG_ERR(err, "pending_clients_add in reply handler");
                    }
                }
            }
            break;

        case ClientType_Exit:
            if (err_is_fail(spawn_err)) {
                // Looks like some spawnd was unable to successfully kill
                // its dispatcher for this domain. Not much the process
                // manager can do about it. Furthermore, this was an exit call,
                // so there's no client to reply back to.
                break;
            }

            err = domain_get_by_cap(domain_cap, &entry);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "failed to retrieve domain by domain_cap "
                          "returned by spawnd after kill");
                break;
            }

            assert(entry->num_spawnds_resources > 0 ||
                   entry->num_spawnds_running > 0);
            assert(entry->status != DOMAIN_STATUS_CLEANED);

            if (entry->num_spawnds_running > 0) {
                --entry->num_spawnds_running;

                err = pending_clients_add(domain_cap, cl->b,
                                          ClientType_Exit, MAX_COREID);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "pending_clients_add in reply handler");
                }

                if (entry->num_spawnds_running == 0) {
                    entry->status = DOMAIN_STATUS_STOPPED;

                    struct domain_waiter *waiter = entry->waiters;
                    while (waiter != NULL) {
                        waiter->b->tx_vtbl.wait_response(waiter->b, NOP_CONT,
                                                         SYS_ERR_OK,
                                                         entry->exit_status);
                        struct domain_waiter *aux = waiter;
                        waiter = waiter->next;
                        free(aux);
                    }

                    for (coreid_t i = 0; i < MAX_COREID; ++i) {
                        if (entry->spawnds[i] == NULL) {
                            continue;
                        }

                        struct spawn_binding *spb = entry->spawnds[i]->b;
                        spb->rx_vtbl.spawn_reply = spawn_reply_handler;
                        errval_t req_err = spb->tx_vtbl.cleanup_request(spb,
                                NOP_CONT, cap_procmng, domain_cap);
                        if (err_is_fail(req_err)) {
                            DEBUG_ERR(req_err, "failed to send cleanup_request "
                                      "to spawnd %u\n", i);
                        }
                    }
                }
            } else {
                --entry->num_spawnds_resources;

                if (entry->num_spawnds_resources == 0) {
                    entry->status = DOMAIN_STATUS_CLEANED;

                    // At this point, the domain exists in state CLEANED for
                    // history reasons. For instance, if some other domain
                    // issues a wait call for this one, the process manager can
                    // return the exit status directly.
                    // At some point, however, we might want to just clean up
                    // the domain entry and recycle the domain cap.
                } else {
                    // Expecting to receive further cleanup replies from other
                    // spawnds for the same domain cap, hence re-add the
                    // pending client.
                    err = pending_clients_add(domain_cap, cl->b,
                                              ClientType_Exit, MAX_COREID);
                    if (err_is_fail(err)) {
                        DEBUG_ERR(err, "pending_clients_add in reply handler");
                    }
                }
            }
            break;

        default:
            // TODO(razvan): Handle the other cases, e.g. wait.
            debug_printf("Unknown client type %u\n", cl->type);
            return;
    }

    if (err_is_ok(spawn_err) && err_is_fail(err)) {
        // Spawnd has successfully completed its end of the operation, but
        // there's been an error in the process manager's book-keeping
        // of domains. Therefore, if the request was a spawn or span one, spawnd
        // needs to be asked to stop the dispatcher which it has just enqueued.
        if (cl->type == ClientType_Spawn ||
            cl->type == ClientType_SpawnWithCaps ||
            cl->type == ClientType_Span) {
            struct spawnd_state *state = spawnd_state_get(cl->core_id);
            assert(state != NULL);
            struct spawn_binding *spb = state->b;
            assert(spb != NULL);

            err = spb->tx_vtbl.kill_request(spb, NOP_CONT, cap_procmng, 
                                            domain_cap);
            if (err_is_fail(err)) {
                // XXX: How severe is this? Maybe we want something more
                // assertive than logging an error message.
                DEBUG_ERR(err, "failed to send kill request for dangling "
                          "dispatcher");
            } else {
                pending_clients_add(domain_cap, cl->b, ClientType_Kill,
                                    MAX_COREID);
            }
        }
    }

    free(cl);

    if (err_is_fail(resp_err)) {
        DEBUG_ERR(resp_err, "failed to send response to client");
    }
}

static errval_t spawn_handler_common(struct proc_mgmt_binding *b,
                                     enum ClientType type,
                                     coreid_t core_id, const char *path,
                                     const char *argvbuf, size_t argvbytes,
                                     const char *envbuf, size_t envbytes,
                                     struct capref inheritcn_cap,
                                     struct capref argcn_cap, uint8_t flags,
                                     struct capref *ret_domain_cap)
{
    assert(ret_domain_cap != NULL);

    if (!spawnd_state_exists(core_id)) {
        return PROC_MGMT_ERR_INVALID_SPAWND;
    }

    struct spawnd_state *state = spawnd_state_get(core_id);
    assert(state != NULL);
    struct spawn_binding *cl = state->b;
    assert(cl != NULL);

    struct capref domain_cap;
    errval_t err = slot_alloc(&domain_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "slot_alloc domain_cap");
        return err_push(err, PROC_MGMT_ERR_CREATE_DOMAIN_CAP);
    }
    err = cap_retype(domain_cap, cap_procmng, 0, ObjType_Domain, 0, 1);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cap_retype domain_cap");
        return err_push(err, PROC_MGMT_ERR_CREATE_DOMAIN_CAP);
    }

    err = pending_clients_add(domain_cap, b, type, core_id);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pending_clients_add");
        return err;
    }

    cl->rx_vtbl.spawn_reply = spawn_reply_handler;
    if (capref_is_null(inheritcn_cap) && capref_is_null(argcn_cap)) {
        err = cl->tx_vtbl.spawn_request(cl, NOP_CONT, cap_procmng, domain_cap,
                                        path, argvbuf, argvbytes, envbuf,
                                        envbytes, flags);
    } else {
        err = cl->tx_vtbl.spawn_with_caps_request(cl, NOP_CONT, cap_procmng,
                                                  domain_cap, path, argvbuf,
                                                  argvbytes, envbuf, envbytes,
                                                  inheritcn_cap, argcn_cap,
                                                  flags);
    }
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "sending spawn request");
        pending_clients_release(domain_cap, NULL);
        return err_push(err, PROC_MGMT_ERR_SPAWND_REQUEST);
    }

    return SYS_ERR_OK;
}

static void spawn_handler(struct proc_mgmt_binding *b, coreid_t core_id,
                          const char *path, const char *argvbuf,
                          size_t argvbytes, const char *envbuf, size_t envbytes,
                          uint8_t flags)
{
    errval_t err, resp_err;
    struct capref domain_cap;
    err = spawn_handler_common(b, ClientType_Spawn, core_id, path, argvbuf,
                               argvbytes, envbuf, envbytes, NULL_CAP, NULL_CAP,
                               flags, &domain_cap);
    if (err_is_ok(err)) {
        // Will respond to client when we get the reply from spawnd.
        return;
    }

    resp_err = b->tx_vtbl.spawn_response(b, NOP_CONT, err, NULL_CAP);
    if (err_is_fail(resp_err)) {
        DEBUG_ERR(resp_err, "failed to send spawn_response");
    }
}

static void spawn_with_caps_handler(struct proc_mgmt_binding *b,
                                    coreid_t core_id, const char *path,
                                    const char *argvbuf, size_t argvbytes,
                                    const char *envbuf, size_t envbytes,
                                    struct capref inheritcn_cap,
                                    struct capref argcn_cap, uint8_t flags)
{
    errval_t err, resp_err;
    struct capref domain_cap;
    err = spawn_handler_common(b, ClientType_SpawnWithCaps, core_id, path,
                               argvbuf, argvbytes, envbuf, envbytes,
                               inheritcn_cap, argcn_cap, flags, &domain_cap);
    if (err_is_ok(err)) {
        // Will respond to client when we get the reply from spawnd.
        return;
    }

    resp_err = b->tx_vtbl.spawn_with_caps_response(b, NOP_CONT, err,
                                                            NULL_CAP);
    if (err_is_fail(resp_err)) {
        DEBUG_ERR(resp_err, "failed to send spawn_with_caps_response");
    }
}

static void span_handler(struct proc_mgmt_binding *b, struct capref domain_cap,
                         coreid_t core_id, struct capref vroot,
                         struct capref dispframe)
{
    errval_t err, resp_err;
    err = domain_can_span(domain_cap, core_id);
    if (err_is_fail(err)) {
        goto respond_with_err;
    }

    if (!spawnd_state_exists(core_id)) {
        err = PROC_MGMT_ERR_INVALID_SPAWND;
        goto respond_with_err;
    }

    struct spawnd_state *state = spawnd_state_get(core_id);
    assert(state != NULL);
    struct spawn_binding *cl = state->b;
    assert(cl != NULL);

    err = pending_clients_add(domain_cap, b, ClientType_Span, core_id);
    if (err_is_fail(err)) {
        goto respond_with_err;
    }

    cl->rx_vtbl.spawn_reply = spawn_reply_handler;
    err = cl->tx_vtbl.span_request(cl, NOP_CONT, cap_procmng, domain_cap, vroot,
                                   dispframe);
    if (err_is_ok(err)) {
        // Will respond to client when we get the reply from spawnd.
        return;
    } else {
        DEBUG_ERR(err, "sending span request");
        pending_clients_release(domain_cap, NULL);
        err = err_push(err, PROC_MGMT_ERR_SPAWND_REQUEST);
    }

respond_with_err:
    resp_err = b->tx_vtbl.span_response(b, NOP_CONT, err);
    if (err_is_fail(resp_err)) {
        DEBUG_ERR(resp_err, "failed to send span_response");
    }
}

static errval_t kill_handler_common(struct proc_mgmt_binding *b,
                                    struct capref domain_cap,
                                    enum ClientType type,
                                    uint8_t exit_status)
{
    errval_t err = pending_clients_add(domain_cap, b, type, MAX_COREID);
    if (err_is_fail(err)) {
        return err;
    }

    struct domain_entry *entry;
    err = domain_get_by_cap(domain_cap, &entry);
    if (err_is_fail(err)) {
        return err;
    }

    entry->exit_status = exit_status;
    domain_stop_pending(entry);

    for (coreid_t i = 0; i < MAX_COREID; ++i) {
        if (entry->spawnds[i] == NULL) {
            continue;
        }

        struct spawn_binding *spb = entry->spawnds[i]->b;
        spb->rx_vtbl.spawn_reply = spawn_reply_handler;
        errval_t req_err = spb->tx_vtbl.kill_request(spb, NOP_CONT, cap_procmng,
                                                     domain_cap);
        if (err_is_fail(req_err)) {
            DEBUG_ERR(req_err, "failed to send kill_request to spawnd %u\n", i);
        }
    }

    return SYS_ERR_OK;
}

static void kill_handler(struct proc_mgmt_binding *b, struct capref domain_cap)
{
    errval_t err = kill_handler_common(b, domain_cap, ClientType_Kill,
                                       EXIT_STATUS_KILLED);
    if (err_is_fail(err)) {
        errval_t resp_err = b->tx_vtbl.kill_response(b, NOP_CONT, err);
        if (err_is_fail(resp_err)) {
            DEBUG_ERR(resp_err, "failed to send kill_response");
        }
    }
}

static void exit_handler(struct proc_mgmt_binding *b, struct capref domain_cap,
                         uint8_t exit_status)
{
    errval_t err = kill_handler_common(b, domain_cap, ClientType_Exit,
                                       exit_status);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "processing exit_handler for requesting domain, exit "
                  "code %u", exit_status);
    }
    // Error or not, there's no client to reply to anymore.
}

static void wait_handler(struct proc_mgmt_binding *b, struct capref domain_cap)
{
    errval_t err, resp_err;
    struct domain_entry *entry;
    err = domain_get_by_cap(domain_cap, &entry);
    if (err_is_fail(err)) {
        goto respond;
    }

    if (entry->status == DOMAIN_STATUS_STOPPED) {
        // Domain has already been stopped, so just reply with exit status.
        goto respond;
    }

    struct domain_waiter *waiter = (struct domain_waiter*) malloc(
            sizeof(struct domain_waiter));
    waiter->b = b;
    waiter->next = entry->waiters;
    entry->waiters = waiter;
    // Will respond when domain is stopped.
    return;

respond:
    resp_err = b->tx_vtbl.wait_response(b, NOP_CONT, err, entry->exit_status);
    if (err_is_fail(resp_err)) {
        DEBUG_ERR(resp_err, "failed to send wait_response");
    }
}

static struct proc_mgmt_rx_vtbl monitor_vtbl = {
    .add_spawnd           = add_spawnd_handler,
    .spawn_call           = spawn_handler,
    .spawn_with_caps_call = spawn_with_caps_handler,
    .span_call            = span_handler,
    .kill_call            = kill_handler,
    .exit                 = exit_handler,
    .wait_call            = wait_handler
};

static struct proc_mgmt_rx_vtbl non_monitor_vtbl = {
    .add_spawnd           = add_spawnd_handler_non_monitor,
    .spawn_call           = spawn_handler,
    .spawn_with_caps_call = spawn_with_caps_handler,
    .span_call            = span_handler,
    .kill_call            = kill_handler,
    .exit                 = exit_handler,
    .wait_call            = wait_handler
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
