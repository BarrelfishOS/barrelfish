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
#include <barrelfish/spawn_client.h>
#include <if/monitor_defs.h>
#include <if/proc_mgmt_defs.h>
#include <if/spawn_defs.h>

#include "domain.h"
#include "internal.h"
#include "pending_clients.h"
#include "spawnd_state.h"

/**
 * \brief Handler for message add_spawnd, for the local monitor binding.
 */
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

/**
 * \brief Handler for message add_spawnd, for non-monitor bindings.
 */
static void add_spawnd_handler_non_monitor(struct proc_mgmt_binding *b,
                                           coreid_t core_id, iref_t iref)
{
    // debug_printf("Ignoring add_spawnd call: %s\n",
    //              err_getstring(PROC_MGMT_ERR_NOT_MONITOR));
}

static bool cleanup_request_sender(struct msg_queue_elem *m);

/**
 * General-purpose handler for replies from spawnd.
 */
static void spawn_reply_handler(struct spawn_binding *b, errval_t spawn_err)
{
    struct pending_client *cl =
            (struct pending_client*) spawnd_state_dequeue_recv(b->st);

    struct pending_spawn *spawn = NULL;
    struct pending_span *span = NULL;
    struct pending_kill_cleanup *kc = NULL;

    struct domain_entry *entry;
    
    errval_t err, resp_err;

    switch (cl->type) {
        case ClientType_Spawn:
        case ClientType_SpawnWithCaps:
            spawn = (struct pending_spawn*) cl->st;
            err = spawn_err;
            if (err_is_ok(spawn_err)) {
                err = domain_spawn(spawn->cap_node, spawn->core_id, spawn->argvbuf,
                                   spawn->argvbytes);
                if (cl->type == ClientType_Spawn) {
                    resp_err = cl->b->tx_vtbl.spawn_response(cl->b, NOP_CONT,
                            err, spawn->cap_node->domain_cap);
                } else {
                    resp_err = cl->b->tx_vtbl.spawn_with_caps_response(cl->b,
                            NOP_CONT, err, spawn->cap_node->domain_cap);
                }
            }

            free(spawn);
            break;

        case ClientType_Span:
            span = (struct pending_span*) cl->st;
            entry = span->entry;
            if (entry->status == DOMAIN_STATUS_RUNNING) {
                resp_err = cl->b->tx_vtbl.span_response(cl->b, NOP_CONT,
                                                        spawn_err);
            }

            free(span);
            break;

        case ClientType_Cleanup:
            kc = (struct pending_kill_cleanup*) cl->st;
            entry = kc->entry;

            assert(entry->num_spawnds_resources > 0);
            assert(entry->status != DOMAIN_STATUS_CLEANED);

            --entry->num_spawnds_resources;
            if (entry->num_spawnds_resources == 0) {
                entry->status = DOMAIN_STATUS_CLEANED;

                // At this point, the domain exists in state CLEANED for history
                // reasons. For instance, if some other domain issues a wait
                // call for this one, the process manager can return the exit
                // status directly. At some point, however, we might want to
                // just clean up the domain entry and recycle the domain cap.
            }

            free(kc);
            break;

        case ClientType_Kill:
        case ClientType_Exit:
            kc = (struct pending_kill_cleanup*) cl->st;
            entry = kc->entry;

            assert(entry->num_spawnds_running > 0);
            assert(entry->status != DOMAIN_STATUS_STOPPED);

            --entry->num_spawnds_running;

            if (entry->num_spawnds_running == 0) {
                entry->status = DOMAIN_STATUS_STOPPED;

                if (cl->type == ClientType_Kill) {
                    entry->exit_status = EXIT_STATUS_KILLED;
                    resp_err = cl->b->tx_vtbl.kill_response(cl->b, NOP_CONT,
                                                            spawn_err);
                }

                struct domain_waiter *waiter = entry->waiters;
                while (waiter != NULL) {
                    waiter->b->tx_vtbl.wait_response(waiter->b, NOP_CONT,
                                                     SYS_ERR_OK,
                                                     entry->exit_status);
                    struct domain_waiter *tmp = waiter;
                    waiter = waiter->next;
                    free(tmp);
                }

                for (coreid_t i = 0; i < MAX_COREID; ++i) {
                    if (entry->spawnds[i] == NULL) {
                        continue;
                    }

                    struct spawn_binding *spb = entry->spawnds[i]->b;

                    struct pending_kill_cleanup *cleanup =
                            (struct pending_kill_cleanup*) malloc(
                                    sizeof(struct pending_kill_cleanup));
                    cleanup->b = spb;
                    cleanup->domain_cap = kc->domain_cap;
                    cleanup->entry = entry;

                    struct pending_client *cleanup_cl =
                            (struct pending_client*) malloc(
                                    sizeof(struct pending_client));
                    cleanup_cl->b = cl->b;
                    cleanup_cl->type = ClientType_Cleanup;
                    cleanup_cl->st = cleanup;

                    struct msg_queue_elem *msg = (struct msg_queue_elem*) malloc(
                            sizeof(struct msg_queue_elem));
                    msg->st = cleanup_cl;
                    msg->cont = cleanup_request_sender;

                    err = spawnd_state_enqueue_send(entry->spawnds[i], msg);

                    if (err_is_fail(err)) {
                        DEBUG_ERR(err, "enqueuing cleanup request");
                        free(cleanup);
                        free(cleanup_cl);
                        free(msg);
                    }
                }
            }

            free(kc);
            break;

        default:
            USER_PANIC("Unknown client type in spawn_reply_handler: %u\n",
                       cl->type);
    }

    free(cl);
}

/**
 * \brief Handler for sending spawn requests.
 */
static bool spawn_request_sender(struct msg_queue_elem *m)
{
    struct pending_client *cl = (struct pending_client*) m->st;
    struct pending_spawn *spawn = (struct pending_spawn*) cl->st;
    spawn->b->rx_vtbl.spawn_reply = spawn_reply_handler;

    errval_t err;
    bool with_caps = !(capref_is_null(spawn->inheritcn_cap) &&
                       capref_is_null(spawn->argcn_cap));
    if (with_caps) {
        err = spawn->b->tx_vtbl.spawn_with_caps_request(spawn->b, NOP_CONT,
                                                        cap_procmng,
                                                        spawn->cap_node->domain_cap,
                                                        spawn->path,
                                                        spawn->argvbuf,
                                                        spawn->argvbytes,
                                                        spawn->envbuf,
                                                        spawn->envbytes,
                                                        spawn->inheritcn_cap,
                                                        spawn->argcn_cap,
                                                        spawn->flags);
    } else {
        err = spawn->b->tx_vtbl.spawn_request(spawn->b, NOP_CONT, cap_procmng,
                                              spawn->cap_node->domain_cap,
                                              spawn->path, spawn->argvbuf,
                                              spawn->argvbytes, spawn->envbuf,
                                              spawn->envbytes, spawn->flags);
    }

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            return false;
        } else {
            USER_PANIC_ERR(err, "sending spawn request");
        }
    }

    free(m);

    return true;
}

/**
 * \brief Handler for sending span requests.
 */
static bool span_request_sender(struct msg_queue_elem *m)
{
    struct pending_client *cl = (struct pending_client*) m->st;
    struct pending_span *span = (struct pending_span*) cl->st;

    errval_t err;
    span->b->rx_vtbl.spawn_reply = spawn_reply_handler;
    err = span->b->tx_vtbl.span_request(span->b, NOP_CONT, cap_procmng,
                                        span->domain_cap, span->vroot,
                                        span->dispframe);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            return false;
        } else {
            USER_PANIC_ERR(err, "sending span request");
        }
    }

    free(m);

    return true;
}

/**
 * \brief Handler for sending kill requests.
 */
static bool kill_request_sender(struct msg_queue_elem *m)
{
    struct pending_client *cl = (struct pending_client*) m->st;
    struct pending_kill_cleanup *kill = (struct pending_kill_cleanup*) cl->st;

    errval_t err;
    kill->b->rx_vtbl.spawn_reply = spawn_reply_handler;
    err = kill->b->tx_vtbl.kill_request(kill->b, NOP_CONT, cap_procmng,
                                        kill->domain_cap);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            return false;
        } else {
            USER_PANIC_ERR(err, "sending kill request");
        }
    }

    free(m);

    return true;
}

/**
 * \brief Handler for sending cleanup requests.
 */
static bool cleanup_request_sender(struct msg_queue_elem *m)
{
    struct pending_client *cl = (struct pending_client*) m->st;
    struct pending_kill_cleanup *cleanup = (struct pending_kill_cleanup*) cl->st;

    errval_t err;
    cleanup->b->rx_vtbl.spawn_reply = spawn_reply_handler;
    err = cleanup->b->tx_vtbl.cleanup_request(cleanup->b, NOP_CONT,
                                              cap_procmng,
                                              cleanup->domain_cap);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            return false;
        } else {
            USER_PANIC_ERR(err, "sending cleanup request");
        }
    }

    free(m);

    return true;
}

/**
 * \brief Common bits of the spawn and spawn_with_caps handlers.
 */
static errval_t spawn_handler_common(struct proc_mgmt_binding *b,
                                     enum ClientType type,
                                     coreid_t core_id, const char *path,
                                     const char *argvbuf, size_t argvbytes,
                                     const char *envbuf, size_t envbytes,
                                     struct capref inheritcn_cap,
                                     struct capref argcn_cap, uint8_t flags)
{
    if (!spawnd_state_exists(core_id)) {
        // XXX fixes race condition for between proc_mgmt and spawnd for 
        // now, but is a problem when spawnd on a certain core is not started 
        // because the cpu driver on that core is not started
        while(!spawnd_state_exists(core_id)) {
            event_dispatch(get_default_waitset());
        }
        //return PROC_MGMT_ERR_INVALID_SPAWND;
    }

    struct spawnd_state *spawnd = spawnd_state_get(core_id);
    assert(spawnd != NULL);
    struct spawn_binding *cl = spawnd->b;
    assert(cl != NULL);

    errval_t err;
    if (domain_should_refill_caps()) {
        err = domain_prealloc_caps();
        if (err_is_fail(err)) {
            return err_push(err, PROC_MGMT_ERR_CREATE_DOMAIN_CAP);
        }
    }

    struct domain_cap_node *cap_node = next_cap_node();

    struct pending_spawn *spawn = (struct pending_spawn*) malloc(
            sizeof(struct pending_spawn));
    spawn->cap_node = cap_node;
    // spawn->domain_cap = domain_cap;
    spawn->b = cl;
    spawn->core_id = core_id;
    spawn->path = path;
    spawn->argvbuf = argvbuf;
    spawn->argvbytes = argvbytes;
    spawn->envbuf = envbuf;
    spawn->envbytes = envbytes;
    spawn->inheritcn_cap = inheritcn_cap;
    spawn->argcn_cap = argcn_cap;
    spawn->flags = flags;

    struct pending_client *spawn_cl = (struct pending_client*) malloc(
            sizeof(struct pending_client));
    spawn_cl->b = b;
    spawn_cl->type = type;
    spawn_cl->st = spawn;

    struct msg_queue_elem *msg = (struct msg_queue_elem*) malloc(
            sizeof(struct msg_queue_elem));
    msg->st = spawn_cl;
    msg->cont = spawn_request_sender;

    err = spawnd_state_enqueue_send(spawnd, msg);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "enqueuing spawn request");
        free(spawn);
        free(spawn_cl);
        free(msg);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Handler for rpc spawn.
 */
static void spawn_handler(struct proc_mgmt_binding *b, coreid_t core_id,
                          const char *path, const char *argvbuf,
                          size_t argvbytes, const char *envbuf, size_t envbytes,
                          uint8_t flags)
{
    errval_t err, resp_err;
    err = spawn_handler_common(b, ClientType_Spawn, core_id, path, argvbuf,
                               argvbytes, envbuf, envbytes, NULL_CAP, NULL_CAP,
                               flags);

    if (err_is_fail(err)) {
        resp_err = b->tx_vtbl.spawn_response(b, NOP_CONT, err, NULL_CAP);
        if (err_is_fail(resp_err)) {
            DEBUG_ERR(resp_err, "failed to send spawn_response");
        }
    }
}

/**
 * \brief Handler for rpc spawn_with_caps.
 */
static void spawn_with_caps_handler(struct proc_mgmt_binding *b,
                                    coreid_t core_id, const char *path,
                                    const char *argvbuf, size_t argvbytes,
                                    const char *envbuf, size_t envbytes,
                                    struct capref inheritcn_cap,
                                    struct capref argcn_cap, uint8_t flags)
{
    errval_t err, resp_err;
    err = spawn_handler_common(b, ClientType_SpawnWithCaps, core_id, path,
                               argvbuf, argvbytes, envbuf, envbytes,
                               inheritcn_cap, argcn_cap, flags);
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

/**
 * \brief Handler for rpc span.
 */
static void span_handler(struct proc_mgmt_binding *b, struct capref domain_cap,
                         coreid_t core_id, struct capref vroot,
                         struct capref dispframe)
{
    errval_t err, resp_err;
    struct domain_entry *entry = NULL;
    err = domain_get_by_cap(domain_cap, &entry);
    if (err_is_fail(err)) {
        goto respond_with_err;
    }

    assert(entry != NULL);
    if (entry->status != DOMAIN_STATUS_RUNNING) {
        err = PROC_MGMT_ERR_DOMAIN_NOT_RUNNING;
        goto respond_with_err;
    }

    if (entry->spawnds[core_id] != NULL) {
        // TODO(razvan): Maybe we want to allow the same domain to span multiple
        // dispatchers onto the same core?
        err = PROC_MGMT_ERR_ALREADY_SPANNED;
        goto respond_with_err;
    }

    if (!spawnd_state_exists(core_id)) {
        err = PROC_MGMT_ERR_INVALID_SPAWND;
        goto respond_with_err;
    }

    struct spawnd_state *spawnd = spawnd_state_get(core_id);
    assert(spawnd != NULL);
    struct spawn_binding *cl = spawnd->b;
    assert(cl != NULL);

    struct pending_span *span = (struct pending_span*) malloc(
            sizeof(struct pending_span));
    span->domain_cap = domain_cap;
    span->entry = entry;
    span->b = cl;
    span->core_id = core_id;
    span->vroot = vroot;
    span->dispframe = dispframe;

    struct pending_client *span_cl = (struct pending_client*) malloc(
            sizeof(struct pending_client));
    span_cl->b = b;
    span_cl->type = ClientType_Span;
    span_cl->st = span;

    struct msg_queue_elem *msg = (struct msg_queue_elem*) malloc(
            sizeof(struct msg_queue_elem));
    msg->st = span_cl;
    msg->cont = span_request_sender;

    err = spawnd_state_enqueue_send(spawnd, msg);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "enqueuing span request");
        free(span);
        free(span_cl);
        free(msg);
    }

respond_with_err:
    resp_err = b->tx_vtbl.span_response(b, NOP_CONT, err);
    if (err_is_fail(resp_err)) {
        DEBUG_ERR(resp_err, "failed to send span_response");
    }
}

/**
 * \brief Common bits of the kill and exit handlers.
 */
static errval_t kill_handler_common(struct proc_mgmt_binding *b,
                                    struct capref domain_cap,
                                    enum ClientType type,
                                    uint8_t exit_status)
{
    struct domain_entry *entry;
    errval_t err = domain_get_by_cap(domain_cap, &entry);
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

        struct pending_kill_cleanup *cmd = (struct pending_kill_cleanup*) malloc(
                sizeof(struct pending_kill_cleanup));
        cmd->domain_cap = domain_cap;
        cmd->entry = entry;
        cmd->b = spb;

        struct pending_client *cl = (struct pending_client*) malloc(
                sizeof(struct pending_client));
        cl->b = b;
        cl->type = type;
        cl->st = cmd;

        struct msg_queue_elem *msg = (struct msg_queue_elem*) malloc(
                sizeof(struct msg_queue_elem));
        msg->st = cl;
        msg->cont = kill_request_sender;

        err = spawnd_state_enqueue_send(entry->spawnds[i], msg);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "enqueuing kill request");
            free(cmd);
            free(cl);
            free(msg);
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief Handler for rpc kill.
 */
static void kill_handler(struct proc_mgmt_binding *b,
                         struct capref victim_domain_cap)
{
    errval_t err = kill_handler_common(b, victim_domain_cap, ClientType_Kill,
                                       EXIT_STATUS_KILLED);
    if (err_is_fail(err)) {
        errval_t resp_err = b->tx_vtbl.kill_response(b, NOP_CONT, err);
        if (err_is_fail(resp_err)) {
            DEBUG_ERR(resp_err, "failed to send kill_response");
        }
    }
}

/**
 * \brief Handler for message exit.
 */
static void exit_handler(struct proc_mgmt_binding *b, struct capref domain_cap,
                         uint8_t exit_status)
{
    errval_t err = kill_handler_common(b, domain_cap, ClientType_Exit,
                                       exit_status);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "processing exit_handler for requesting domain, exit "
                  "code %u", exit_status);
    }
    // Error or not, there's no client to respond to anymore.
}

/**
 * \brief Handler for rpc wait.
 */
static void wait_handler(struct proc_mgmt_binding *b, struct capref domain_cap, bool nohang)
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

    if (nohang) {
        entry->exit_status = -1;
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

/**
 * \brief Handler for rpc get_domainlist.
 */
static void get_domainlist_handler(struct proc_mgmt_binding *b)
{
    errval_t resp_err;
    size_t len;
    domainid_t* domains;

    domain_get_all_ids(&domains, &len);

    // 4096 hardcoded limit in flounder interface
    assert(sizeof(domainid_t)/sizeof(uint8_t)*len < 4096);

    resp_err = b->tx_vtbl.get_domainlist_response(b, NOP_CONT, (uint8_t*) domains, 
                                                  sizeof(domainid_t)/sizeof(uint8_t)*len);
    if (err_is_fail(resp_err)) {
        DEBUG_ERR(resp_err, "failed to send wait_response");
    }
}

/**
 * \brief Handler for rpc get_status.
 */
static void get_status_handler(struct proc_mgmt_binding *b, domainid_t domain)
{
    errval_t err;
    struct domain_entry* entry;
    proc_mgmt_ps_entry_t pse;
    memset(&pse, 0, sizeof(pse));

    err = domain_get_by_id(domain, &entry);
    if (err_is_fail(err)) {
        err = b->tx_vtbl.get_status_response(b, NOP_CONT, pse, NULL, 0,
                                             err);
        if(err_is_fail(err)) {
            DEBUG_ERR(err, "status_response");
        }
    }

    pse.status = entry->status;

    err = b->tx_vtbl.get_status_response(b, NOP_CONT, pse, entry->argbuf, entry->argbytes,
                                         SYS_ERR_OK);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "status_response");
    }
}

static struct proc_mgmt_rx_vtbl monitor_vtbl = {
    .add_spawnd           = add_spawnd_handler,
    .spawn_call           = spawn_handler,
    .spawn_with_caps_call = spawn_with_caps_handler,
    .span_call            = span_handler,
    .kill_call            = kill_handler,
    .exit_call            = exit_handler,
    .wait_call            = wait_handler
};

static struct proc_mgmt_rx_vtbl non_monitor_vtbl = {
    .add_spawnd           = add_spawnd_handler_non_monitor,
    .spawn_call           = spawn_handler,
    .spawn_with_caps_call = spawn_with_caps_handler,
    .span_call            = span_handler,
    .kill_call            = kill_handler,
    .exit_call            = exit_handler,
    .wait_call            = wait_handler,
    .get_domainlist_call  = get_domainlist_handler,
    .get_status_call      = get_status_handler
};

/**
 * \brief Allocates a special LMP endpoint for authenticating with the monitor.
 */
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
    errval_t err = domain_prealloc_caps();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err_push(err, PROC_MGMT_ERR_CREATE_DOMAIN_CAP),
                       "domain_prealloc_caps in start_service");
    }

    return proc_mgmt_export(NULL, export_cb, connect_cb, get_default_waitset(),
            IDC_EXPORT_FLAGS_DEFAULT);
}
