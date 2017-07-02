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
                                struct capref domain_cap, errval_t spawn_err);
static void spawn_with_caps_reply_handler(struct spawn_binding *b,
                                          struct capref domain_cap,
                                          errval_t spawn_err);
static void span_reply_handler(struct spawn_binding *b,
                               struct capref domain_cap, errval_t span_err);
static void kill_reply_handler(struct spawn_binding *b,
                               struct capref domain_cap, errval_t kill_err);
static void exit_reply_handler(struct spawn_binding *b,
                               struct capref domain_cap, errval_t exit_err);
static void cleanup_reply_handler(struct spawn_binding *b,
                                  struct capref domain_cap,
                                  errval_t cleanup_err);

static void spawn_request_sender(void *arg)
{
    struct pending_spawn *spawn = (struct pending_spawn*) arg;

    errval_t err;
    bool with_caps = !(capref_is_null(spawn->inheritcn_cap) &&
                       capref_is_null(spawn->argcn_cap));
    if (with_caps) {
        spawn->b->rx_vtbl.spawn_with_caps_reply = spawn_with_caps_reply_handler;
        err = spawn->b->tx_vtbl.spawn_with_caps_request(spawn->b, NOP_CONT,
                                                        cap_procmng,
                                                        spawn->domain_cap,
                                                        spawn->path,
                                                        spawn->argvbuf,
                                                        spawn->argvbytes,
                                                        spawn->envbuf,
                                                        spawn->envbytes,
                                                        spawn->inheritcn_cap,
                                                        spawn->argcn_cap,
                                                        spawn->flags);
    } else {
        spawn->b->rx_vtbl.spawn_reply = spawn_reply_handler;
        err = spawn->b->tx_vtbl.spawn_request(spawn->b, NOP_CONT, cap_procmng,
                                              spawn->domain_cap, spawn->path,
                                              spawn->argvbuf, spawn->argvbytes,
                                              spawn->envbuf, spawn->envbytes,
                                              spawn->flags);
    }
    if (err_is_ok(err)) {
        free(spawn);
    } else {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = spawn->b->register_send(spawn->b, spawn->b->waitset,
                                          MKCONT(spawn_request_sender, arg));
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "registering for spawn request");
                pending_clients_release(spawn->domain_cap,
                                        with_caps ? ClientType_SpawnWithCaps
                                                  : ClientType_Spawn,
                                        NULL);
                event_mutex_unlock(&spawn->b->mutex);
                free(spawn);
            }
        } else {
            DEBUG_ERR(err, "sending spawn request");
            pending_clients_release(spawn->domain_cap,
                                    with_caps ? ClientType_SpawnWithCaps
                                              : ClientType_Spawn,
                                    NULL);
            event_mutex_unlock(&spawn->b->mutex);
            free(spawn);
        }
    }
}

static void span_request_sender(void *arg)
{
    struct pending_span *span = (struct pending_span*) arg;

    errval_t err;
    span->b->rx_vtbl.span_reply = span_reply_handler;
    err = span->b->tx_vtbl.span_request(span->b, NOP_CONT, cap_procmng,
                                        span->domain_cap, span->vroot,
                                        span->dispframe);
    if (err_is_ok(err)) {
        err = domain_span(span->domain_cap, span->core_id);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed domain_span to core %u\n", span->core_id);
        }
        free(span);
    } else {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = span->b->register_send(span->b, span->b->waitset,
                                         MKCONT(span_request_sender, arg));
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "registering for span request");
                pending_clients_release(span->domain_cap, ClientType_Span,
                                        NULL);
                event_mutex_unlock(&span->b->mutex);
                free(span);
            }
        } else {
            DEBUG_ERR(err, "sending span request");
            pending_clients_release(span->domain_cap, ClientType_Span, NULL);
            event_mutex_unlock(&span->b->mutex);
            free(span);
        }
    }
}

static void kill_request_sender(void *arg)
{
    struct pending_kill_exit_cleanup *kill = (struct pending_kill_exit_cleanup*) arg;

    errval_t err;
    kill->sb->rx_vtbl.kill_reply = kill_reply_handler;
    err = kill->sb->tx_vtbl.kill_request(kill->sb, NOP_CONT, cap_procmng,
                                        kill->domain_cap);
    if (err_is_ok(err)) {
        free(kill);
    } else {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = kill->sb->register_send(kill->sb, kill->sb->waitset,
                                         MKCONT(kill_request_sender, arg));
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "registering for kill request");

                struct pending_client *cl;
                err = pending_clients_release_one(kill->domain_cap,
                                                  ClientType_Kill,
                                                  kill->pmb, &cl);
                if (err_is_ok(err)) {
                    while (cl != NULL) {
                        struct pending_client *tmp = cl;
                        cl = cl->next;
                        free(tmp);
                    }
                }

                event_mutex_unlock(&kill->sb->mutex);
                free(kill);
            }
        } else {
            DEBUG_ERR(err, "sending kill request");
            
            struct pending_client *cl;
            err = pending_clients_release_one(kill->domain_cap,
                                              ClientType_Kill,
                                              kill->pmb, &cl);
            if (err_is_ok(err)) {
                while (cl != NULL) {
                    struct pending_client *tmp = cl;
                    cl = cl->next;
                    free(tmp);
                }
            }

            event_mutex_unlock(&kill->sb->mutex);
            free(kill);
        }
    }
}

static void exit_request_sender(void *arg)
{
    struct pending_kill_exit_cleanup *exit = (struct pending_kill_exit_cleanup*) arg;

    errval_t err;
    exit->sb->rx_vtbl.exit_reply = exit_reply_handler;
    err = exit->sb->tx_vtbl.exit_request(exit->sb, NOP_CONT, cap_procmng,
                                        exit->domain_cap);
    if (err_is_ok(err)) {
        free(exit);
    } else {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = exit->sb->register_send(exit->sb, exit->sb->waitset,
                                         MKCONT(exit_request_sender, arg));
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "registering for exit request");
                err = pending_clients_release(exit->domain_cap, ClientType_Exit,
                                              NULL);
                event_mutex_unlock(&exit->sb->mutex);
                free(exit);
            }
        } else {
            DEBUG_ERR(err, "sending exit request");
            err = pending_clients_release(exit->domain_cap, ClientType_Exit,
                                          NULL);
            event_mutex_unlock(&exit->sb->mutex);
            free(exit);
        }
    }
}

static void cleanup_request_sender(void *arg)
{
    struct pending_kill_exit_cleanup *cleanup = (struct pending_kill_exit_cleanup*) arg;

    errval_t err;
    cleanup->sb->rx_vtbl.cleanup_reply = cleanup_reply_handler;
    err = cleanup->sb->tx_vtbl.cleanup_request(cleanup->sb, NOP_CONT, cap_procmng,
                                              cleanup->domain_cap);
    if (err_is_ok(err)) {
        free(cleanup);
    } else {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = cleanup->sb->register_send(cleanup->sb, cleanup->sb->waitset,
                                            MKCONT(cleanup_request_sender, arg));
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "registering for cleanup request");
                pending_clients_release(cleanup->domain_cap, ClientType_Cleanup,
                                        NULL);
                event_mutex_unlock(&cleanup->sb->mutex);
                free(cleanup);
            }
        } else {
            DEBUG_ERR(err, "sending cleanup request");
            pending_clients_release(cleanup->domain_cap, ClientType_Cleanup,
                                    NULL);
            event_mutex_unlock(&cleanup->sb->mutex);
            free(cleanup);
        }
    }
}

static void spawn_reply_handler(struct spawn_binding *b,
                                struct capref domain_cap, errval_t spawn_err)
{
    event_mutex_unlock(&b->mutex);

    struct pending_client *cl;
    errval_t err = pending_clients_release(domain_cap, ClientType_Spawn, &cl);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to retrieve pending spawn client based on domain"
                  " cap");
        return;
    }

    err = spawn_err;
    if (err_is_ok(spawn_err)) {
        err = domain_spawn(domain_cap, cl->core_id);
    }

    errval_t resp_err = cl->b->tx_vtbl.spawn_response(cl->b, NOP_CONT, err,
                                                      domain_cap);
    if (err_is_fail(resp_err)) {
        DEBUG_ERR(resp_err, "failed to send spawn_response to client");
    }
    
    free(cl);
}

static void spawn_with_caps_reply_handler(struct spawn_binding *b,
                                          struct capref domain_cap,
                                          errval_t spawn_err)
{
    event_mutex_unlock(&b->mutex);
    
    struct pending_client *cl;
    errval_t err = pending_clients_release(domain_cap, ClientType_SpawnWithCaps,
                                           &cl);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to retrieve pending spawn_with_caps client based"
                  " on domain cap");
        return;
    }

    err = spawn_err;
    if (err_is_ok(spawn_err)) {
        err = domain_spawn(domain_cap, cl->core_id);
    }

    errval_t resp_err = cl->b->tx_vtbl.spawn_with_caps_response(cl->b, NOP_CONT,
                                                                err,
                                                                domain_cap);
    if (err_is_fail(resp_err)) {
        DEBUG_ERR(resp_err, "failed to send spawn_with_caps_response to "
                  "client");
    }
    
    free(cl);
}

static void span_reply_handler(struct spawn_binding *b,
                               struct capref domain_cap, errval_t span_err)
{
    event_mutex_unlock(&b->mutex);

    struct pending_client *cl;
    errval_t err = pending_clients_release(domain_cap, ClientType_Span, &cl);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to retrieve pending span client based on domain"
                  " cap");
        return;
    }

    struct domain_entry *entry;
    err = domain_get_by_cap(cl->domain_cap, &entry);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to retrieve span client by domain cap");
        return;
    }

    if (entry->status != DOMAIN_STATUS_RUNNING) {
        // Domain has been stopped while we were serving the request; there's
        // no one to respond to.
        free(cl);
        return;
    }

    err = cl->b->tx_vtbl.span_response(cl->b, NOP_CONT, span_err);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to send span_response to client");
    }
    
    free(cl);
}

static void cleanup_reply_handler(struct spawn_binding *b,
                                  struct capref domain_cap,
                                  errval_t cleanup_err)
{
    event_mutex_unlock(&b->mutex);

    struct pending_client *cl;
    errval_t err = pending_clients_release(domain_cap, ClientType_Cleanup, &cl);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to retrieve pending cleanup client based on "
                  "domain cap");
        return;
    }

    if (err_is_fail(cleanup_err)) {
        // TODO(razvan): Here, spawnd has failed deleting its local cspace.
        // Should we send another cleanup message, until it might succeed?
        free(cl);
        return;
    }

    struct domain_entry *entry;
    err = domain_get_by_cap(domain_cap, &entry);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to retrieve domain by cap returned by spawnd "
                  "after cleanup");
        return;
    }

    assert(entry->num_spawnds_resources > 0);
    assert(entry->status != DOMAIN_STATUS_CLEANED);

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
                                  ClientType_Cleanup, MAX_COREID);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "pending_clients_add in cleanup_reply_handler");
        }
    }
}

static void kill_reply_handler(struct spawn_binding *b,
                               struct capref domain_cap, errval_t kill_err)
{
    event_mutex_unlock(&b->mutex);

    struct pending_client *cl;
    errval_t err = pending_clients_release(domain_cap, ClientType_Kill, &cl);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to retrieve pending kill client based on domain "
                  "cap");
        return;
    }

    errval_t resp_err;
    if (err_is_fail(kill_err)) {
        // TODO(razvan): Here, spawnd has failed deleting its local dispatcher.
        // Should we send another kill message, until it might succeed?
        while (cl != NULL) {
            resp_err = cl->b->tx_vtbl.kill_response(cl->b, NOP_CONT,
                                                     kill_err);
            if (err_is_fail(resp_err)) {
                DEBUG_ERR(resp_err, "failed to send kill_response to client");
            }
            struct pending_client *tmp = cl;
            cl = cl->next;
            free(tmp);
        }
        return;
    }

    struct domain_entry *entry;
    err = domain_get_by_cap(domain_cap, &entry);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to retrieve domain by cap returned by spawnd "
                  "after kill");
        return;
    }

    assert(entry->num_spawnds_running > 0);
    assert(entry->status != DOMAIN_STATUS_STOPPED);

    --entry->num_spawnds_running;

    if (entry->num_spawnds_running == 0) {
        entry->status = DOMAIN_STATUS_STOPPED;
        entry->exit_status = EXIT_STATUS_KILLED;

        err = pending_clients_add(domain_cap, NULL, ClientType_Cleanup,
                                  MAX_COREID);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "pending_clients_add in kill_reply_handler");
        }

        // TODO(razvan): Might it be more sane if we respond back
        // to the client after the domain has been cleaned up (i.e.
        // the cspace root has been revoked for all dispatchers)?
        while (cl != NULL) {
            resp_err = cl->b->tx_vtbl.kill_response(cl->b, NOP_CONT,
                                                     kill_err);
            if (err_is_fail(resp_err)) {
                DEBUG_ERR(resp_err, "failed to send kill_response to client");
            }
            struct pending_client *tmp = cl;
            cl = cl->next;
            free(tmp);
        }
        
        // TODO(razvan): Same problem applies to the waiters: would
        // it be better if we sent them wait_responses after the
        // cspace root has been revoked, too? (here and in the exit
        // case).
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

            struct pending_kill_exit_cleanup *cleanup = (struct pending_kill_exit_cleanup*) malloc(
                    sizeof(struct pending_kill_exit_cleanup));
            cleanup->sb = spb;
            cleanup->domain_cap = domain_cap;

            spb->rx_vtbl.cleanup_reply = cleanup_reply_handler;
            event_mutex_enqueue_lock(&spb->mutex,
                                     &cleanup->qn,
                                     (struct event_closure) {
                                         .handler = cleanup_request_sender,
                                         .arg = cleanup });
        }
    } else {
        err = pending_clients_add(domain_cap, cl->b, ClientType_Kill,
                                  MAX_COREID);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "pending_clients_add in kill_reply_handler");
        }
    }
}

static void exit_reply_handler(struct spawn_binding *b,
                               struct capref domain_cap, errval_t exit_err)
{
    event_mutex_unlock(&b->mutex);

    struct pending_client *cl;
    errval_t err = pending_clients_release(domain_cap, ClientType_Exit, &cl);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to retrieve pending exit client based on domain "
                  "cap");
        return;
    }

    if (err_is_fail(exit_err)) {
        // TODO(razvan): Here, spawnd has failed deleting its local dispatcher.
        // Should we send another kill message, until it might succeed?
        free(cl);
        return;
    }

    struct domain_entry *entry;
    err = domain_get_by_cap(domain_cap, &entry);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to retrieve domain by cap returned by spawnd "
                  "after exit");
        return;
    }

    assert(entry->num_spawnds_running > 0);
    assert(entry->status != DOMAIN_STATUS_STOPPED);

    --entry->num_spawnds_running;

    if (entry->num_spawnds_running == 0) {
        entry->status = DOMAIN_STATUS_STOPPED;
        
        free(cl);

        // TODO(razvan): Same problem applies to the waiters: would
        // it be better if we sent them wait_responses after the
        // cspace root has been revoked, too? (here and in the exit
        // case).
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

            struct pending_kill_exit_cleanup *cleanup = (struct pending_kill_exit_cleanup*) malloc(
                    sizeof(struct pending_kill_exit_cleanup));
            cleanup->sb = spb;
            cleanup->domain_cap = domain_cap;

            spb->rx_vtbl.cleanup_reply = cleanup_reply_handler;
            event_mutex_enqueue_lock(&spb->mutex,
                                     &cleanup->qn,
                                     (struct event_closure) {
                                         .handler = cleanup_request_sender,
                                         .arg = cleanup });
        }
    } else {
        err = pending_clients_add(domain_cap, cl->b, ClientType_Exit,
                                  MAX_COREID);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "pending_clients_add in kill_reply_handler");
        }
    }
}

static errval_t spawn_handler_common(struct proc_mgmt_binding *b,
                                     enum ClientType type,
                                     coreid_t core_id, const char *path,
                                     const char *argvbuf, size_t argvbytes,
                                     const char *envbuf, size_t envbytes,
                                     struct capref inheritcn_cap,
                                     struct capref argcn_cap, uint8_t flags)
{
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

    struct pending_spawn *spawn = (struct pending_spawn*) malloc(
            sizeof(struct pending_spawn));
    spawn->domain_cap = domain_cap;
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

    event_mutex_enqueue_lock(&cl->mutex, &spawn->qn,
                             (struct event_closure) {
                                 .handler = spawn_request_sender,
                                 .arg = spawn });

    return SYS_ERR_OK;
}

static void spawn_handler(struct proc_mgmt_binding *b, coreid_t core_id,
                          const char *path, const char *argvbuf,
                          size_t argvbytes, const char *envbuf, size_t envbytes,
                          uint8_t flags)
{
    errval_t err, resp_err;
    err = spawn_handler_common(b, ClientType_Spawn, core_id, path, argvbuf,
                               argvbytes, envbuf, envbytes, NULL_CAP, NULL_CAP,
                               flags);
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

    struct pending_span *span = (struct pending_span*) malloc(
            sizeof(struct pending_span));
    span->domain_cap = domain_cap;
    span->b = cl;
    span->core_id = core_id;
    span->vroot = vroot;
    span->dispframe = dispframe;

    event_mutex_enqueue_lock(&cl->mutex, &span->qn,
                             (struct event_closure) {
                                 .handler = span_request_sender,
                                 .arg = span });

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

        struct pending_kill_exit_cleanup *cmd = (struct pending_kill_exit_cleanup*) malloc(
                sizeof(struct pending_kill_exit_cleanup));
        cmd->domain_cap = domain_cap;
        cmd->sb = spb;

        switch (type) {
            case ClientType_Kill:
                cmd->pmb = b;

                event_mutex_enqueue_lock(&spb->mutex,
                                         &cmd->qn,
                                         (struct event_closure) {
                                            .handler = kill_request_sender,
                                            .arg = cmd });
                break;

            case ClientType_Exit:
                event_mutex_enqueue_lock(&spb->mutex,
                                         &cmd->qn,
                                         (struct event_closure) {
                                            .handler = exit_request_sender,
                                            .arg = cmd });
                break;
            default:
                USER_PANIC("invalid client type for kill: %u\n", type);
        }
    }

    return SYS_ERR_OK;
}

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
