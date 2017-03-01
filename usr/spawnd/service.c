/**
 * \file
 * \brief spawn service
 */

/*
 * Copyright (c) 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <spawndomain/spawndomain.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/cpu_arch.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>
#include <dist/barrier.h>
#include <if/spawn_defs.h>
#include <if/monitor_blocking_defs.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/invocations_arch.h>

#include "internal.h"
#include "ps.h"


static errval_t spawn(const char *path, char *const argv[], const char *argbuf,
                      size_t argbytes, char *const envp[],
                      struct capref inheritcn_cap, struct capref argcn_cap,
                      uint8_t flags, domainid_t *domainid)
{
    errval_t err, msgerr;

    /* read file into memory */
    vfs_handle_t fh;
    err = vfs_open(path, &fh);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_LOAD);
    }

    struct vfs_fileinfo info;
    err = vfs_stat(fh, &info);
    if (err_is_fail(err)) {
        vfs_close(fh);
        return err_push(err, SPAWN_ERR_LOAD);
    }

    assert(info.type == VFS_FILE);
    uint8_t *image = malloc(info.size);
    if (image == NULL) {
        vfs_close(fh);
        return err_push(err, SPAWN_ERR_LOAD);
    }

    size_t pos = 0, readlen;
    do {
        err = vfs_read(fh, &image[pos], info.size - pos, &readlen);
        if (err_is_fail(err)) {
            vfs_close(fh);
            free(image);
            return err_push(err, SPAWN_ERR_LOAD);
        } else if (readlen == 0) {
            vfs_close(fh);
            free(image);
            return SPAWN_ERR_LOAD; // XXX
        } else {
            pos += readlen;
        }
    } while (err_is_ok(err) && readlen > 0 && pos < info.size);

    err = vfs_close(fh);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to close file %s", path);
    }

    // find short name (last part of path)
    const char *name = strrchr(path, VFS_PATH_SEP);
    if (name == NULL) {
        name = path;
    } else {
        name++;
    }

    /* spawn the image */
    struct spawninfo si;
    si.flags = flags;
    err = spawn_load_image(&si, (lvaddr_t)image, info.size, CURRENT_CPU_TYPE,
                           name, my_core_id, argv, envp, inheritcn_cap,
                           argcn_cap);
    if (err_is_fail(err)) {
        free(image);
        return err;
    }

    free(image);

    /* request connection from monitor */
    struct monitor_blocking_binding *mrpc = get_monitor_blocking_binding();
    struct capref monep;
    err = slot_alloc(&monep);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MONEP_SLOT_ALLOC);
    }
    err = mrpc->rpc_tx_vtbl.alloc_monitor_ep(mrpc, &msgerr, &monep);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MONITOR_CLIENT);
    } else if (err_is_fail(msgerr)) {
        return msgerr;
    }

    /* copy connection into the new domain */
    struct capref destep = {
        .cnode = si.taskcn,
        .slot  = TASKCN_SLOT_MONITOREP,
    };
    err = cap_copy(destep, monep);
    if (err_is_fail(err)) {
        spawn_free(&si);
        cap_destroy(monep);
        return err_push(err, SPAWN_ERR_MONITOR_CLIENT);
    }

    err = cap_destroy(monep);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MONITOR_CLIENT);
    }

    debug_printf("spawning %s on core %u\n", path, my_core_id);

    /* give the perfmon capability */
    struct capref dest, src;
    dest.cnode = si.taskcn;
    dest.slot = TASKCN_SLOT_PERF_MON;
    src.cnode = cnode_task;
    src.slot = TASKCN_SLOT_PERF_MON;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_PERF_MON);
    }

    /* run the domain */
    err = spawn_run(&si);
    if (err_is_fail(err)) {
        spawn_free(&si);
        return err_push(err, SPAWN_ERR_RUN);
    }

    // Allocate domain id
    struct ps_entry *pe = malloc(sizeof(struct ps_entry));
    assert(pe != NULL);
    memset(pe, 0, sizeof(struct ps_entry));
    memcpy(pe->argv, argv, MAX_CMDLINE_ARGS*sizeof(*argv));
    pe->argbuf = memdup(argbuf, argbytes);
    pe->argbytes = argbytes;
    /*
     * NB: It's important to keep a copy of the DCB *and* the root
     * CNode around.  We need to revoke both (in the right order, see
     * kill_domain() below), so that we ensure no one else is
     * referring to the domain's CSpace anymore. Especially the loop
     * created by placing rootcn into its own address space becomes a
     * problem here.
     */
    err = slot_alloc(&pe->rootcn_cap);
    assert(err_is_ok(err));
    err = cap_copy(pe->rootcn_cap, si.rootcn_cap);
    pe->rootcn = si.rootcn;
    assert(err_is_ok(err));
    err = slot_alloc(&pe->dcb);
    assert(err_is_ok(err));
    err = cap_copy(pe->dcb, si.dcb);
    assert(err_is_ok(err));
    pe->status = PS_STATUS_RUNNING;
    err = ps_allocate(pe, domainid);
    if(err_is_fail(err)) {
        free(pe);
    }

    // Store in target dispatcher frame
    struct dispatcher_generic *dg = get_dispatcher_generic(si.handle);
    dg->domain_id = *domainid;

    /* cleanup */
    err = spawn_free(&si);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_FREE);
    }

    return SYS_ERR_OK;
}

static void retry_use_local_memserv_response(void *a)
{
    errval_t err;

    struct spawn_binding *b = (struct spawn_binding*)a;

    err = b->tx_vtbl.use_local_memserv_response(b, NOP_CONT);

    if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        // try again
        err = b->register_send(b, get_default_waitset(),
                               MKCONT(retry_use_local_memserv_response,a));
    }
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error sending use_local_memserv reply\n");
    }

}


static void use_local_memserv_handler(struct spawn_binding *b)
{
    ram_alloc_set(NULL);

    errval_t err;
    err = b->tx_vtbl.use_local_memserv_response(b, NOP_CONT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error sending use_local_memserv reply");
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = b->register_send(b, get_default_waitset(),
                               MKCONT(retry_use_local_memserv_response, b));
            if (err_is_fail(err)) {
                // note that only one continuation may be registered at a time
                DEBUG_ERR(err, "register_send failed!");
            }
        }
    }
}

struct pending_spawn_response {
    struct spawn_binding *b;
    errval_t err;
    domainid_t domainid;
};

static errval_t spawn_with_caps_common(const char *path, const char *argbuf,
                                       size_t argbytes, const char *envbuf,
                                       size_t envbytes,
                                       struct capref inheritcn_cap,
                                       struct capref argcn_cap, uint8_t flags,
                                       domainid_t *domainid)
{
    errval_t err;
    assert(domainid);
    *domainid = 0;

    /* extract arguments from buffer */
    char *argv[MAX_CMDLINE_ARGS + 1];
    int i = 0;
    size_t pos = 0;
    while (pos < argbytes && i < MAX_CMDLINE_ARGS) {
        argv[i++] = (CONST_CAST)argbuf + pos;
        char *end = memchr(&argbuf[pos], '\0', argbytes - pos);
        if (end == NULL) {
            err = SPAWN_ERR_GET_CMDLINE_ARGS;
            goto finish;
        }
        pos = end - argbuf + 1;
    }
    assert(i <= MAX_CMDLINE_ARGS);
    argv[i] = NULL;

    /* extract environment from buffer */
    char *envp[MAX_CMDLINE_ARGS + 1];
    i = 0;
    pos = 0;
    while (pos < envbytes && i < MAX_CMDLINE_ARGS) {
        envp[i++] = (CONST_CAST)envbuf + pos;
        char *end = memchr(&envbuf[pos], '\0', envbytes - pos);
        if (end == NULL) {
            err = SPAWN_ERR_GET_CMDLINE_ARGS;
            goto finish;
        }
        pos = end - envbuf + 1;
    }
    assert(i <= MAX_CMDLINE_ARGS);
    envp[i] = NULL;

    char *npath;
    npath = alloca(strlen(path));
    strcpy(npath, path);
    vfs_path_normalise(npath);

    err = spawn(npath, argv, argbuf, argbytes, envp, inheritcn_cap, argcn_cap,
                flags, domainid);
    // XXX: do we really want to delete the inheritcn and the argcn here? iaw:
    // do we copy these somewhere? -SG
    if (!capref_is_null(inheritcn_cap)) {
        errval_t err2;
        err2 = cap_delete(inheritcn_cap);
        assert(err_is_ok(err2));
    }
    if (!capref_is_null(argcn_cap)) {
        errval_t err2;
        err2 = cap_delete(argcn_cap);
        assert(err_is_ok(err2));
    }

 finish:
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "spawn");
    }

    return err;
}

static errval_t spawn_with_caps_handler(struct spawn_binding *b, const char *path,
    const char *argvbuf, size_t argvbytes, const char *envbuf, size_t envbytes,
    struct capref inheritcn_cap, struct capref argcn_cap, uint8_t flags,
    errval_t *err, spawn_domainid_t *domain_id)
{
    *err = spawn_with_caps_common(path, argvbuf, argvbytes, envbuf, envbytes,
                                 inheritcn_cap, argcn_cap, flags, domain_id);
    return SYS_ERR_OK;
}

static errval_t spawn_handler(struct spawn_binding *b, const char *path,
    const char *argvbuf, size_t argvbytes, const char *envbuf, size_t envbytes,
    uint8_t flags, errval_t *err, spawn_domainid_t *domain_id)
{
    *err = spawn_with_caps_common(path, argvbuf, argvbytes, envbuf, envbytes,
                                 NULL_CAP, NULL_CAP, flags, domain_id);
    return SYS_ERR_OK;
}

/**
 * \brief Removes a zombie domain.
 */
static void cleanup_domain(domainid_t domainid)
{
    errval_t err;
    struct ps_entry *ps = ps_get(domainid);
    assert(ps != NULL);

    // Tell all waiters of exit and free list as we go
    for(struct ps_waiter *w = ps->waiters; w != NULL;) {
        err = w->binding->tx_vtbl.wait_response
            (w->binding, NOP_CONT, ps->exitcode, SYS_ERR_OK);
        if(err_is_fail(err)) {
            DEBUG_ERR(err, "wait_response");
        }

        struct ps_waiter *oldw = w;
        w = w->next;
        free(oldw);
    }
    ps->waiters = NULL;

    // Cleanup rest of ps entry
    free(ps->argbuf);

    ps_remove(domainid);
}

static void cleanup_cap(struct capref cap)
{
    errval_t err;

    err = cap_revoke(cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cap_revoke");
    }
    err = cap_destroy(cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cap_destroy");
    }
}

static errval_t kill_domain(domainid_t domainid, uint8_t exitcode)
{
    struct ps_entry *ps = ps_get(domainid);

    if(ps == NULL) {
        return SPAWN_ERR_DOMAIN_NOTFOUND;
    }

    ps->status = PS_STATUS_ZOMBIE;
    ps->exitcode = exitcode;

    // Garbage collect victim's capabilities
    cleanup_cap(ps->dcb);       // Deschedule dispatcher (do this first!)
    cleanup_cap(ps->rootcn_cap);

    // XXX: why only when waiters exist? -SG
    if(ps->waiters != NULL) {
        // Cleanup local data structures and inform waiters
        cleanup_domain(domainid);
    }

    return SYS_ERR_OK;
}

static void kill_handler(struct spawn_binding *b, domainid_t domainid)
{
    errval_t err = kill_domain(domainid, 0);

    err = b->tx_vtbl.kill_response(b, NOP_CONT, err);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "kill_response");
    }
}

static void exit_handler(struct spawn_binding *b, domainid_t domainid,
                         uint8_t exitcode)
{
    errval_t err = kill_domain(domainid, exitcode);
    struct ps_entry *ps = ps_get(domainid);

    if(err_is_fail(err)) {
        DEBUG_ERR(err, "kill_domain");
    }

    if(ps == NULL) {
        // XXX: Can't do nothing
        return;
    }

    // May never return anything to client
}

static void wait_handler(struct spawn_binding *b, domainid_t domainid,
                         bool nohang)
{
    errval_t err;
    struct ps_entry *ps = ps_get(domainid);

    if(ps == NULL) {
        err = b->tx_vtbl.wait_response(b, NOP_CONT, 0, SPAWN_ERR_DOMAIN_NOTFOUND);
        if(err_is_fail(err)) {
            DEBUG_ERR(err, "wait_response");
        }
    } else {
        if(!nohang || ps->status == PS_STATUS_ZOMBIE) {
            // Enqueue the waiter
            struct ps_waiter *waiter = malloc(sizeof(struct ps_waiter));
            assert(waiter != NULL);
            waiter->next = ps->waiters;
            waiter->binding = b;
            ps->waiters = waiter;
        } else {
            // nohang and no zombie, return error
            err = b->tx_vtbl.wait_response(b, NOP_CONT, 0, SPAWN_ERR_DOMAIN_RUNNING);
            if(err_is_fail(err)) {
                DEBUG_ERR(err, "wait_response");
            }
        }

        // Cleanup if zombie (will send the reply)
        if(ps->status == PS_STATUS_ZOMBIE) {
            cleanup_domain(domainid);
        }
    }
}

static void get_domainlist_sent(void *arg)
{
    free(arg);
}

static void get_domainlist_handler(struct spawn_binding *b)
{
    errval_t err;
    size_t len = 0;
    uint8_t *domains = calloc(sizeof(uint8_t), MAX_DOMAINS);

    // XXX: Very inefficient
    for(domainid_t i = 0; i < MAX_DOMAINS; i++) {
        if(ps_exists(i)) {
            domains[len++] = i;
        }
    }

    err = b->tx_vtbl.get_domainlist_response
        (b, MKCLOSURE(get_domainlist_sent, domains), domains, len);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "get_domainlist_response");
        free(domains);
    }
}

static void status_handler(struct spawn_binding *b, domainid_t domainid)
{
    errval_t err;
    struct ps_entry *ps = ps_get(domainid);
    spawn_ps_entry_t pse;

    memset(&pse, 0, sizeof(pse));

    if(ps == NULL) {
        err = b->tx_vtbl.status_response(b, NOP_CONT, pse, NULL, 0,
                                         SPAWN_ERR_DOMAIN_NOTFOUND);
        if(err_is_fail(err)) {
            DEBUG_ERR(err, "status_response");
        }
    }

    pse.status = ps->status;

    err = b->tx_vtbl.status_response(b, NOP_CONT, pse, ps->argbuf, ps->argbytes,
                                     SYS_ERR_OK);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "status_response");
    }
}


static errval_t dump_capabilities(domainid_t domainid) {
    struct ps_entry *ps = ps_get(domainid);

    if(ps == NULL) {
        return SPAWN_ERR_DOMAIN_NOTFOUND;
    }

    return invoke_dispatcher_dump_capabilities(ps->dcb);
}

static void dump_capabilities_handler(struct spawn_binding *b, domainid_t domainid) {
    errval_t err = dump_capabilities(domainid);

    err = b->tx_vtbl.dump_capabilities_response(b, NOP_CONT, err);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "debug_print_capabilities_response");
    }
}

static struct spawn_rx_vtbl rx_vtbl = {
    // .spawn_domain_call = spawn_handler,
    // .spawn_domain_with_caps_call = spawn_with_caps_handler,
    .use_local_memserv_call = use_local_memserv_handler,
    .kill_call = kill_handler,
    .exit_call = exit_handler,
    .wait_call = wait_handler,
    .get_domainlist_call = get_domainlist_handler,
    .status_call = status_handler,
    .dump_capabilities_call = dump_capabilities_handler
};

static struct spawn_rpc_rx_vtbl rpc_rx_vtbl = {
    .spawn_domain_call = spawn_handler,
    .spawn_domain_with_caps_call = spawn_with_caps_handler,
    // .use_local_memserv_call = use_local_memserv_handler,
    // .kill_call = kill_handler,
    // .exit_call = exit_handler,
    // .wait_call = wait_handler,
    // .get_domainlist_call = get_domainlist_handler,
    // .status_call = status_handler,
    // .dump_capabilities_call = dump_capabilities_handler
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


static errval_t connect_cb(void *st, struct spawn_binding *b)
{
    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;
    b->rpc_rx_vtbl = rpc_rx_vtbl;
    return SYS_ERR_OK;
}

errval_t start_service(void)
{
    return spawn_export(NULL, export_cb, connect_cb, get_default_waitset(),
                         IDC_EXPORT_FLAGS_DEFAULT);
}
