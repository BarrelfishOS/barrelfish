/**
 * \file
 * \brief spawn service
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich.
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
#include <if/monitor_blocking_rpcclient_defs.h>

#include "internal.h"

static errval_t spawn(char *path, char *const argv[], char *const envp[])
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
    char *name = strrchr(path, VFS_PATH_SEP);
    if (name == NULL) {
        name = path;
    } else {
        name++;
    }

    /* spawn the image */
    struct spawninfo si;
    err = spawn_load_image(&si, (lvaddr_t)image, info.size, CURRENT_CPU_TYPE,
                           name, my_core_id, argv, envp);
    if (err_is_fail(err)) {
        free(image);
        return err;
    }

    free(image);

    /* request connection from monitor */
    struct monitor_blocking_rpc_client *mrpc = get_monitor_blocking_rpc_client();
    struct capref monep;
    err = mrpc->vtbl.alloc_monitor_ep(mrpc, &msgerr, &monep);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_MONITOR_CLIENT);
    } else if (err_is_fail(msgerr)) {
        return msgerr;
    }

    /* copy connection into the new domain */
    struct capref destep = {
        .cnode = si.rootcn,
        .slot  = ROOTCN_SLOT_MONITOREP,
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

    /* run the domain */
    err = spawn_run(&si);
    if (err_is_fail(err)) {
        spawn_free(&si);
        return err_push(err, SPAWN_ERR_RUN);
    }

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
    ram_alloc_set(NULL, NULL);

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
};

static void retry_spawn_domain_response(void *a)
{
    errval_t err;

    struct pending_spawn_response *r = (struct pending_spawn_response*)a;
    struct spawn_binding *b = r->b;

    err = b->tx_vtbl.spawn_domain_response(b, NOP_CONT, r->err, 0);

    if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        // try again
        err = b->register_send(b, get_default_waitset(), 
                               MKCONT(retry_spawn_domain_response,a));
    }
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error sending spawn_domain reply\n");
    }

    free(a);
}


static errval_t spawn_reply(struct spawn_binding *b, errval_t rerr)
{
    errval_t err;
 
    err = b->tx_vtbl.spawn_domain_response(b, NOP_CONT, rerr, 0);

    if (err_is_fail(err)) { 
        DEBUG_ERR(err, "error sending spawn_domain reply\n");

        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            // this will be freed in the retry handler
            struct pending_spawn_response *sr = 
                malloc(sizeof(struct pending_spawn_response));
            if (sr == NULL) {
                return LIB_ERR_MALLOC_FAIL;
            }
            sr->b = b;
            sr->err = rerr;
            err = b->register_send(b, get_default_waitset(), 
                                   MKCONT(retry_spawn_domain_response, sr));
            if (err_is_fail(err)) {
                // note that only one continuation may be registered at a time
                free(sr);
                DEBUG_ERR(err, "register_send failed!");
                return err;
            }
        }
    }

    return SYS_ERR_OK;
}

static void spawn_handler(struct spawn_binding *b, char *path, char *argbuf,
                          size_t argbytes, char *envbuf, size_t envbytes)
{
    errval_t err;

    /* extract arguments from buffer */
    char *argv[MAX_CMDLINE_ARGS + 1];
    int i = 0;
    size_t pos = 0;
    while (pos < argbytes && i < MAX_CMDLINE_ARGS) {
        argv[i++] = &argbuf[pos];
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
        envp[i++] = &envbuf[pos];
        char *end = memchr(&envbuf[pos], '\0', envbytes - pos);
        if (end == NULL) {
            err = SPAWN_ERR_GET_CMDLINE_ARGS;
            goto finish;
        }
        pos = end - envbuf + 1;
    }
    assert(i <= MAX_CMDLINE_ARGS);
    envp[i] = NULL;

    vfs_path_normalise(path);
    err = spawn(path, argv, envp);

 finish:
    err = spawn_reply(b, err);

    if (err_is_fail(err)) {
        // not much we can do about this
        DEBUG_ERR(err, "while sending reply in spawn_handler");
    }

    free(argbuf);
    free(envbuf);
    free(path);
}


static struct spawn_rx_vtbl rx_vtbl = {
    .spawn_domain_call = spawn_handler,
    .use_local_memserv_call = use_local_memserv_handler,
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
    // let the master know we are ready
    err = nsb_register_n(my_core_id, SERVICE_BASENAME);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nsb_register_n failed");
    }

    // wait for boot to finish
    err = nsb_wait(ALL_SPAWNDS_UP);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed ns barrier wait for %s", ALL_SPAWNDS_UP);
    }
    // debug_printf("got \"%s\", continuing\n", ALL_SPAWNDS_UP);
}


static errval_t connect_cb(void *st, struct spawn_binding *b)
{
    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;
    return SYS_ERR_OK;
}

errval_t start_service(void)
{
    return spawn_export(NULL, export_cb, connect_cb, get_default_waitset(),
                         IDC_EXPORT_FLAGS_DEFAULT);
}
