/**
 * \file
 * \brief Mirror an NFS handle into ramfs
 */
/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>

#include <lwip/init.h>
#include <lwip/tcpip.h>
#include <lwip/sockets.h>

#include <vfs/vfs_path.h>
#include <vfs/vfs.h>

#include "http_cache.h"

static struct ip_addr serverip;
static const char *serverpath;

#define BASE_PATH "/data/"

bool really_done = false;
static void realinit(void)
{
    really_done = true;
    printf("Are we done, yes we are.\n");
}
void place_file_in_ramfs(struct http_cache_entry* e);
void place_file_in_ramfs(struct http_cache_entry* e) {
    assert (e->valid);
    char real_path[99];
    strcpy(real_path, BASE_PATH);
    strcat(real_path, e->name);

    vfs_handle_t ohandle;

    printf("%s:%s:%d: vfs create for real_path = %s\n",
           __FILE__, __FUNCTION__, __LINE__, real_path);
    errval_t err = vfs_create(real_path, &ohandle);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vfs_create failed.");
    }

    size_t bytes_written;
    err = vfs_write(ohandle, e->hbuff->data, e->hbuff->len, &bytes_written);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vfs_write failed.");
    }
    assert(bytes_written == e->hbuff->len);

    err = vfs_close(ohandle);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vfs_close ohandle failed.");
    }
}

void create_all_the_dirs(struct http_cache_entry* e);
void create_all_the_dirs(struct http_cache_entry* e) {
    char real_path[256];
    strcpy(real_path, BASE_PATH);
    strcat(real_path, e->name);

    printf("%s:%s:%d: vfs create for real_path = %s\n",
           __FILE__, __FUNCTION__, __LINE__, real_path);
    errval_t err = vfs_mkdir(real_path);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "can not create dir, does it already exist?");
    }
}


int main(int argc, char **argv)
{

    printf("%s:%s:%d: Lets do this!\n", __FILE__, __FUNCTION__, __LINE__);
    errval_t err;

    printf("%s:%s:%d: Init it..\n", __FILE__, __FUNCTION__, __LINE__);
    err = lwip_init_auto();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "lwip failed.");
    }
    vfs_init();
    err = vfs_mkdir("/data");
    if (err_is_fail(err)) {
      DEBUG_ERR(err, "vfs_mkdir failed\n");
      return 1;
    }


    // Hardcode it
    struct in_addr server1;
    if (inet_aton("10.110.4.4", &server1) == 0) {
        printf("Invalid IP addr: %s\n", argv[2]);
        return 1;
    }
    serverip.addr = server1.s_addr; // XXX
    serverpath = "/local/nfs/zgerd/data/tpch_tiny";

    http_cache_init(serverip, serverpath, realinit);

    printf("%s:%s:%d: We did it! Now go into the loop and do shit asynchronous..\n",
           __FILE__, __FUNCTION__, __LINE__);

    struct waitset *ws = get_default_waitset();
    while (!really_done) {
        // check for any event without blocking
        err = event_dispatch_non_block(ws);
        //err = event_dispatch(ws);
        if (err != LIB_ERR_NO_EVENT) {
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in event_dispatch");
                break;
            }
        }

        // Check if lwip has any pending work to finish
        wrapper_perform_lwip_work();
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }

    return 0;
}