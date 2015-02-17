/**
 * \file
 * \brief Spawn daemon for Barrelfish.
 * Offers a service on each core to spawn programs from
 * the file system.
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
#include <barrelfish/dispatch.h>
#include <barrelfish_kpi/cpu.h> // for cpu_type_to_archstr()
#include <barrelfish/cpu_arch.h> // for CURRENT_CPU_TYPE

#include <vfs/vfs.h>

#include <if/monitor_defs.h>

#include "internal.h"

coreid_t my_core_id;
bool is_bsp_core;
const char *gbootmodules;

/* set an initial default environment for our boot-time children */
static void init_environ(void)
{
    int r;

    /* PATH=/arch/sbin */
    char pathstr[64];
    snprintf(pathstr, sizeof(pathstr), "/" BF_BINARY_PREFIX "%s/sbin",
             cpu_type_to_archstr(CURRENT_CPU_TYPE));
    pathstr[sizeof(pathstr) - 1] = '\0';
    r = setenv("PATH", pathstr, 0);
    if (r != 0) {
        USER_PANIC("failed to set PATH");
    }

    /* HOME=/ */
    r = setenv("HOME", "/", 0);
    if (r != 0) {
        USER_PANIC("failed to set HOME");
    }
}

/* open bootmodules file and read it in */
static void get_bootmodules(void)
{
    errval_t err;

    // open bootmodules file and read it in
    vfs_handle_t vh;
    err = vfs_open("/bootmodules", &vh);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "unable to open /bootmodules");
    }

    struct vfs_fileinfo info;
    err = vfs_stat(vh, &info);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "unable to stat /bootmodules");
    }

    char *bootmodules = malloc(info.size + 1);
    if (bootmodules == NULL) {
        USER_PANIC_ERR(LIB_ERR_MALLOC_FAIL,
                       "failed to allocate memory for bootmodules");
    }
    size_t bootmodules_len;
    err = vfs_read(vh, bootmodules, info.size, &bootmodules_len);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "unable to read /bootmodules");
    } else if (bootmodules_len == 0) {
        USER_PANIC_ERR(err, "/bootmodules is empty");
    } else if (bootmodules_len != info.size) {
        USER_PANIC_ERR(err, "unexpected short read of /bootmodules");
    }

    err = vfs_close(vh);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not close bottmodules file");
    }

    // terminate as a string
    bootmodules[bootmodules_len] = '\0';
    gbootmodules = bootmodules;

}

int main(int argc, const char *argv[])
{
    errval_t err;

    my_core_id = disp_get_core_id();

    printf("spawnd.%u up.\n", my_core_id);

    vfs_init();

    // read in the bootmodules file so that we know what to start
    get_bootmodules();

    // construct sane inital environment
    init_environ();

    err = start_service();
    if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to start spawnd service loop");
    }

    messages_handler_loop();
}
