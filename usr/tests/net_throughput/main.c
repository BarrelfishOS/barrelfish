/**
 * \file
 * \brief throughput testing program
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

// Specific for barrelfish
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/waitset.h>

#define DIRNAME   "/nfs"
#define FILENAME   "/nfs/testfile.txt"
#define MAX_DATA   512
int main(int argc, char**argv)
{

    if(argc < 2) {
        printf("Usage: %s  mount-URL\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    errval_t err = vfs_mkdir(DIRNAME);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_mount");
    }

    printf("######## mkdir done\n");
    err = vfs_mount(DIRNAME, argv[1]);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_mount");
    }
    assert(err_is_ok(err));
    printf("######## mount done\n");

    printf("######## reading file...\n");

    // Parse trace file into memory records
    FILE *f = fopen(FILENAME, "r");
    assert(f != NULL);
    printf("######## file opened\n");

    while(!feof(f)) {
        char data[MAX_DATA];
        int ret = fread(data, MAX_DATA, 1, f);
        if (ret <= 0) {
            printf("fread returned %d, so EOF\n", ret);
            break;
        }
        printf("%s", data);
    }
    printf("######## Everythin done\n");

    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }

}
