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
#include <netbench/netbench.h>

#define MOUNT_DIR   "/nfs"

// reads the file over nfs
static int cat(char *path)
{
    size_t size;
    vfs_handle_t vh;
    errval_t err;
    uint64_t filesize = 0;

    err = vfs_open(path, &vh);
    if (err_is_fail(err)) {
        printf("%s: file not found\n", path);
        return 0;
    }

    struct vfs_fileinfo info;
    err = vfs_stat(vh, &info);
    if(err_is_fail(err)){
    	printf("Could not stat file %s\n", path);
    }
    printf("Reading %d bytes from %s.\n", (int)info.size, path);
    void *buf = malloc(10485760);
    assert(buf);

    uint64_t start = rdtsc();

    for (; filesize != info.size;) {
    	err = vfs_read(vh, buf, 10485760, &size);
    	if (err_is_fail(err)) {
    		// XXX: Close any files that might be open
    		DEBUG_ERR(err, "error reading file");
    		return 0;
    	}
        debug_printf("%s: %ld:%ld\n", __func__, filesize, info.size);
    	filesize += size;
    }
    assert(info.size == filesize);

    // record stop time
    uint64_t stop = rdtsc();
    printf("Everything done\n");
    double speed = ((filesize/in_seconds(stop - start))/(1024 * 1024));
    if (speed < 50) {
        printf("Warning: NFS throughput too low!! [%f]\n", speed);
    }
    printf("## Data size = %f MB,  Processing time [%"PU"], speed [%f] MB/s\n",
           filesize/(double)(1024 * 1024), in_seconds(stop - start),
           speed);

    err = vfs_close(vh);
    if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_close");
    }
    free(buf);
    return filesize;
}

int main(int argc, char**argv)
{
    errval_t err;
    vfs_init();

    if(argc < 3) {
        printf("Usage: %s mount-URL filepath\n", argv[0]);
        printf("Example: %s nfs://10.110.4.41/shared  /nfs/pravin/601.avi\n",
                argv[0]);
        exit(EXIT_FAILURE);
    }

// don't have to do this, MOUNT_DIR is already there
//    err = vfs_mkdir(MOUNT_DIR);
//    if (err_is_fail(err)) {
//        DEBUG_ERR(err, "vfs_mount");
//    }

    err = vfs_mount(MOUNT_DIR, argv[1]);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_mount");
    }
    assert(err_is_ok(err));
    printf("mount done\n");

    printf("reading file 1. time [%s]\n", argv[2]);
    cat(argv[2]);
    printf("receive 1 done.\n");

    /*
    printf("reading file 2. time [%s]\n", argv[2]);
	cat(argv[2]);
	printf("receive 2 done.\n"); */
	printf("All done.\n");

    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }

}
