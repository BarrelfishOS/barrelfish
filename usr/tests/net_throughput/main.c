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
#include <lwip/init.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/waitset.h>
#include <contmng/netbench.h>

#define DIRNAME   "/nfs"
#define FILENAME   "/nfs/pravin/testfile.txt"
//#define MAX_DATA   (1330 * 8)
#define MAX_DATA   (130)
//#define MAX_DATA   (1U << 12)

uint8_t buf[1024 * 1024 * 2];

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

    uint64_t start = rdtsc();
    lwip_benchmark_control(1, BMS_START_REQUEST, 0, 0);
    do {
        err = vfs_read(vh, buf, sizeof(buf), &size);
        if (err_is_fail(err)) {
            // XXX: Close any files that might be open
            DEBUG_ERR(err, "error reading file");
            return 0;
        }

        filesize += size;
//      fwrite(buf, 1, size, stdout);
    } while(size > 0);

    // record stop time
    uint64_t stop = rdtsc();
    printf("######## Everythin done\n");
    printf("Data size = %f MB,  Processing time [%"PU"], speed [%f] MB/s\n",
           filesize/(double)(1024 * 1024), in_seconds(stop - start),
           ((filesize/in_seconds(stop - start))/(1024 * 1024)));
    lwip_print_interesting_stats();

    err = vfs_close(vh);
    if (err_is_fail(err)) {
            DEBUG_ERR(err, "in vfs_close");
    }

    return filesize;
}


int main(int argc, char**argv)
{

    if(argc < 4) {
        printf("Usage: %s mount-DIR  mount-URL filepath\n", argv[0]);
        printf("Example: %s /nfs nfs://10.110.4.41/shared  /nfs/pravin/601.avi\n",
                argv[0]);
        exit(EXIT_FAILURE);
    }

    errval_t err = vfs_mkdir(argv[1]);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_mount");
    }

    printf("######## mkdir done\n");
    err = vfs_mount(argv[1], argv[2]);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_mount");
    }
    assert(err_is_ok(err));
    printf("######## mount done\n");

    printf("######## reading file [%s]\n", argv[3]);

    cat(argv[3]);

#if 0
    // Parse trace file into memory records
    FILE *f = fopen(argv[3], "r");
    assert(f != NULL);
    //    printf("######## file opened[%s]\n", argv[3]);

    /* FIXME: record start time */
    uint64_t total_size = 0;
    uint64_t start = rdtsc();
    while(!feof(f)) {
        char data[MAX_DATA];
        int ret = fread(data, MAX_DATA, 1, f);
        if (ret <= 0) {
            printf("fread returned %d, so EOF\n", ret);
            break;
        }
        total_size += ret;
        data[ret] = '\0';
        //        printf("%s", data);
    }
    uint64_t stop = rdtsc();
    /* FIXME: record stop time */
    printf("######## Everythin done\n");
    printf("Data size = %"PRIu64",  Processing time [%"PRIu64"] [%"PU"]\n",
           total_size, (stop - start), in_seconds(stop - start));
#endif // 0

    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }

}
