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

#include <lwip/init.h>
#include <lwip/tcpip.h>
#include <lwip/sockets.h>

#include <vfs/vfs_path.h>
#include <vfs/vfs.h>


#define NFS_PREFIX "/data"

/**
 * Mirrors NFS mountpoint into RAM to avoid
 * this https://code.systems.ethz.ch/T27
 *
 * @param mountpoint NFS mount point
 */
static void mirror_nfs(char* path) {
    printf("%s:%d: path=%s\n", __FUNCTION__, __LINE__, path);
    vfs_handle_t dhandle;
    struct vfs_fileinfo info;

    errval_t err = vfs_opendir(path, &dhandle);
    assert(err_is_ok(err));

    char* name;
    while(err_is_ok(vfs_dir_read_next(dhandle, &name, &info))) {

        if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0)
            continue;
        char* newpath = malloc(snprintf(NULL, 0, "%s/%s", path, name) + 1);
        sprintf(newpath, "%s/%s", path, name);

        switch (info.type) {
        case VFS_DIRECTORY:
            printf("%s:%d: Found directory %s\n", __FUNCTION__, __LINE__, newpath);
            printf("%s:%s:%d: Mirror Directory from %s to %s\n",
                   __FILE__, __FUNCTION__, __LINE__,
                   newpath,
                   newpath+strlen(NFS_PREFIX));
            err = vfs_mkdir(newpath+strlen(NFS_PREFIX));
            if (err_is_fail(err)) {
                if (err_no(err) == FS_ERR_EXISTS) {
                    printf("%s:%s:%d: Dir %s exists, ignore\n",
                           __FILE__, __FUNCTION__, __LINE__,
                           newpath+strlen(NFS_PREFIX));
                } else {
                    USER_PANIC_ERR(err, "vfs_mkdir failed.");
                }
            }

            mirror_nfs(newpath);
            break;

        case VFS_FILE:
            printf("%s:%d: Found file %s\n", __FUNCTION__, __LINE__, newpath);
            vfs_handle_t fhandle;

            err = vfs_open(newpath, &fhandle);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "vfs open failed.");
            }
            char* buf = malloc(info.size);
            assert (buf != NULL);
            printf("%s:%s:%d: trying to read %zu bytes\n",
                   __FILE__, __FUNCTION__, __LINE__, info.size);

            size_t bytes_read = 0;
            if (info.size > 0) {
                err = vfs_read(fhandle, buf, info.size, &bytes_read);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "vfs_read failed.");
                }
                assert(bytes_read == info.size);
            }

            printf("%s:%s:%d: mirror file from %s to %s\n",
                   __FILE__, __FUNCTION__, __LINE__,
                   newpath,
                   newpath+strlen(NFS_PREFIX));
            vfs_handle_t ohandle;
            err = vfs_create(newpath+strlen(NFS_PREFIX), &ohandle);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "vfs_create failed.");
            }

            if (info.size > 0) {
                size_t bytes_written;
                err = vfs_write(ohandle, buf, info.size, &bytes_written);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "vfs_write failed.");
                }
                assert(bytes_written == info.size);
            }

            err = vfs_close(ohandle);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "vfs_close ohandle failed.");
            }

            err = vfs_close(fhandle);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "vfs_close fhandle failed.");
            }

            free(buf);
            break;
        }

        free(name);
        free(newpath);
    }

    err = vfs_closedir(dhandle);
    assert(err_is_ok(err));
}


int main(int argc, char** argv)
{

    printf("%s:%s:%d: Lets do this!\n", __FILE__, __FUNCTION__, __LINE__);
    errval_t err;

    printf("%s:%s:%d: Init it..\n", __FILE__, __FUNCTION__, __LINE__);
    err = lwip_init_auto();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "lwip failed.");
    }
    vfs_init();

    printf("%s:%s:%d: Create it..\n", __FILE__, __FUNCTION__, __LINE__);
    err = vfs_mkdir(NFS_PREFIX);
    if (err_is_fail(err)) {
      DEBUG_ERR(err, "vfs_mkdir failed\n");
      return 1;
    }

    printf("%s:%s:%d: Mount it..\n", __FILE__, __FUNCTION__, __LINE__);
    err = vfs_mount(NFS_PREFIX, "nfs://10.110.4.4/local/nfs/zgerd/data/tpch_tiny");
    if (err_is_fail(err)) {
      DEBUG_ERR(err, "vfs_mount");
      return 1;
    }

    printf("%s:%s:%d: Mirror it..\n", __FILE__, __FUNCTION__, __LINE__);
    mirror_nfs(NFS_PREFIX);

    printf("%s:%s:%d: We did it!\n", __FILE__, __FUNCTION__, __LINE__);

    return 0;
}