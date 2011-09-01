#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <barrelfish/nameservice_client.h>
#include <if/replay_defs.h>
#include "defs.h"

static char *defdir;

static void export_cb(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));
    char name[256];
    snprintf(name, 256, "replay_slave.%u", disp_get_core_id());
    err = nameservice_register(name, iref);
    assert(err_is_ok(err));
}

#define MAX_FD_CONV     256
#define MAX_DATA        (1 * 1024 * 1024)

static int pidconv[MAX_PIDS] = { 0 };
static FILE *fdconv[MAX_PIDS][MAX_FD_CONV];
static int fnumconv[MAX_PIDS][MAX_FD_CONV];
static bool writerconv[MAX_PIDS][MAX_FD_CONV];
static char data[MAX_DATA];

static void handle_event(struct replay_binding *b, replay_eventrec_t er)
{
    char fname[256];
    char *flags = NULL;

    switch(er.mode) {
    case FLAGS_RdOnly:
        flags = "r";
        break;

    case FLAGS_WrOnly:
        flags = "w";
        break;

    case FLAGS_RdWr:
        flags = "w+";
        break;
    }

    int mypid;
    for(mypid = 0; mypid < MAX_PIDS; mypid++) {
        if(pidconv[mypid] == er.pid) {
            break;
        }

        if(pidconv[mypid] == 0) {
            /* printf("%d: mypid %d is PID %d\n", disp_get_core_id(), mypid, er.pid); */
            pidconv[mypid] = er.pid;
            break;
        }
    }
    assert(mypid < MAX_PIDS);

    snprintf(fname, 256, "%s/%u", defdir, er.fnumsize);
    FILE *f = fdconv[mypid][er.fd];
    int ret;

    /* printf("%s is %p, %d\n", fname, f, er.fd); */

    switch(er.op) {
    case TOP_Open:
    case TOP_Create:
        /* assert(f == NULL); */
        assert(flags != NULL);
        fdconv[mypid][er.fd] = fopen(fname, flags);
        fnumconv[mypid][er.fd] = er.fnumsize;
        if(er.mode != FLAGS_RdOnly) {
            writerconv[mypid][er.fd] = true;
        } else {
            writerconv[mypid][er.fd] = false;
        }
        if(fdconv[mypid][er.fd] == NULL) {
            printf("%d: open fname = %s, fd = %d, fline = %d, pid = %d, mypid = %d\n", disp_get_core_id(), fname, er.fd, er.fline, er.pid, mypid);
            printf("errno = %d\n", errno);
        }
        assert(fdconv[mypid][er.fd] != NULL);
        break;

    case TOP_Unlink:
        ret = unlink(fname);
        assert(ret != -1);
        break;

    case TOP_Read:
        if(er.fnumsize > MAX_DATA) {
            printf("er.fnumsize == %u\n", er.fnumsize);
        }
        assert(er.fnumsize <= MAX_DATA);
        if(f == NULL) {
            printf("%d: read fnumsize = %d, fd = %d, fline = %d, pid = %d, mypid = %d\n", disp_get_core_id(), er.fnumsize, er.fd, er.fline, er.pid, mypid);
        }
        assert(f != NULL);
        ret = fread(data, er.fnumsize, 1, f);
        /* if(ret != er.fnumsize) { */
        /*     printf("read: ret == %d, fnumsize == %d\n", ret, er.fnumsize); */
        /* } */
        /* assert(ret == er.fnumsize); */
        break;

    case TOP_Write:
        assert(er.fnumsize <= MAX_DATA);
        if(f == NULL) {
            printf("%d: write fnumsize = %d, fd = %d, fline = %d\n", disp_get_core_id(), er.fnumsize, er.fd, er.fline);
        } else {
            assert(f != NULL);
            ret = fwrite(data, er.fnumsize, 1, f);
        }
        /* if(ret != er.fnumsize) { */
        /*     printf("write: ret == %d, fnumsize == %d\n", ret, er.fnumsize); */
        /* } */
        /* assert(ret == er.fnumsize); */
        break;

    case TOP_Close:
        /* printf("%d: close fd = %d, fline = %d, fname = %d\n", disp_get_core_id(), er.fd, er.fline, fnumconv[mypid][er.fd]); */
        /* if(f == NULL) { */
        /*     printf("%d: Warning: Double close: %d, %u, %d, %d\n", */
        /*            disp_get_core_id(), er.op, er.fnumsize, er.fd, er.mode); */
        /* } else { */
        if(f != NULL) {
            assert(f != NULL);
            ret = fclose(f);
            assert(ret != -1);
        }
        fdconv[mypid][er.fd] = NULL;

        /* if(writerconv[mypid][er.fd]) { */
        /*     errval_t err = b->tx_vtbl.event_done(b, NOP_CONT, fnumconv[mypid][er.fd]); */
        /*     assert(err_is_ok(err)); */
        /* } */
        break;

    default:
        printf("Invalid request: %d\n", er.op);
        break;
    }
}

static struct replay_rx_vtbl replay_vtbl = {
    .event = handle_event,
};

static errval_t connect_cb(void *st, struct replay_binding *b)
{
    b->rx_vtbl = replay_vtbl;
    return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    assert(argc >= 4);
    defdir = argv[1];

    memset(fdconv, 0, MAX_PIDS * MAX_FD_CONV * sizeof(FILE *));
    memset(pidconv, 0, MAX_PIDS * sizeof(int));

    printf("replay slave up\n");

    errval_t err = vfs_mkdir(argv[2]);
    if(err_is_fail(err) && err_no(err) != FS_ERR_EXISTS) {
        DEBUG_ERR(err, "vfs_mkdir");
    }
    /* assert(err_is_ok(err)); */

    err = vfs_mount(argv[2], argv[3]);
    assert(err_is_ok(err));

    err = replay_export(NULL, export_cb, connect_cb,
                        get_default_waitset(),
                        IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(err));

    for(;;) {
        err = event_dispatch(get_default_waitset());
        assert(err_is_ok(err));
    }

    return 0;
}
