#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#ifndef __linux__
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <barrelfish/nameservice_client.h>
#include <if/replay_defs.h>
#else
#include <stdbool.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netdb.h>
#include <errno.h>
#include <sched.h>
#include <inttypes.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif
#include "defs.h"

#define RED(x) "\033[31m" x "\033[0m\n"

static char *defdir;

#ifndef __linux__
static void export_cb(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));
    char name[256];
    snprintf(name, 256, "replay_slave.%u", disp_get_core_id());
    err = nameservice_register(name, iref);
    assert(err_is_ok(err));
}
#else
static int connsock = -1;
#endif

#define MAX_FD_CONV     256
#define MAX_DATA        (1 * 1024 * 1024)

static int pidconv[MAX_PIDS] = { 0 };
static FILE *fdconv[MAX_PIDS][MAX_FD_CONV];
//static int fnumconv[MAX_PIDS][MAX_FD_CONV];
//static bool writerconv[MAX_PIDS][MAX_FD_CONV];
//static char data[MAX_DATA];

//static int openfiles = 0;

#ifndef __linux__
static void handle_event(struct replay_binding *b, replay_eventrec_t er)
#else
static void handle_event(replay_eventrec_t er)
#endif
{
    char fname[256];
    char *flags = NULL;
    //printf("%s:%s() :: ENTER\n", __FILE__, __FUNCTION__);
    static int pid = 0;

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

    #if 0
    int mypid;
    for(mypid = 0; mypid < MAX_PIDS; mypid++) {
        if(pidconv[mypid] == er.pid) {
            break;
        }

        if(pidconv[mypid] == 0) {
#ifndef __linux__
            /* printf("%d: mypid %d is PID %d\n", disp_get_core_id(), mypid, er.pid); */
#else
            /* printf("mypid %d is PID %d\n", mypid, er.pid); */
#endif
            pidconv[mypid] = er.pid;
            break;
        }
    }
    assert(mypid < MAX_PIDS);
    #endif

    snprintf(fname, 256, "%s/%u", defdir, er.fnumsize);
    //FILE *f = fdconv[mypid][er.fd];
    //int ret;

    /* if(er.fline >= 41352 && er.fline <= 41365) { */
    /*     printf("%s is %p, %d, fline = %d, mypid = %d\n", fname, f, er.fd, er.fline, mypid); */
    /* } */

    if (pid == 0 && !(er.op == TOP_Open || er.op == TOP_End))
        printf(">>>>>>>>>>>>>>>>>>>. Unexpected op: %d [pid=%d]\n", er.op, er.pid);

    if (pid != 0 && er.pid != pid && er.op != TOP_End)
        printf(">>>>>>>>>>>>>>>>>>>. Unexpected pid on: %d [pid=%d,current pid=%d]\n", er.op, er.pid, pid);

    //if (er.pid == 24873)
    //    printf(">>>>>>>>>>>>>>>>>>>. op=%d pid=%d current pid=%d]\n", er.op, er.pid, pid);

    switch(er.op) {
    case TOP_Open:
    case TOP_Create:
        if (pid == 0) {
            printf("SLAVE[%u]: got new pid:%d\n", disp_get_core_id(), (pid = er.pid));
        }
        #if 0
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
#ifndef __linux__
            printf("%d: open fname = %s, fd = %d, fline = %d, pid = %d, mypid = %d\n", disp_get_core_id(), fname, er.fd, er.fline, er.pid, mypid);
#else
            printf("open fname = %s, fd = %d, fline = %d, pid = %d, mypid = %d\n", fname, er.fd, er.fline, er.pid, mypid);
#endif
            printf("errno = %d (%s)\n", errno, strerror(errno));
        }
        openfiles++;
        /* printf("open files = %d\n", openfiles); */
        assert(fdconv[mypid][er.fd] != NULL);
        /* if(er.fline == 41352) { */
        /*     printf("file is open %d, %d = %p\n", mypid, er.fd, fdconv[mypid][er.fd]); */
        /* } */
        #endif
        break;

    case TOP_Unlink:
        #if 0
        ret = unlink(fname);
        assert(ret != -1);
        #endif
        break;

    case TOP_Read:
        #if 0
        if(er.fnumsize > MAX_DATA) {
            printf("er.fnumsize == %u\n", er.fnumsize);
        }
        assert(er.fnumsize <= MAX_DATA);
        /* printf("read fnumsize = %d, fd = %d, fline = %d, pid = %d, mypid = %d, f = %p\n", er.fnumsize, er.fd, er.fline, er.pid, mypid, f); */
        if(f == NULL) {
#ifndef __linux__
        printf("%d: read fnumsize = %d, fd = %d, fline = %d, pid = %d, mypid = %d, f = %p\n", disp_get_core_id(), er.fnumsize, er.fd, er.fline, er.pid, mypid, f);
#else
        printf("read fnumsize = %d, fd = %d, fline = %d, pid = %d, mypid = %d, f = %p\n", er.fnumsize, er.fd, er.fline, er.pid, mypid, f);
#endif
        }
        assert(f != NULL);
        ret = fread(data, er.fnumsize, 1, f);
        /* if(ret != er.fnumsize) { */
        /*     printf("read: ret == %d, fnumsize == %d\n", ret, er.fnumsize); */
        /* } */
        /* assert(ret == er.fnumsize); */
        #endif
        break;

    case TOP_Write:
        #if 0
        assert(er.fnumsize <= MAX_DATA);
        if(f == NULL) {
#ifndef __linux__
            /* printf("%d: write fnumsize = %d, fd = %d, fline = %d\n", disp_get_core_id(), er.fnumsize, er.fd, er.fline); */
#endif
        } else {
            assert(f != NULL);
            ret = fwrite(data, er.fnumsize, 1, f);
        }
        /* if(ret != er.fnumsize) { */
        /*     printf("write: ret == %d, fnumsize == %d\n", ret, er.fnumsize); */
        /* } */
        /* assert(ret == er.fnumsize); */
        #endif
        break;

    case TOP_Close:
        #if 0
        /* printf("%d: close fd = %d, fline = %d, fname = %d\n", disp_get_core_id(), er.fd, er.fline, fnumconv[mypid][er.fd]); */
        /* if(f == NULL) { */
        /*     printf("%d: Warning: Double close: %d, %u, %d, %d\n", */
        /*            disp_get_core_id(), er.op, er.fnumsize, er.fd, er.mode); */
        /* } else { */
        if(f != NULL) {
            assert(f != NULL);
            ret = fclose(f);
            assert(ret != -1);
            openfiles--;
        }
        fdconv[mypid][er.fd] = NULL;

        /* if(writerconv[mypid][er.fd]) { */
        /*     errval_t err = b->tx_vtbl.event_done(b, NOP_CONT, fnumconv[mypid][er.fd]); */
        /*     assert(err_is_ok(err)); */
        /* } */
        #endif
        break;

    case TOP_End:
        printf("SLAVE[%u]: END\n", disp_get_core_id());
        {
            errval_t err = b->tx_vtbl.finished(b, NOP_CONT);
            assert(err_is_ok(err));
        }
        break;

    case TOP_Exit: {
        printf(RED("SLAVE[%u]: TOP_Exit on %d ***********************"), disp_get_core_id(), er.pid);
        errval_t err = b->tx_vtbl.task_completed(b, NOP_CONT, er.pid);
        pid = 0;
        assert(err_is_ok(err));
        break;
    }

    default:
        printf("Invalid request: %d\n", er.op);
        break;
    }
}

#ifndef __linux__
static struct replay_rx_vtbl replay_vtbl = {
    .event = handle_event,
};

static errval_t connect_cb(void *st, struct replay_binding *b)
{
    b->rx_vtbl = replay_vtbl;
    return SYS_ERR_OK;
}
#endif

int main(int argc, char *argv[])
{
#ifndef __linux__
    assert(argc >= 4);
#else
    assert(argc >= 2);
#endif
    defdir = argv[1];

    memset(fdconv, 0, MAX_PIDS * MAX_FD_CONV * sizeof(FILE *));
    memset(pidconv, 0, MAX_PIDS * sizeof(int));

    printf("replay slave up\n");

#ifndef __linux__
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

    printf("%s:%s() :: slave starts servicing requests\n", __FILE__, __FUNCTION__);
    for(;;) {
        err = event_dispatch(get_default_waitset());
        assert(err_is_ok(err));
    }
#else
    /* { */
    /*     struct rlimit rl; */
    /*     rl.rlim_cur = 2048; */
    /*     rl.rlim_max = 2050; */
    /*     int r = setrlimit(RLIMIT_NOFILE, &rl); */
    /*     if(r == -1) { */
    /*         printf("setrlimit errno = %s\n", strerror(errno)); */
    /*     } */
    /*     assert(r == 0); */
    /* } */

    // Listen on port 1234
    int listensock = socket(AF_INET, SOCK_STREAM, 0);
    assert(listensock != -1);
    struct sockaddr_in a = {
        .sin_family = PF_INET,
        .sin_port = htons(1234),
        .sin_addr = {
            .s_addr = htonl(INADDR_ANY)
        }
    };
    int r = bind(listensock, (struct sockaddr *)&a, sizeof(a));
    if(r == -1) {
        printf("bind: %s\n", strerror(errno));
    }
    assert(r == 0);
    r = listen(listensock, 5);
    assert(r == 0);

    socklen_t sizea = sizeof(a);
    connsock = accept(listensock, (struct sockaddr *)&a, &sizea);
    assert(connsock != -1);
    assert(sizea == sizeof(a));

    int from = (ntohl(a.sin_addr.s_addr) & 0xff) - 1;
    printf("got connection from %d\n", from);

    int lastline = 0;
    for(;;) {
        replay_eventrec_t er;
        memset(&er, 0, sizeof(er));

        size_t remain = sizeof(er), pos = 0;
        char *buf = (char *)&er;
        do {
            ssize_t r = recv(connsock, buf + pos, remain, MSG_WAITALL);
            if(r == -1) {
                printf("recv errno = %s\n", strerror(errno));
                abort();
            }
            if(r == 0) {
                printf("end of session\n");
                return 0;
            }
            //            assert(r == sizeof(er));
            remain -= r;
            pos += r;
        } while(remain > 0);

        if(er.fline <= lastline) {
            printf("line repeat! %d <= %d\n", er.fline, lastline);
            return 1;
        }
        lastline = er.fline;

        handle_event(er);
    }
#endif

    return 0;
}
