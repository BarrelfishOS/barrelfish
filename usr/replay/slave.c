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

//static int pidconv[MAX_PIDS] = { 0 };
//static FILE *fdconv[MAX_PIDS][MAX_FD_CONV];
//static int fnumconv[MAX_PIDS][MAX_FD_CONV];
//static bool writerconv[MAX_PIDS][MAX_FD_CONV];

static int openfiles = 0;
static FILE *fd2fptr[MAX_FD_CONV] = {0};
static int  fd2fname[MAX_FD_CONV] = {0};
static char data[MAX_DATA];
static uint64_t tscperms;


#ifndef __linux__
static void handle_event(struct replay_binding *b, replay_eventrec_t er)
#else
static void handle_event(replay_eventrec_t er)
#endif
{
    static int pid = 0;
    static int op_id = 0;
    //static int wrcnt = 0;
    //static int rdcnt = 0;
    //static int opencnt = 0;
    //static int closecnt = 0;
    //static unsigned long total_ticks=0;
    //unsigned long handle_ticks = rdtsc();
    /* pick a file for this operation */
    // (only needed for Open/Create/Unlink)
    char fname[256];
    snprintf(fname, 256, "%s/%u", defdir, er.fnumsize);

    /* protocol:
     * - master will send consecutive operations with the same pid
     * - the pid will change after an an Op_Exit, and a subsequent Open/Create
     */

    // sanity chacks
    if (pid == 0) { // client is not associated with a pid
        assert(er.op == TOP_Open || er.op == TOP_Create || er.op == TOP_End);
    } else {         // client is associated with a pid
        assert(er.pid == pid);
    }

    op_id++;
    dmsg("SLAVE[%u]: REQ pid:%d op:%d [op_id:%d]\n", disp_get_core_id(), er.pid, er.op, op_id);
    switch(er.op) {
    case TOP_Open:
    case TOP_Create: {
        //uint64_t ticks = rdtsc();
        char *flags = NULL;

        if (pid == 0) {
            // new pid
            pid = er.pid;
            dmsg("SLAVE[%u]: got new pid:%d\n", disp_get_core_id(), pid);
        }

        /* set flags */
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

        /* open the file */

        /* assert(fd2fptr[er.fd] == NULL);
         * seems that some close() miss from the file:
         *  $ egrep -c -e 'close' <kernelcompile.trace.anon
         *  10779
         *  $ egrep -c -e '(open|creat)' <kernelcompile.trace.anon
         *  10974
         */

        fd2fptr[er.fd] = fopen(fname, flags);
        fd2fname[er.fd] = er.fnumsize;
        if (fd2fptr[er.fd] != NULL) {
            openfiles++;
        } else {
            printf("Open file:%s (%s) failed\n", fname, flags);
            assert(0);
        }
        //ticks = rdtsc() - ticks;
        //msg("SLAVE[%d] OPEN %d took %lu ticks (%lf ms)\n", disp_get_core_id(), opencnt++, ticks, (double)ticks/(double)tscperms);
        break;
    }

    case TOP_Unlink: {
        int ret = unlink(fname);
        assert(ret != -1);
        break;
    }

    case TOP_Read: {
        //uint64_t ticks = rdtsc();
        if (er.fnumsize > MAX_DATA) {
            printf("er.fnumsize == %u\n", er.fnumsize);
            assert(0);
        }
        FILE *fptr = fd2fptr[er.fd];
        assert(fptr != NULL);
        int ret = fread(data, 1, er.fnumsize, fptr);
        if (ret != er.fnumsize) {
            msg("SLAVE[%u] [R] op_id:%d er.fnumsize:%u, read:%d fname:%d pid:%d error:%d eof:%d pos:%ld\n", disp_get_core_id(), op_id, er.fnumsize, ret, fd2fname[er.fd], er.pid, ferror(fptr), feof(fptr), ftell(fptr));
        }
        //ticks = rdtsc() - ticks;
        //msg("SLAVE[%d] READ %d took %lu ticks (%lf ms)\n", disp_get_core_id(), rdcnt++, ticks, (double)ticks/(double)tscperms);
        break;
    }

    case TOP_Write: {
        if (er.fnumsize > MAX_DATA) {
            printf("er.fnumsize == %u\n", er.fnumsize);
            assert(0);
        }
        FILE *fptr = fd2fptr[er.fd];
        assert(fptr != NULL);
        int ret = fwrite(data, 1, er.fnumsize, fptr);
        if (ret != er.fnumsize) {
            msg("[W] op_id:%d er.fnumsize:%u, write:%d fname:%d pid:%d error:%d eof:%d pos:%ld\n", op_id, er.fnumsize, ret, fd2fname[er.fd], er.pid, ferror(fptr), feof(fptr), ftell(fptr));
        }
        break;
    }

    case TOP_Close: {
        FILE *fptr = fd2fptr[er.fd];
        assert(fptr != NULL);
        //uint64_t ticks = rdtsc();
        int ret = fclose(fptr);
        //ticks = rdtsc() - ticks;
        //msg("SLAVE[%d] CLOSE %d took %lu ticks (%lf ms)\n", disp_get_core_id(), closecnt++, ticks, (double)ticks/(double)tscperms);
        assert(ret == 0);
        openfiles--;
        fd2fptr[er.fd] = NULL;
        fd2fname[er.fd] = 0;
        break;
    }

    case TOP_End: {
        dmsg("SLAVE[%u]: END\n", disp_get_core_id());
        //total_ticks += (rdtsc() - handle_ticks);
        //msg("SLAVE[%u]: END took %lu ticks (%lf ms)\n", disp_get_core_id(), total_ticks, (double)total_ticks/(double)tscperms);
        errval_t err = b->tx_vtbl.finished(b, NOP_CONT);
        assert(err_is_ok(err));
        break;
    }

    case TOP_Exit: {
        dmsg("SLAVE[%u]: TOP_Exit on %d\n", disp_get_core_id(), er.pid);
        errval_t err = b->tx_vtbl.task_completed(b, NOP_CONT, er.pid);
        pid = 0;
        assert(err_is_ok(err));
        break;
    }

    default:
        printf("Invalid request: %d\n", er.op);
        assert(0);
        break;
    }

    //total_ticks += (rdtsc() - handle_ticks);
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

    printf("replay slave up\n");
#ifndef __linux__
    assert(err_is_ok(sys_debug_get_tsc_per_ms(&tscperms)));

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
