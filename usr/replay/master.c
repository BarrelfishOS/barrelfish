#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
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
#endif
#include "defs.h"

void cache_print_stats(void);

static struct trace_entry *trace = NULL, *tend = NULL;

#define MAX_LINE        1024
#define MAX_SLAVES      64

struct qelem {
    replay_eventrec_t er;
    struct qelem *next, *prev;
};

struct slave {
    int pid[MAX_PIDS];
    struct qelem *queue, *qend;
#ifndef __linux__
    struct replay_binding *b;
#else
    int socket;
    ssize_t sentbytes;
    char sendbuf[256];
    int num;
#endif
};

static struct slave slaves[MAX_SLAVES];
static int num_slaves, num_finished;

struct writer {
    int fnum, pid;
    struct slave *slave;
    struct writer *prev, *next;
};

static struct writer *writers = NULL;
static bool bound;

#ifdef __linux__
static int sock[MAX_SLAVES];
#endif

#ifndef __linux__
static void event_done(struct replay_binding *b, uint32_t fnum)
{
    /* if(te->op == TOP_Close) { */
        // See if it was a writer and remove

    printf("writer done for %u\n", fnum);

        for(struct writer *w = writers; w != NULL; w = w->next) {
            if(w->fnum == fnum) {
                assert(w != NULL);
                if(w != writers) {
                    assert(w != NULL);
                    assert(w->prev != NULL);
                    w->prev->next = w->next;
                } else {
                    writers = w->next;
                }
                free(w);
                break;
            }
        }
    /* } */
}

static void finish_handler(struct replay_binding *b)
{
    num_finished++;
}

static struct replay_rx_vtbl replay_vtbl = {
    .event_done = event_done,
    .finished = finish_handler,
};

static void replay_bind_cont(void *st, errval_t err, struct replay_binding *b)
{
    static int slavenum = 0;
    struct slave *sl = &slaves[slavenum];
    slavenum++;

    assert(err_is_ok(err));
    sl->b = b;
    b->rx_vtbl = replay_vtbl;
    bound = true;
    /* printf("assigned binding to %p\n", sl); */
}
#else
static inline uint64_t rdtsc(void)
{
    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t)edx << 32) | eax;
}

static ssize_t send_buf(struct slave *s, replay_eventrec_t *er)
{
    ssize_t r;

    /* if(er->fline <= lastline) { */
    /*     printf("master: line repeat! %d > %d\n", er->fline, lastline); */
    /* } */
    /* lastline = er->fline; */

    if(s->sentbytes != -1) {
        char *bufpos = s->sendbuf + s->sentbytes;
        r = send(s->socket, bufpos, sizeof(replay_eventrec_t) - s->sentbytes, MSG_DONTWAIT);
        if(r == -1) {
            return r;
        } else {
            s->sentbytes += r;
            assert(s->sentbytes <= sizeof(replay_eventrec_t));
            if(s->sentbytes == sizeof(replay_eventrec_t)) {
                s->sentbytes = -1;
            }
            errno = EAGAIN;
            return -1;
        }
    }

    r = send(s->socket, er, sizeof(replay_eventrec_t), MSG_DONTWAIT);
    if(r == -1) {
        return r;
    }

    if(r < sizeof(replay_eventrec_t)) {
        memcpy(s->sendbuf, er, sizeof(replay_eventrec_t));
        s->sentbytes = r;
        return sizeof(replay_eventrec_t);
    }
    assert(r == sizeof(replay_eventrec_t));
    return r;
}
#endif

static bool printall = false;

int main(int argc, char *argv[])
{
#ifndef __linux__
    if(argc < 5) {
        printf("Usage: %s tracefile nslaves mountdir mount-URL\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    errval_t err = vfs_mkdir(argv[3]);
    assert(err_is_ok(err));

    err = vfs_mount(argv[3], argv[4]);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_mount");
    }
    assert(err_is_ok(err));
#else
    if(argc < 3) {
        printf("Usage: %s tracefile nslaves\n", argv[0]);
        exit(EXIT_FAILURE);
    }
#endif

    memset(slaves, 0, sizeof(struct slave) * MAX_SLAVES);

    printf("reading tracefile...\n");

    // Parse trace file into memory records
    FILE *f = fopen(argv[1], "r");
    assert(f != NULL);
    int linen = 0;
    while(!feof(f)) {
        char line[MAX_LINE];
        if(fgets(line, MAX_LINE, f) == NULL) {
            break;
        }

        linen++;

        if(linen % 100 == 0) {
            printf("line = %d\n", linen);
        }

        /* if(linen >= 41352 && linen <= 41365) { */
        /*     printf("got line %d: %s\n", linen, line); */
        /* } */

        size_t fnum, size;
        char flags[1024];
        int fd;
        unsigned int pid;

        struct trace_entry *te = malloc(sizeof(struct trace_entry));
        assert(te != NULL);

        if(sscanf(line, "open %zu %s %d %u", &fnum, flags, &fd, &pid) >= 4) {
            te->op = TOP_Open;
            te->fd = fd;
            te->u.fnum = fnum;
        } else if(sscanf(line, "close %d %u", &fd, &pid) >= 2) {
            te->op = TOP_Close;
            te->fd = fd;
        } else if(sscanf(line, "read %d %zu %u", &fd, &size, &pid) >= 3) {
            te->op = TOP_Read;
            te->fd = fd;
            te->u.size = size;
        } else if(sscanf(line, "write %d %zu %u", &fd, &size, &pid) >= 3) {
            te->op = TOP_Write;
            te->fd = fd;
            te->u.size = size;
        } else if(sscanf(line, "creat %zu %s %d %u", &fnum, flags, &fd, &pid) >= 4) {
            te->op = TOP_Create;
            te->fd = fd;
            te->u.fnum = fnum;
        } else if(sscanf(line, "unlink %zu %s %u", &fnum, flags, &pid) >= 3) {
            te->op = TOP_Unlink;
            te->u.fnum = fnum;
        } else if(sscanf(line, "exit %u", &pid) >= 1) {
            te->op = TOP_Exit;
        } else {
            printf("Invalid line %d: %s\n", linen, line);
            exit(EXIT_FAILURE);
        }

        // There's always a PID
        te->pid = pid;
        te->fline = linen;

        // If we have flags, set them now
        if(te->op == TOP_Open || te->op == TOP_Create) {
            if(!strcmp(flags, "rdonly")) {
                te->mode = FLAGS_RdOnly;
            } else if(!strcmp(flags, "wronly")) {
                te->mode = FLAGS_WrOnly;
            } else if(!strcmp(flags, "rdwr")) {
                te->mode = FLAGS_RdWr;
            } else {
                printf("Invalid open flags: %s\n", flags);
                exit(EXIT_FAILURE);
            }
        }

        // Link it in with the rest of the list (forward order)
        te->next = NULL;
        if(trace == NULL) {
            trace = te;
        } else {
            tend->next = te;
        }
        tend = te;
    }
    fclose(f);

    printf("tracefile read\n");

    uint64_t tscperms;
    num_slaves = atoi(argv[2]);
#ifndef __linux__
    err = sys_debug_get_tsc_per_ms(&tscperms);
    assert(err_is_ok(err));

    // Connect to all slaves
    for(int i = 0; i < num_slaves; i++) {
        char name[128];
        iref_t iref;

        int r = snprintf(name, 128, "replay_slave.%u", i + 1);
        assert(r != -1);

        err = nameservice_blocking_lookup(name, &iref);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "could not lookup IREF for replay slave");
            abort();
        }

        bound = false;
        err = replay_bind(iref, replay_bind_cont, NULL, get_default_waitset(),
                          IDC_BIND_FLAGS_DEFAULT);
        if(err_is_fail(err)) {
            DEBUG_ERR(err, "replay_bind");
        }

        while(!bound) {
            err = event_dispatch(get_default_waitset());
            assert(err_is_ok(err));
        }

        printf("bound to slave %d\n", i);
    }
#else
    tscperms = 533000;

    printf("connecting to slaves...\n");

    for(int i = 0; i < num_slaves; i++) {
        sock[i] = socket(AF_INET, SOCK_STREAM, 0);
        assert(sock[i] != -1);

        struct sockaddr_in a = {
            .sin_family = PF_INET,
            .sin_port = htons(0),
            .sin_addr = {
                .s_addr = htonl(INADDR_ANY)
            }
        };
        int r = bind(sock[i], (struct sockaddr *)&a, sizeof(a));
        assert(r == 0);

        char host[128];
        snprintf(host, 128, "rck%02u", i + 1);

        printf("connecting to '%s'\n", host);

        struct hostent *h;
        h = gethostbyname(host);
        assert(h != NULL && h->h_length == sizeof(struct in_addr));

        struct sockaddr_in sa;
        sa.sin_port = htons(1234);
        sa.sin_addr = *(struct in_addr *)h->h_addr_list[0];

        r = connect(sock[i], (struct sockaddr *)&sa, sizeof(sa));
        if(r < 0) {
            printf("connect: %s\n", strerror(errno));
        }
        assert(r == 0);

        struct slave *sl = &slaves[i];
        sl->socket = sock[i];
        sl->sentbytes = -1;
        sl->num = i;
    }
#endif

    printf("starting replay\n");

    /* for(struct trace_entry *te = trace; te != NULL; te = te->next) { */
    /*     static int cnt = 0; */
    /*     printf("%d: %d, %zu, %d, %d, %d, fline %d\n", */
    /*            cnt, te->op, te->u.fnum, te->fd, te->mode, te->pid, te->fline); */
    /*     cnt++; */
    /* } */

    uint64_t start = rdtsc();

    // Start trace replay
    for(struct trace_entry *te = trace; te != NULL; te = te->next) {
        // Distribute work to slaves -- either they are empty (PID ==
        // 0) or they already execute for a PID, in which case we keep
        // sending them that PID's work until the PID exits)

        static int cnt = 0;

        /* if(((cnt * 100) / linen) % 5 == 0) { */
            /* printf("%d / %d\n", cnt, linen); */
        /* } */
        cnt++;

        /* printall = false; */
        /* if(cnt == 6186 || cnt == 5840) { */
        /*     printall = true; */
        /* } */

        // If this is an exit, remove the PID and continue
        if(te->op == TOP_Exit) {
            int i;
            /* printf("PIDs: "); */
            for(i = 0; i < num_slaves; i++) {
                /* printf("%u ", slaves[i].pid); */
                for(int j = 0; j < MAX_PIDS; j++) {
                    if(slaves[i].pid[j] == te->pid) {
                        slaves[i].pid[j] = 0;
                        goto outexit;
                    }
                }
            }
        outexit:
            /* printf("\n"); */

            if(i < num_slaves) {
                continue;
            } else {
                printf("%d: exit on non-existant PID (%u), file line %d\n",
                       cnt, te->pid, te->fline);
                exit(EXIT_FAILURE);
            }
        }

        if(printall) {
            printf("find slave\n");
        }

    /* again: */
        // Find a slave with the same PID
        struct slave *emptyslave = NULL, *s = NULL;
        int i;
        for(i = 0; i < num_slaves; i++) {
            s = &slaves[i];

            /* if(s->pid == 0) { */
            /*     /\* printf("slave %d is the empty slave\n", i); *\/ */
            /*     emptyslave = s; */
            /* } */

            for(int j = 0; j < MAX_PIDS; j++) {
                if(s->pid[j] == te->pid) {
                    goto out;
                }
            }
        }
    out:

        // Didn't find one, find an empty one
        if(i == num_slaves) {
            // No empty slave -- wait for something to happen and try again
            if(emptyslave == NULL) {
                // Pick one randomly
                int randslave = rand() / (RAND_MAX / num_slaves);
                assert(randslave < num_slaves);
                s = &slaves[randslave];

                /* printf("no empty slave\n"); */
                /* err = event_dispatch(get_default_waitset()); */
                /* assert(err_is_ok(err)); */
                /* printf("past no empty slave\n"); */
                /* goto again; */
            } else {
                s = emptyslave;
            }
        }

        // Assign slave this PID
        int j;
        for(j = 0; j < MAX_PIDS; j++) {
            if(s->pid[j] == 0 || s->pid[j] == te->pid) {
                break;
            }
        }
        assert(j < MAX_PIDS);
        s->pid[j] = te->pid;

        /* if(i == num_slaves) { */
        /*     printf("found empty slave\n"); */
        /* } else { */
        /*     printf("found slave %d, PID %d\n", i, s->pid); */
        /* } */

        /* if(te->fline >= 41352 && te->fline <= 41365) { */
        /*     printf("%d: %d, %zu, %d, %d, %d to slave %d, fline %d\n", */
        /*            cnt, te->op, te->u.fnum, te->fd, te->mode, te->pid, i, te->fline); */
        /* } */

#if 1
        if(te->op == TOP_Exit) {
            printf("exit %u\n", te->pid);
            // See if it was a writer and remove
            for(struct writer *w = writers; w != NULL; w = w->next) {
                assert(te != NULL);
                assert(w != NULL);
                if(w->pid == te->pid) {
                    assert(w != NULL);
                    if(w != writers) {
                        assert(w != NULL);
                        assert(w->prev != NULL);
                        w->prev->next = w->next;
                    } else {
                        writers = w->next;
                    }
                    free(w);
                    break;
                }
            }
        }
#endif

        // If someone opens a file, we have to make sure
        // that anyone else has stopped writing to that file.
        if(te->op == TOP_Open || te->op == TOP_Create) {
            /* for(;;) { */
                if(printall) {
                    printf("find writer\n");
                }

                struct writer *w;
                for(w = writers; w != NULL; w = w->next) {
                    assert(w != NULL);
                    assert(te != NULL);
                    if(w->fnum == te->u.fnum) {
                        // Somebody's writing to this file -- wait for him to finish
                        /* printf("Warning: Concurrent file writer, fline = %d, fnum = %zu\n", */
                        /*        te->fline, te->u.fnum); */
                        /* assert(!"NYI"); */
                        break;
                    }
                }

#if 0
                // There's a writer -- wait for it to finish
                if(w != NULL) {
                    printf("Waiting for close from previous writer\n");
                    err = event_dispatch(get_default_waitset());
                    assert(err_is_ok(err));
                } else {
                    break;
                }
#endif
            }

            // Add a new writer to the list
            if(te->mode != FLAGS_RdOnly) {
                struct writer *w = malloc(sizeof(struct writer));

                /* printf("new writer to file %zu\n", te->u.fnum); */

                //                printall = true;

                w->fnum = te->u.fnum;
                w->pid = te->pid;
                w->slave = s;
                w->prev = NULL;
                w->next = writers;
                if(writers) {
                    w->next->prev = w;
                }
                writers = w;
            }
    /* } */

        // Send it to the slave
        replay_eventrec_t er = {
            .op = te->op,
            .fd = te->fd,
            .mode = te->mode,
            .fline = te->fline,
            .pid = te->pid,
        };

        switch(te->op) {
        case TOP_Open:
        case TOP_Create:
        case TOP_Unlink:
            er.fnumsize = te->u.fnum;
            break;

        case TOP_Read:
        case TOP_Write:
            er.fnumsize = te->u.size;
            break;

        default:
            // Nothing
            break;
        }

        if(printall) {
            printf("sending\n");
        }

        assert(s != NULL);
        if(s->queue == NULL) {
#ifndef __linux__
            assert(s->b != NULL);
            err = s->b->tx_vtbl.event(s->b, NOP_CONT, er);
            if(err_is_fail(err)) {
                if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
                    if(printall) {
                        printf("queueing\n");
                    }
                    /* printf("queueing\n"); */
                    struct qelem *q = malloc(sizeof(struct qelem));
                    assert(q != NULL);
                    q->er = er;
                    q->next = s->queue;
                    if(s->queue != NULL) {
                        s->queue->prev = q;
                    } else {
                        assert(s->qend == NULL);
                    }
                    q->prev = NULL;
                    s->queue = q;
                    if(s->qend == NULL) {
                        s->qend = q;
                    }

                    // Need to keep pumping and dispatch at least one event
                    /* printf("busy\n"); */
                    err = event_dispatch(get_default_waitset());
                    assert(err_is_ok(err));
                } else {
                    DEBUG_ERR(err, "event");
                    assert(err_is_ok(err));
                }
            }
#else
            if(printall) {
                printf("send_buf 1\n");
            }
            ssize_t r = send_buf(s, &er);
            if(printall) {
                printf("after send_buf 1\n");
            }
            /* ssize_t r = send(s->socket, &er, sizeof(er), MSG_DONTWAIT); */
            if(r == -1) {
                if(errno == EAGAIN) {
                    if(printall) {
                        printf("queueing\n");
                    }
                    /* printf("queueing\n"); */
                    struct qelem *q = malloc(sizeof(struct qelem));
                    assert(q != NULL);
                    q->er = er;
                    q->next = s->queue;
                    if(s->queue != NULL) {
                        s->queue->prev = q;
                    } else {
                        assert(s->qend == NULL);
                    }
                    q->prev = NULL;
                    s->queue = q;
                    if(s->qend == NULL) {
                        s->qend = q;
                    }
                } else {
                    printf("send_message to %d: %s\n", s->num, strerror(errno));
                    abort();
                }
            } else {
                if(r != sizeof(er)) {
                    printf("send_message: r == %zd, size = %zu\n", r, sizeof(er));
                }
                assert(r == sizeof(er));
            }
#endif
        } else {
            // Put on slave's queue
            if(printall) {
                printf("queueing\n");
            }
            /* printf("queueing\n"); */
            struct qelem *q = malloc(sizeof(struct qelem));
            assert(q != NULL);
            q->er = er;
            q->next = s->queue;
            if(s->queue != NULL) {
                s->queue->prev = q;
            } else {
                assert(s->qend == NULL);
            }
            q->prev = NULL;
            s->queue = q;
            if(s->qend == NULL) {
                s->qend = q;
            }
        }

        if(printall) {
            printf("resending\n");
        }

        // Resend items that got queued
        for(i = 0; i < num_slaves; i++) {
            s = &slaves[i];
            for(struct qelem *q = s->qend; q != NULL;) {
                // Need to keep pumping and dispatch at least one event

#ifndef __linux__
                err = s->b->tx_vtbl.event(s->b, NOP_CONT, q->er);
                if(err_is_ok(err)) {
                    if(printall) {
                        printf("resent %d\n", q->er.fline);
                    }
                    struct qelem *oldq = q;
                    s->qend = q = q->prev;
                    free(oldq);
                    if(s->qend == NULL) {
                        s->queue = NULL;
                    }
                } else if(err_no(err) != FLOUNDER_ERR_TX_BUSY) {
                    DEBUG_ERR(err, "error");
                    abort();
                } else {
                    // still busy, can't dequeue anything
                    /* printf("busy2\n"); */
                    err = event_dispatch(get_default_waitset());
                    assert(err_is_ok(err));
                    break;
                    /* printf("still busy\n"); */
                    /* qend = q = q->prev; */
                }
#else
                if(printall) {
                    printf("send_buf 2\n");
                }
                ssize_t r = send_buf(s, &q->er);
                if(printall) {
                    printf("after send_buf 2\n");
                }
                /* ssize_t r = send(s->socket, &q->er, sizeof(q->er), MSG_DONTWAIT); */
                if(r == -1) {
                    if(errno == EAGAIN) {
                        break;
                    } else {
                        printf("send_message to %d: %s\n", s->num, strerror(errno));
                        abort();
                    }
                } else {
                    if(r != sizeof(er)) {
                        printf("send_message: r == %zd, size = %zu\n", r, sizeof(er));
                    }
                    assert(r == sizeof(er));
                    struct qelem *oldq = q;
                    s->qend = q = q->prev;
                    free(oldq);
                    if(s->qend == NULL) {
                        s->queue = NULL;
                    }
                }
#endif
            }
        }
    }

    printf("draining\n");

    // Drain the queue
    for(int i = 0; i < num_slaves; i++) {
        struct slave *s = &slaves[i];
        for(struct qelem *q = s->qend; q != NULL;) {
#ifndef __linux__
            err = s->b->tx_vtbl.event(s->b, NOP_CONT, q->er);
            if(err_is_ok(err)) {
                /* printf("resent %d\n", q->er.fline); */
                struct qelem *oldq = q;
                s->qend = q = q->prev;
                free(oldq);
                if(s->qend == NULL) {
                    s->queue = NULL;
                }
            } else if(err_no(err) != FLOUNDER_ERR_TX_BUSY) {
                DEBUG_ERR(err, "error");
                abort();
            } else {
                // still busy, can't dequeue anything
                break;
                /* printf("still busy\n"); */
                /* qend = q = q->prev; */
            }
#else
            ssize_t r = send_buf(s, &q->er);
            /* ssize_t r = send(s->socket, &q->er, sizeof(q->er), MSG_DONTWAIT); */
            if(r == -1) {
                if(errno == EAGAIN) {
                    break;
                } else {
                    printf("send_message to %d: %s\n", s->num, strerror(errno));
                    abort();
                }
            } else {
                if(r != sizeof(q->er)) {
                    printf("send_message: r == %zd, size = %zu\n", r, sizeof(q->er));
                }
                assert(r == sizeof(q->er));
                struct qelem *oldq = q;
                s->qend = q = q->prev;
                free(oldq);
                if(s->qend == NULL) {
                    s->queue = NULL;
                }
            }
#endif
        }
    }

    for(int i = 0; i < num_slaves; i++) {
        struct slave *s = &slaves[i];
        replay_eventrec_t er = {
            .op = TOP_End
        };
#ifndef __linux__
        err = s->b->tx_vtbl.event(s->b, NOP_CONT, er);
        assert(err_is_ok(err));
#else
        ssize_t r = send_buf(s, &er);
        if(r == -1) {
            if(errno == EAGAIN) {
                printf("buffer full\n");
                abort();
            } else {
                printf("send_message to %d: %s\n", s->num, strerror(errno));
                abort();
            }
        }
#endif
    }

    do {
        err = event_dispatch(get_default_waitset());
        assert(err_is_ok(err));
    } while(num_finished < num_slaves);

    uint64_t end = rdtsc();

#if 0
    // Wait for 5 seconds
    uint64_t beg = rdtsc();
    while(rdtsc() - beg < tscperms * 5000) {
#ifndef __linux__
        thread_yield();
#else
        sched_yield();
#endif
    }
#endif

    printf("replay done, took %" PRIu64" ms\n", (end - start) / tscperms);

#ifndef __linux__
    cache_print_stats();
#endif

    return 0;
}
