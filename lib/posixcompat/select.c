/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/deferred.h>
#include <if/monitor_defs.h>
#include <assert.h>
#include <unistd.h>
#include <lwip/sys.h>
#include <lwip/sockets.h>
#include "unixsock.h"
#include <vfs/fdtab.h>
#include "posixcompat.h"

#define	MAX(a,b) (((a)>(b))?(a):(b))

struct timeout_event {
  bool fired;
};

static void timeout_fired(void *arg)
{
  struct timeout_event *toe = arg;
  assert(toe != NULL);
  toe->fired = true;
}


int select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
           struct timeval *timeout)
{
    struct monitor_binding *mb = get_monitor_binding();
    struct waitset ws, *monitor_ws = mb->waitset;
    bool wait_monitor = false;
    errval_t err;
    int retfds = 0;
    fd_set oreadfds, owritefds, oexceptfds, changed_ws;
    struct timeout_event toe = {
      .fired = false
    };

    // XXX: LWIP sockets and other FDs are mutually exclusive.
    // We currently only support lwip_select() or our own select(), not both
    // Check that this is the case and convert our FDs into lwIP FDs
    {
        bool othertype = false, lwiptype = false;
        fd_set lreadfds, lwritefds, lexceptfds;
        fd_set *preadfds = NULL, *pwritefds = NULL, *pexceptfds = NULL;
        int lnfds = 0;

        FD_ZERO(&lreadfds);
        FD_ZERO(&lwritefds);
        FD_ZERO(&lexceptfds);

        if(readfds != NULL) {
            preadfds = &lreadfds;
            for(int fd = 0; fd < nfds; fd++) {
                if(FD_ISSET(fd, readfds)) {
                    struct fdtab_entry *e = fdtab_get(fd);
                    if(e->type == FDTAB_TYPE_LWIP_SOCKET) {
                        lwiptype = true;
                        FD_SET(e->fd, &lreadfds);
                        lnfds = MAX(lnfds, e->fd);
                    } else {
                        othertype = true;
                    }
                }
            }
        }
        if(writefds != NULL) {
            pwritefds = &lwritefds;
            for(int fd = 0; fd < nfds; fd++) {
                if(FD_ISSET(fd, writefds)) {
                    struct fdtab_entry *e = fdtab_get(fd);
                    if(e->type == FDTAB_TYPE_LWIP_SOCKET) {
                        lwiptype = true;
                        FD_SET(e->fd, &lwritefds);
                        lnfds = MAX(lnfds, e->fd);
                    } else {
                        othertype = true;
                    }
                }
            }
        }
        if(exceptfds != NULL) {
            pexceptfds = &lexceptfds;
            for(int fd = 0; fd < nfds; fd++) {
                if(FD_ISSET(fd, exceptfds)) {
                    struct fdtab_entry *e = fdtab_get(fd);
                    if(e->type == FDTAB_TYPE_LWIP_SOCKET) {
                        lwiptype = true;
                        FD_SET(e->fd, &lexceptfds);
                        lnfds = MAX(lnfds, e->fd);
                    } else {
                        othertype = true;
                    }
                }
            }
        }

        if(lwiptype && othertype) {
            printf("XXX: select(): lwIP sockets and other FDs are mutually "
                   "exclusive. Program abort!\n");
            abort();
        }

        if(lwiptype) {
            lnfds++;
            lwip_mutex_lock();
            int r = lwip_select(lnfds, preadfds, pwritefds, pexceptfds,
                                timeout);
            lwip_mutex_unlock();
            if(r == -1) {
                return r;
            }

            // Convert FDs back to our FDs
            if(readfds != NULL) {
                FD_ZERO(readfds);
                for(int fd = 0; fd < lnfds; fd++) {
                    if(FD_ISSET(fd, &lreadfds)) {
                        struct fdtab_entry e = {
                            .type = FDTAB_TYPE_LWIP_SOCKET,
                            .fd = fd,
                        };
                        int pfd = fdtab_search(&e);
                        assert(pfd != -1);
                        FD_SET(pfd, readfds);
                    }
                }
            }
            if(writefds != NULL) {
                FD_ZERO(writefds);
                for(int fd = 0; fd < lnfds; fd++) {
                    if(FD_ISSET(fd, &lwritefds)) {
                        struct fdtab_entry e = {
                            .type = FDTAB_TYPE_LWIP_SOCKET,
                            .fd = fd,
                        };
                        int pfd = fdtab_search(&e);
                        assert(pfd != -1);
                        FD_SET(pfd, writefds);
                    }
                }
            }
            if(exceptfds != NULL) {
                FD_ZERO(exceptfds);
                for(int fd = 0; fd < lnfds; fd++) {
                    if(FD_ISSET(fd, &lexceptfds)) {
                        struct fdtab_entry e = {
                            .type = FDTAB_TYPE_LWIP_SOCKET,
                            .fd = fd,
                        };
                        int pfd = fdtab_search(&e);
                        assert(pfd != -1);
                        FD_SET(pfd, exceptfds);
                    }
                }
            }

            return r;
        }
    }

    FD_ZERO(&oreadfds);
    FD_ZERO(&owritefds);
    FD_ZERO(&oexceptfds);
    FD_ZERO(&changed_ws);

    // Loop until something we're waiting for has happened
    for(;;) {
        waitset_init(&ws);

        // Check list of readfds for events
        if(readfds != NULL) {
            for(int fd = 0; fd < nfds; fd++) {
                if(FD_ISSET(fd, readfds)) {
                    struct fdtab_entry *e = fdtab_get(fd);

                    switch(e->type) {
                    case FDTAB_TYPE_UNIX_SOCKET:
                        {
                            struct _unix_socket *us = e->handle;

                            if(us->passive) {
                                int i;

                                // Check for pending connection requests
                                for(i = 0; i < us->u.passive.max_backlog; i++) {
                                    if(us->u.passive.backlog[i] != NULL) {
                                        break;
                                    }
                                }

                                if(i == us->u.passive.max_backlog) {
                                    wait_monitor = true;
                                } else {
                                    FD_SET(fd, &oreadfds);
                                    retfds++;
                                }
                            } else {
                                // Check for incoming data
                                if(us->recv_buf_valid > 0) {
                                    FD_SET(fd, &oreadfds);
                                    retfds++;
                                } else {
                                    err = us->u.active.binding->change_waitset
                                        (us->u.active.binding, &ws);
                                    if(err_is_fail(err)) {
                                        USER_PANIC_ERR(err, "change_waitset");
                                    }
                                    FD_SET(fd, &changed_ws);
                                }
                            }
                        }
                        break;

                    default:
                        printf("select() on FD type %d NYI\n", e->type);
                        assert(!"NYI");
                        return -1;
                    }
                }
            }
        }

        // Check list of writefds for events
        if(writefds != NULL) {
            for(int fd = 0; fd < nfds; fd++) {
                if(FD_ISSET(fd, writefds)) {
                    struct fdtab_entry *e = fdtab_get(fd);

                    switch(e->type) {
                    case FDTAB_TYPE_UNIX_SOCKET:
                        {
                            struct _unix_socket *us = e->handle;

                            assert(!us->passive);

                            switch(us->u.active.mode) {
                            case _UNIX_SOCKET_MODE_CONNECTING:
                                wait_monitor = true;
                                break;

                            case _UNIX_SOCKET_MODE_CONNECTED:
                                if(us->send_buf != NULL) {
                                    err = us->u.active.binding->change_waitset
                                        (us->u.active.binding, &ws);
                                    if(err_is_fail(err)) {
                                        USER_PANIC_ERR(err, "change_waitset");
                                    }
                                    FD_SET(fd, &changed_ws);
                                } else {
                                    FD_SET(fd, &owritefds);
                                    retfds++;
                                }
                                break;

                            default:
                                assert(!"NYI");
                                break;
                            }
                        }
                        break;

                    default:
                        assert(!"NYI");
                        return -1;
                    }
                }
            }
        }

        // XXX: exceptfds ignored. Find out what to map to them.

        // Bail out if something happened
        if(retfds > 0 || toe.fired) {
            break;
        }

	    struct deferred_event timeout_event;

        if(timeout != NULL) {
	        delayus_t delay = timeout->tv_sec * 1000000 + timeout->tv_usec;
	        deferred_event_init(&timeout_event);
	        err = deferred_event_register(&timeout_event, &ws, delay, MKCLOSURE(timeout_fired, &toe));
	        assert(err_is_ok(err));
        }

        // Wait on monitor?
        if(wait_monitor) {
	        printf("%"PRIuDOMAINID": Need to wait on monitor\n", disp_get_domain_id());
            err = mb->change_waitset(mb, &ws);
            if(err_is_fail(err)) {
                USER_PANIC_ERR(err, "monitor change_waitset");
            }
        }

        // Wait on selected FDs
        err = event_dispatch(&ws);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "waitset_destroy");
        }

        // If waited on monitor, restore its old waitset
        if(wait_monitor) {
            err = mb->change_waitset(mb, monitor_ws);
            if(err_is_fail(err)) {
                USER_PANIC_ERR(err, "monitor change_waitset");
            }
        }

        // Restore all other waitsets
        // XXX: Assume they were all on the default waitset for now
        for(int fd = 0; fd < nfds; fd++) {
            if(FD_ISSET(fd, &changed_ws)) {
                struct fdtab_entry *e = fdtab_get(fd);

                switch(e->type) {
                case FDTAB_TYPE_UNIX_SOCKET:
                    {
                        struct _unix_socket *us = e->handle;
                        err = us->u.active.binding->change_waitset
                            (us->u.active.binding, get_default_waitset());
                        if(err_is_fail(err)) {
                            USER_PANIC_ERR(err, "change_waitset");
                        }
                    }
                    break;

                default:
                    assert(!"NYI");
                }
            }
        }

        err = waitset_destroy(&ws);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "waitset_destroy");
        }
    }

    err = waitset_destroy(&ws);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "waitset_destroy");
    }

    // Update FD sets
    if(readfds != NULL) {
        memcpy(readfds, &oreadfds, sizeof(fd_set));
    }
    if(writefds != NULL) {
        memcpy(writefds, &owritefds, sizeof(fd_set));
    }
    if(exceptfds != NULL) {
        memcpy(exceptfds, &oexceptfds, sizeof(fd_set));
    }

    return retfds;
}
