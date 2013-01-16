/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <fcntl.h>
#include <assert.h>
#include <stdarg.h>
#include <errno.h>
#include <vfs/fdtab.h>
#include "posixcompat.h"
#include "unixsock.h"

int fcntl(int fd, int cmd, ...)
{
    int retval = 0;
    va_list arg;

    va_start(arg, cmd);

    struct fdtab_entry *e = fdtab_get(fd);
    if (e->type == FDTAB_TYPE_AVAILABLE) {
        va_end(arg);
        return -1;
    }

    switch(cmd) {
    case F_DUPFD:
        {
            int from = va_arg(arg, int);
            retval = fdtab_alloc_from(e, from);
            break;
        }

    case F_SETFD:
        {
            int flags = va_arg(arg, int);

            POSIXCOMPAT_DEBUG("fcntl(%d, F_SETFD, %d)\n", fd, flags);

            if(flags & FD_CLOEXEC) {
                // XXX: close-on-exec not supported, since exec() not supported
            } else {
            }
            break;
        }

    case F_SETFL:
        {
            int flags = va_arg(arg, int);

            POSIXCOMPAT_DEBUG("fcntl(%d, F_SETFL, %d)\n", fd, flags);

            switch(e->type) {
            case FDTAB_TYPE_UNIX_SOCKET:
                {
                    struct _unix_socket *us = e->handle;
                    us->nonblocking = flags & O_NONBLOCK ? true : false;
                }
                break;

            default:
                return -1;
            }

            break;
        }

    case F_GETFL:
        {
            POSIXCOMPAT_DEBUG("fcntl(%d, F_GETFL)\n", fd);

            switch(e->type) {
            case FDTAB_TYPE_FILE:
            case FDTAB_TYPE_LWIP_SOCKET:
                {
                    // no flags set
                    return 0;
                }
                break;

            default:
                assert(!"NYI");
                return -1;
                break;
            }
        }

    default:
        assert(!"NYI");
        retval = -1;
        break;
    }

    va_end(arg);
    return retval;
}
