/*
 * Copyright (c) 2007, 2008, 2009, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <sys/socket.h>
#include <barrelfish/barrelfish.h>

int pipe(int pipefd[2])
{
    // XXX: Emulate pipe via a pair of AF_UNIX sockets
    // This means they are bi-directional, but this is fine, as pipe()
    // doesn't specify bi-directionality.
    return socketpair(AF_UNIX, SOCK_STREAM, 0, pipefd);
}
