/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <sys/mman.h>
#include <barrelfish/barrelfish.h>
#include "posixcompat.h"

void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset)
{
    POSIXCOMPAT_DEBUG("Warning: mmap(%p, %zx, %d, %d, %d, %zu) ignored\n",
            addr, length, prot, flags, fd, offset);
    USER_PANIC("mmap NYI!");
    return NULL;
}

int munmap(void *addr, size_t length)
{
    POSIXCOMPAT_DEBUG("Warning: munmap(%p, %zx) ignored\n",
            addr, length);
    USER_PANIC("mmap NYI!");
    return -1;
}
