/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <errno.h>

#include <sys/mman.h>

#include "posixcompat.h"

static vregion_flags_t prot_to_vregion_flags(int flags)
{
    vregion_flags_t f = VREGION_FLAGS_NONE;
    if (flags & PROT_READ) {
        f |= VREGION_FLAGS_READ;
    }
    if (flags & PROT_WRITE) {
        f |= VREGION_FLAGS_WRITE;
    }
    if (flags & PROT_EXEC) {
        f |= VREGION_FLAGS_EXECUTE;
    }
    return f;
}

int mprotect(void *memptr, size_t size, int flags)
{
    errval_t err;

    struct vspace *vs = get_current_vspace();
    struct vregion *v = vspace_get_region(vs, memptr);

    genvaddr_t vaddr = (genvaddr_t)(lvaddr_t)memptr;

    if (vaddr == 0 || vaddr & BASE_PAGE_SIZE) {
        *__error() = EINVAL;
        return -1;
    }

    genvaddr_t vbase = vregion_get_base_addr(v);
    genvaddr_t vend  = vbase + vregion_get_size(v);

    if (vend < vaddr + size) {
        // according to mprotect manpage, mprotect on region that is not fully mapped
        // triggers ENOMEM
        *__error() = ENOMEM;
        return -1;
    }

    struct memobj *m = vregion_get_memobj(v);
    err = m->f.protect(m, v, vaddr-vbase, size, prot_to_vregion_flags(flags));
    if (err_is_fail(err)) {
        posixcompat_set_bf_error(err);
        *__error() = ENOMEM;
        return -1;
    }

    return 0;
}
