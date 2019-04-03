/*
 * Copyright (c) 2011, 2012, 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef POSIXCOMPAT_H
#define POSIXCOMPAT_H

#include <barrelfish/caddr.h>
#include <barrelfish/types.h>
#include <errors/errno.h>

#include <sys/cdefs.h>

__BEGIN_DECLS

struct memobj_anon* sbrk_get_memobj(void);
struct vregion* sbrk_get_vregion(void);
void* sbrk_get_base(void);
size_t sbrk_get_offset(void);

errval_t spawn_setup_fds(struct capref *frame, int rfd);
errval_t posixcompat_unpack_fds(void);
iref_t posixcompat_pts_get_iref(int fd);

enum pthread_action {
    PTHREAD_ACTION_CREATE,
    PTHREAD_ACTION_DESTROY,
};
typedef int (*pthread_placement_fn)(enum pthread_action action, int coreid);
errval_t posixcompat_pthread_set_placement_fn(pthread_placement_fn fn);

// To propagate BF errors through POSIX layer, like errno.
errval_t posixcompat_get_bf_error(void);

__END_DECLS

#endif
