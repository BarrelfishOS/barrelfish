/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_AARCH64_BARRELFISH_CORESTATE_H
#define ARCH_AARCH64_BARRELFISH_CORESTATE_H

#include <barrelfish/core_state.h>

struct vspace_state {
    struct vspace vspace;
    struct pmap_aarch64 pmap;
};

struct pinned_state {
    struct thread_mutex mutex;
    struct memobj_pinned memobj;
    struct vregion vregion;
    lvaddr_t offset;
    struct slab_allocator vregion_list_slab;
    struct slab_allocator frame_list_slab;
    size_t refill_count;
};

struct core_state_arch {
    struct core_state_generic c;
    struct vspace_state vspace_state;
    struct pinned_state pinned_state;
};

#endif
