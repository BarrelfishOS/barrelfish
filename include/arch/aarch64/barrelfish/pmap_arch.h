/**
 * \file
 * \brief pmap management wrappers
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_AARCH64_BARRELFISH_PMAP_H
#define ARCH_AARCH64_BARRELFISH_PMAP_H

#include <target/aarch64/barrelfish/pmap_target.h>

#define ARCH_DEFAULT_PMAP_SIZE sizeof(struct pmap_aarch64)

errval_t pmap_init(struct pmap *p, struct vspace *v, struct capref vnode,
                   struct slot_allocator *opt_slot_alloc);
errval_t pmap_current_init(bool);

#endif // ARCH_AARCH64_BARRELFISH_PMAP_H
