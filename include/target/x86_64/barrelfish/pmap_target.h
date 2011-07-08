/**
 * \file
 * \brief Arch specific declerations that can be included by others
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_X86_64_BARRELFISH_PMAP_H
#define TARGET_X86_64_BARRELFISH_PMAP_H

#include <target/x86/barrelfish/pmap_target.h>

errval_t pmap_x86_64_init(struct pmap *pmap, struct vspace *vspace,
                          struct capref vnode,
                          struct slot_allocator *opt_slot_alloc);
errval_t pmap_x86_64_current_init(bool);

#endif // TARGET_X86_64_BARRELFISH_PMAP_H
