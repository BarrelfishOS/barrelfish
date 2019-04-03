/**
 * \file
 * \brief pmap datastructure header for linked list pmap. This file is
 * included by selecting the right include dir in lib/barrelfish/Hakefile
 */

/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBF_INCLUDE_PMAP_DS_H
#define LIBBF_INCLUDE_PMAP_DS_H

/**
 * \brief a macro that provides a datastructure-independent way of iterating
 * through the non-null children of the vnode `root`.
 *
 * Note: this macro requires both root and iter to be 'struct vnode *'.
 */
#define pmap_foreach_child(root, iter) \
    for (iter = (root)->v.u.vnode.children; iter; iter = iter->v.meta.next)

#endif // LIBBF_INCLUDE_PMAP_DS_H
