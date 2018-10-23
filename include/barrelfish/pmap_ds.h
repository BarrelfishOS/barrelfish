/**
 * \file
 * \brief public declarations for pmap datastructure stuff.
 */

/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_PMAP_DS_H
#define BARRELFISH_PMAP_DS_H

#if defined(PMAP_LL)
typedef struct vnode pmap_ds_child_t;
#elif defined(PMAP_ARRAY)
typedef struct vnode* pmap_ds_child_t;
#else
#error Unknown Pmap datastructure.
#endif

#endif // BARRELFISH_PMAP_DS_H
