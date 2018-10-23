/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_PMAP_PRIV_H
#define LIBBARRELFISH_PMAP_PRIV_H

/**
 * \brief internal mapping function. This assumes that enough slabs are
 * available for metadata.
 */
errval_t do_map(struct pmap *pmap, genvaddr_t vaddr,
                struct capref frame, size_t offset, size_t size,
                vregion_flags_t flags, size_t *retoff, size_t *retsize);

/**
 * \brief return the number of slabs required for mapping a region of size
 * `bytes` with small pages.
 */
size_t max_slabs_required(size_t bytes);

errval_t pmap_vnode_mgmt_current_init(struct pmap *pmap);


#endif // LIBBARRELFISH_PMAP_PRIV_H
