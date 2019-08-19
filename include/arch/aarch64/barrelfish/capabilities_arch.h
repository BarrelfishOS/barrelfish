/**
 * \file
 * \brief Base capability/cnode handling functions.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INCLUDEBARRELFISH_CAPABILITIES_ARCH_H
#define INCLUDEBARRELFISH_CAPABILITIES_ARCH_H

#include <stdint.h>
#include <sys/cdefs.h>

#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/capabilities.h>
#include <barrelfish_kpi/dispatcher_shared.h>
#include <barrelfish_kpi/distcaps.h>
#include <barrelfish/invocations.h>
#include <barrelfish/slot_alloc.h>

__BEGIN_DECLS

/*
 * MVAS extension: arch specific flags to vnode_inherit.
 */
static inline errval_t vnode_inherit_attr(struct capref dest, struct capref src,
                                          cslot_t start, cslot_t end,
                                          paging_x86_64_flags_t newflags,
                                          struct capref *src_mapping_cn,
                                          struct capref *dst_mapping_cn)
{
    enum cnode_type slevel = get_cap_level(src);
    capaddr_t saddr = get_cap_addr(src);

    capaddr_t mcn[2*MCN_COUNT];
    for (size_t i = 0; i < MCN_COUNT; i++) {
        mcn[i] = get_cap_addr(src_mapping_cn[i]);
        assert(get_cap_level(src_mapping_cn[i]) == CNODE_TYPE_OTHER);
    }
    for (size_t i = 0; i < MCN_COUNT; i++) {
        mcn[MCN_COUNT+i] = get_cap_addr(dst_mapping_cn[i]);
        assert(get_cap_level(dst_mapping_cn[i]) == CNODE_TYPE_OTHER);
    }

    return invoke_vnode_inherit(dest, saddr, slevel, start, end, newflags, mcn);
}

__END_DECLS

#endif //INCLUDEBARRELFISH_CAPABILITIES_ARCH_H
