/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INTEL_VTD_CTXT_CACHE_H_
#define INTEL_VTD_CTXT_CACHE_H_ 1


#include "intel_vtd.h"
#include "intel_vtd_debug.h"
#include "intel_vtd_iotlb.h"

#define CTXT_CACHE_INVALIDATE_TIMEOUT 0x1000

static inline void vtd_ctxt_cache_do_invalidate(vtd_t *vtd, vtd_CCMD_t reg)
{
    // perform the invalidate
    reg = vtd_CCMD_icc_insert(reg, 1);
    vtd_CCMD_wr(vtd, reg);

    size_t timeout = CTXT_CACHE_INVALIDATE_TIMEOUT;
    volatile uint8_t pending;
    do {
        pending = vtd_CCMD_icc_rdf(vtd);
    } while(pending && timeout--);

    // should have been cleared by now
    assert(!vtd_CCMD_icc_rdf(vtd));
}


static inline void vtd_ctxt_cache_invalidate(struct vtd *vtd)
{
    INTEL_VTD_DEBUG_CTXTCACHE("invalidate global\n");

    vtd_CCMD_t reg = vtd_CCMD_default;
    reg = vtd_CCMD_cirg_insert(reg, vtd_gir);

    vtd_ctxt_cache_do_invalidate(&vtd->vtd_dev, reg);

    vtd_iotlb_invalidate(vtd);
}


static inline void vtd_ctxt_cache_invalidate_domain(struct vtd *vtd,
                                                    vtd_domid_t domid)
{
    INTEL_VTD_DEBUG_CTXTCACHE("invalidate domain %u\n", domid);

    vtd_CCMD_t reg = vtd_CCMD_default;
    reg = vtd_CCMD_cirg_insert(reg, vtd_gir);
    reg = vtd_CCMD_did_insert(reg, domid);

    vtd_ctxt_cache_do_invalidate(&vtd->vtd_dev, reg);

    vtd_iotlb_invalidate_domain(vtd, domid);
}


static inline void vtd_ctxt_cache_invalidate_device(struct vtd *vtd, uint8_t bus,
                                                    uint8_t dev, uint8_t fun,
                                                    vtd_domid_t domid)
{
    INTEL_VTD_DEBUG_CTXTCACHE("invalidate device %u\n", domid);

    vtd_CCMD_t reg = vtd_CCMD_default;
    reg = vtd_CCMD_cirg_insert(reg, vtd_gir);
    reg = vtd_CCMD_did_insert(reg, domid);

    uint16_t sid = ((uint16_t)bus) << 8 | ((0x3f & dev) << 3) | (0x7 & fun);
    reg = vtd_CCMD_sid_insert(reg, sid);
    reg = vtd_CCMD_fm_insert(reg, vtd_nomask);

    vtd_ctxt_cache_do_invalidate(&vtd->vtd_dev, reg);

    vtd_iotlb_invalidate_domain(vtd, domid);
}


#endif /// INTEL_VTD_CTXT_CACHE_H_
