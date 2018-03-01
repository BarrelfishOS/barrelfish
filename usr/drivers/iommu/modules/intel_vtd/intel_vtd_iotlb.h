/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INTEL_VTD_IOTLB_H_
#define INTEL_VTD_IOTLB_H_ 1


#include <dev/vtd_iotlb_dev.h>

#include "intel_vtd.h"
#include "intel_vtd_debug.h"

#define IOTLB_INVALIDATE_TIMEOUT 0x1000

static inline void vtd_flush_write_buffer(struct vtd *vtd)
{
    return;
}

static inline void vtd_iotlb_do_invalidate(vtd_iotlb_t *iotlb)
{
    // perform the invalidate
    vtd_iotlb_iotlb_reg_ivt_wrf(iotlb, 1);

    size_t timeout = IOTLB_INVALIDATE_TIMEOUT;
    volatile uint8_t pending;
    do {
        pending = vtd_iotlb_iotlb_reg_ivt_rdf(iotlb);
    } while(pending && timeout--);

    // should have been cleared by now
    assert(vtd_iotlb_iotlb_reg_ivt_rdf(iotlb));
}


static inline void vtd_iotlb_invalidate(struct vtd *vtd)
{
    INTEL_VTD_DEBUG_IOTLB("invalidate global\n");

    // Flush the Root-Complex internal write buffers
    vtd_flush_write_buffer(vtd);

    // set global invalidation
    vtd_iotlb_iotlb_reg_iirg_wrf(&vtd->registers.iotlb, vtd_iotlb_gir);

    // drain oustanding write and read requests
    vtd_iotlb_iotlb_reg_dw_wrf(&vtd->registers.iotlb, 1);
    vtd_iotlb_iotlb_reg_dr_wrf(&vtd->registers.iotlb, 1);

    // perform the invalidate
    vtd_iotlb_do_invalidate(&vtd->registers.iotlb);
}


static inline void vtd_iotlb_invalidate_domain(struct vtd_domain *dom)
{
    INTEL_VTD_DEBUG_IOTLB("invalidate domain %u\n", dom->id);

    struct vtd_domain_mapping *dm = dom->devmappings;
    while(dm) {
        vtd_flush_write_buffer(dm->vtd);

        vtd_iotlb_t* iotlb = &dm->vtd->registers.iotlb;

        // set domain invalidation
        vtd_iotlb_iotlb_reg_iirg_wrf(iotlb, vtd_iotlb_domir);

        // set the domain id field
        vtd_iotlb_iotlb_reg_did_wrf(iotlb, dom->id);

        // drain oustanding write and read requests
        vtd_iotlb_iotlb_reg_dw_wrf(iotlb, 1);
        vtd_iotlb_iotlb_reg_dr_wrf(iotlb, 1);

        // perform the invalidate
        vtd_iotlb_do_invalidate(iotlb);

        dm = dm->next;
    }
}


static inline void vtd_iotlb_invalidate_page_attr(struct vtd_domain *dom,
                                                  genvaddr_t addr, uint8_t mask,
                                                  bool hint)
{
    addr = addr & ~((1UL << mask) - 1);

    INTEL_VTD_DEBUG_IOTLB("invalidate address domain=%u, [%lx..%lx]\n",
                           dom->id, addr, addr + (1 << mask) - 1 );

    struct vtd_domain_mapping *dm = dom->devmappings;
    while(dm) {
        vtd_flush_write_buffer(dm->vtd);

        vtd_iotlb_t* iotlb = &dm->vtd->registers.iotlb;
        
        // set domain invalidation
        vtd_iotlb_iotlb_reg_iirg_wrf(iotlb, vtd_iotlb_pir);

        // set the domain id field
        vtd_iotlb_iotlb_reg_did_wrf(iotlb, dom->id);

        // drain oustanding write and read requests
        vtd_iotlb_iotlb_reg_dw_wrf(iotlb, 1);
        vtd_iotlb_iotlb_reg_dr_wrf(iotlb, 1);

        // write the address hint
        vtd_iotlb_iva_reg_ih_wrf(iotlb, hint);

        // set the address
        vtd_iotlb_iva_reg_addr_wrf(iotlb, (addr >> 12));

        // set the address mask
        vtd_iotlb_iva_reg_am_wrf(iotlb, mask);

        // perform the invalidate
        vtd_iotlb_do_invalidate(iotlb);

        dm = dm->next;
    }
}


static inline void vtd_iotlb_invalidate_page(struct vtd_domain *dom,
                                             genvaddr_t addr)
{
    vtd_iotlb_invalidate_page_attr(dom, addr, 21, false);
}


static inline void vtd_iotlb_enable(struct vtd *vtd)
{

}

static inline void vtd_iotlb_disable(struct vtd *vtd)
{

}

#endif /// INTEL_VTD_IOTLB_H_