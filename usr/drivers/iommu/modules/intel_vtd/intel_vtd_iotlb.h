/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INTEL_VTD_IOTLB_H_
#define INTEL_VTD_IOTLB_H_ 1


#include "intel_vtd.h"
#include "intel_vtd_debug.h"

#define IOTLB_INVALIDATE_TIMEOUT 0x1000


static inline void vtd_iotlb_do_invalidate(vtd_t *iotlb)
{
    // perform the invalidate
    vtd_IOTLB_ivt_wrf(iotlb, 1);

    size_t timeout = IOTLB_INVALIDATE_TIMEOUT;
    volatile uint8_t pending;
    do {
        pending = vtd_IOTLB_ivt_rdf(iotlb);
    } while(pending && timeout--);

    // should have been cleared by now
    assert(!vtd_IOTLB_ivt_rdf(iotlb));
}


static inline void vtd_iotlb_invalidate(struct vtd *vtd)
{
    INTEL_VTD_DEBUG_IOTLB("invalidate global\n");

    // set global invalidation
    vtd_IOTLB_iirg_wrf(&vtd->vtd_dev, vtd_gir);

    // drain oustanding write and read requests
    vtd_IOTLB_dw_wrf(&vtd->vtd_dev, 1);
    vtd_IOTLB_dr_wrf(&vtd->vtd_dev, 1);

    // perform the invalidate
    vtd_iotlb_do_invalidate(&vtd->vtd_dev);
}


static inline void vtd_iotlb_invalidate_domain(struct vtd *vtd, vtd_domid_t domid)
{
    INTEL_VTD_DEBUG_IOTLB("invalidate domain %u\n", domid);

    // set domain invalidation
    vtd_IOTLB_iirg_wrf(&vtd->vtd_dev, vtd_domir);

    // set the domain id field
    vtd_IOTLB_did_wrf(&vtd->vtd_dev, domid);

    // drain oustanding write and read requests
    vtd_IOTLB_dw_wrf(&vtd->vtd_dev, 1);
    vtd_IOTLB_dr_wrf(&vtd->vtd_dev, 1);

    // perform the invalidate
    vtd_iotlb_do_invalidate(&vtd->vtd_dev);
}


static inline void vtd_iotlb_invalidate_page_attr(struct vtd_domain *dom,
                                                  genvaddr_t addr, uint8_t mask,
                                                  bool hint)
{
    addr = addr & ~((1UL << mask) - 1);

    INTEL_VTD_DEBUG_IOTLB("invalidate address domain=%u, [%lx..%lx]\n",
                           dom->id, addr, addr + (1 << mask) - 1 );

    struct vtd_device *dev = dom->devices;
    while(dev) {
        struct vtd *vtd = (struct vtd *)dev->dev.iommu;

        // set domain invalidation
         vtd_IOTLB_iirg_wrf(&vtd->vtd_dev, vtd_domir);

        // set the domain id field
        vtd_IOTLB_did_wrf(&vtd->vtd_dev, dom->id);

        // drain oustanding write and read requests
        vtd_IOTLB_dw_wrf(&vtd->vtd_dev, 1);
        vtd_IOTLB_dr_wrf(&vtd->vtd_dev, 1);

        // write the address hint
        vtd_IVA_ih_wrf(&vtd->vtd_dev, hint);

        // set the address
        vtd_IVA_addr_wrf(&vtd->vtd_dev, (addr >> 12));

        // set the address mask
        vtd_IVA_am_wrf(&vtd->vtd_dev, mask);

        // perform the invalidate
        vtd_iotlb_do_invalidate(&vtd->vtd_dev);

        dev = dev->domain_next;
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
