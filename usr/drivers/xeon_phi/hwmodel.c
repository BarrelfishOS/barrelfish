/**
 * \file
 * \brief Boot module for the Xeon Phi
 *
 * Loads the co processor OS onto the card and boots it
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <skb/skb.h>

#include <flounder/flounder_txqueue.h>

#include <if/interphi_defs.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_domain.h>

#include "xeon_phi_internal.h"
#include "interphi.h"
#include "smpt.h"
#include "domain.h"
#include "service.h"
#include "xphi_service.h"
#include "sysmem_caps.h"


#include <driverkit/driverkit.h>
#include <driverkit/iommu.h>
#include <driverkit/hwmodel.h>
#include "xeon_phi_internal.h"

#define ALIAS_CONF_Q "state_get(S)," \
                     "alias_conf_wrap(S, "  \
                     "%"PRIi32", %"PRIu64", %zu," \
                     "%"PRIi32", NewS)," \
                     "state_set(NewS)."

/*
 * the translate address is called when a region is registered
 */
errval_t xeon_phi_hw_model_query_and_config(void *arg,
                                            struct capref mem,
                                            genpaddr_t *retaddr)
{
    errval_t err;
    struct xeon_phi *xphi = arg;

    /* query the model */
    #ifdef __k1om__
    return LIB_ERR_NOT_IMPLEMENTED;
    #endif

    struct frame_identity id;
    err = invoke_frame_identify(mem, &id);
    if (err_is_fail(err)) {
        return err;
    }

    err = skb_client_connect();
    if (err_is_fail(err)) {
        return err;
    }


    // TODO: replace with id.pasid; 
    int32_t mem_nodeid = driverkit_hwmodel_lookup_dram_node_id();
    uint64_t addr = id.base;
    size_t size = id.bytes;
    err = skb_execute_query(ALIAS_CONF_Q, mem_nodeid, addr, size, xphi->nodeid);
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "alias_conf \n");
        return err;
    }


    struct hwmodel_name names[1];
    int conversions;
    driverkit_parse_namelist(skb_get_output(), names, &conversions);
    assert(conversions == 1);
    assert(retaddr != NULL);

    *retaddr = names[0].address;
    debug_printf("[knc] Translated address into Xeon Phi space: 0x%"PRIx64"\n",
            *retaddr);

    /* TODO: print & make use of configuration */

    /*resolve(id, addr, xeonphiid)  -> [addr,
                                      iommucfg(in,out),
                                      smptconfig(in,out),
                                      retaddr]; */




    /* map the frame in the iommu space */

#if 0

    if (id.base >= phi->apt.pbase &&
            ((id.base + id.bytes) <= (phi->apt.pbase + phi->apt.bytes))) {

        debug_printf("%s:%u = %lx -> %lx\n", __FUNCTION__, __LINE__,
                     id.base, id.base - phi->apt.pbase);

        *retaddr = id.base - phi->apt.pbase;

    } else {

        struct dmem dmem;
        err = driverkit_iommu_vspace_map_cl(phi->iommu_client, mem,
                                            VREGION_FLAGS_READ_WRITE, &dmem);
        if (err_is_fail(err)) {
            return err;
        }

        debug_printf("%s:%u with IOMMU %" PRIxGENPADDR " -> %" PRIxGENPADDR "\n", __FUNCTION__, __LINE__,
                     id.base, dmem.devaddr + XEON_PHI_SYSMEM_BASE -  2 * (512UL << 30));

        *retaddr = dmem.devaddr + XEON_PHI_SYSMEM_BASE -  2 * (512UL << 30);
    }
#endif

    /* xxx */

    return SYS_ERR_OK;

}
