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
#include "xeon_phi_internal.h"


/*
 * the translate address is called when a region is registered
 */
errval_t xeon_phi_hw_model_query_and_config(void *arg,
                                            struct capref mem,
                                            genpaddr_t *retaddr)
{
    errval_t err;

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

    #if 0

    // asid = id.pasid
    // addr = id.base
    // size = id.bytes
    err = skb_execute_query("TODO.");
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to query the skb\n");
        return err;
    }

    struct list_parser_status status;
    skb_read_list_init(&status);

    uint64_t tmp;
    while(skb_read_list(&status, "TODO(%" SCNd64")", &tmp)) {

        /* TODO: handle the constructs */

    }


    resolve(id, addr, xeonphiid)  -> [addr,
                                      iommucfg(in,out),
                                      smptconfig(in,out),
                                      retaddr];



    #endif

    struct xeon_phi *phi = arg;



    /* map the frame in the iommu space */


    // set the
    if (id.base + id.bytes < phi->apt.pbase) {
        struct dmem dmem;
        err = driverkit_iommu_vspace_map_cl(phi->iommu_client, mem, VREGION_FLAGS_READ_WRITE, &dmem);
        if (err_is_fail(err)) {
            return err;
        }

        *retaddr = dmem.devaddr + XEON_PHI_SYSMEM_BASE;
    } else {
        *retaddr = id.base - phi->apt.pbase;
    }

    /* xxx */

    return SYS_ERR_OK;

}