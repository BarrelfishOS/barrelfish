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

#define SIZE16G (16ll*1024*1024*1024)

static errval_t lookup_phi_enums(int32_t pci_nodeid,
        int32_t *knc_socket,
        int32_t *smpt,
        int32_t *iommu
        )
{
    errval_t err;

    skb_execute_query("decoding_net_listing");

    err = skb_execute_query(
        "state_get(S), "
        "xeon_phi_meta(S, %"PRIi32", E1, E2, E3),"
        "write(E1), write(' '),"
        "write(E2), write(' '),"
        "write(E3).",
        pci_nodeid);
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "query xeon phi meta");
        return err;
    }

    int enums[3];
    err = skb_read_output("%d %d %d", &enums[0], &enums[1], &enums[2]);
    if(err_is_fail(err)) return err;

    if(knc_socket) *knc_socket = enums[0];
    if(smpt) *smpt = enums[1];
    if(iommu) *iommu = enums[2];

    return SYS_ERR_OK;
}

// Can install SMPT and IOMMU configs
static errval_t install_config(struct xeon_phi *xphi, char * conf, struct dmem *dmem){
    errval_t err;
    assert(!(skb_get_output() <= conf && conf <= skb_get_output() + 512));

    // Get configurable nodeids
    int32_t smpt_id, iommu_id;
    err = lookup_phi_enums(xphi->nodeid, NULL, &smpt_id, &iommu_id);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "lookup nodeids");
        return err;
    }

    debug_printf("xphi nodeid: %d, iommu nodeid: %d, smpt nodeid: %d\n",
            xphi->nodeid, iommu_id, smpt_id);

    struct list_parser_status status;
    skb_read_list_init_offset(&status, conf, 0);
    uint64_t inaddr, outaddr;
    int32_t nodeid;
    dmem->devaddr = 0xfffffffffffffff;
    while(skb_read_list(&status, "c(%"SCNi32", %"SCNu64", %"SCNu64")",
                &nodeid, &inaddr, &outaddr)) {
        debug_printf("nodeid=%"PRIi32"\n", nodeid);
        if(nodeid == smpt_id){
            debug_printf("CONF SMPT: in=0x%"PRIx64", out=0x%"PRIx64"\n", inaddr, outaddr);
            smpt_set_address(xphi, inaddr / SIZE16G, outaddr, 1);
        } else if(nodeid == iommu_id){
            debug_printf("CONF IOMMU: in=0x%"PRIx64", out=0x%"PRIx64"\n", inaddr, outaddr);
            if(inaddr < dmem->devaddr){
                dmem->vbase = inaddr;
                dmem->devaddr = inaddr;
            }
        } else {
            debug_printf("%s:%d: Don't know how to config node %d. Ignoring.\n",
                    __FUNCTION__, __LINE__, nodeid);
        }
    }

    dmem->vbase = 0;
    err = driverkit_iommu_vspace_map_fixed_cl(xphi->iommu_client, dmem->mem,
                                              VREGION_FLAGS_READ_WRITE, dmem);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "map_fixed");
    }
    return err;
}

/*
 * the translate address is called when a region is registered
 */
errval_t xeon_phi_hw_model_query_and_config(void *arg,
                                            struct capref mem,
                                            genpaddr_t *retaddr,
                                            genvaddr_t *local_retaddr)
{
    errval_t err;
    struct xeon_phi *xphi = arg;
    assert(arg != NULL);
    assert(retaddr != NULL);

    struct frame_identity id;
    err = invoke_frame_identify(mem, &id);
    if (err_is_fail(err)) {
        return err;
    }

    /* query the model */
    #ifdef __k1om__
    return LIB_ERR_NOT_IMPLEMENTED;
    #endif

    char conf[1024];
    int32_t knc_sock_id;
    err = lookup_phi_enums(xphi->nodeid, &knc_sock_id, NULL, NULL);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "lookup nodeids");
        return err;
    }

    err = driverkit_hwmodel_alias_conf(mem, knc_sock_id, conf, sizeof(conf), retaddr);

    skb_execute_query("decoding_net_listing");

    debug_printf("[knc] Translated address into Xeon Phi space: 0x%"PRIx64"\n",
            *retaddr);

           
    struct dmem dmem;
    dmem.size = id.bytes;
    dmem.mem = mem;
    err = install_config(xphi, conf, &dmem);
    assert(err_is_ok(err));

    /* TODO: only do this if IOMMU is present or needs to be changed */

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "TODO: CLEANUP!");
    }

    if(local_retaddr) {
        *local_retaddr = dmem.vbase;
    }

    return SYS_ERR_OK;
}
