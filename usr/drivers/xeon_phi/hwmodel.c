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


errval_t xeon_phi_hw_model_lookup_nodeids(int32_t pci_nodeid, int32_t *knc_socket,
                                          int32_t *smpt, int32_t *iommu,
                                          int32_t *dma, int32_t *k1om_core,
                                          int32_t *gddr_node)
{
    static int enums[6];
    static int skb_read = 0;
    errval_t err;

    if(!skb_read){
        skb_execute_query("decoding_net_listing");

        HWMODEL_QUERY_DEBUG("state_get(S),xeon_phi_meta_wrap(S, %"PRIi32").",
            pci_nodeid);
        err = skb_execute_query("state_get(S),xeon_phi_meta_wrap(S, %"PRIi32").",
            pci_nodeid);
        if (err_is_fail(err)) {
            DEBUG_SKB_ERR(err, "query xeon phi meta. Retrying....");
            skb_execute_query("decoding_net_listing");

            HWMODEL_QUERY_DEBUG("state_get(S),xeon_phi_meta_wrap(S, %"PRIi32").",
                pci_nodeid);
            err = skb_execute_query("state_get(S),xeon_phi_meta_wrap(S, %"PRIi32").",
                pci_nodeid);
            if (err_is_fail(err)) {
                DEBUG_SKB_ERR(err, "query xeon phi meta");
            }
            return err;
        }
        err = skb_read_output("%d %d %d %d %d %d",
                &enums[0], &enums[1], &enums[2], &enums[3], &enums[4], &enums[5]);
        if(err_is_fail(err)) return err;
        skb_read = true;
        debug_printf("PHI NodeIds: KNC_SOCKET=%d, SMPT=%d, IOMMU=%d, DMA=%d,"
                " K1OM_CORE=%d\n",
                enums[0], enums[1], enums[2], enums[3], enums[4]);
    };

    if(knc_socket) *knc_socket = enums[0];
    if(smpt) *smpt = enums[1];
    if(iommu) *iommu = enums[2];
    if(dma) *dma = enums[3];
    if(k1om_core) *k1om_core = enums[4];
    if(gddr_node) *gddr_node = enums[5];

    return SYS_ERR_OK;
}

// Can install SMPT and IOMMU configs
static errval_t install_config(struct xeon_phi *xphi, char * conf, struct dmem *dmem){
    errval_t err;
    assert(!(skb_get_output() <= conf && conf <= skb_get_output() + 512));

    // Get configurable nodeids
    int32_t smpt_id, iommu_id;
    err = xeon_phi_hw_model_lookup_nodeids(xphi->nodeid, NULL, &smpt_id, &iommu_id, NULL, NULL, NULL);
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
    bool iommu_needs_conf = false;
    while(skb_read_list(&status, "c(%"SCNi32", %"SCNu64", %"SCNu64")",
                &nodeid, &inaddr, &outaddr)) {
        debug_printf("nodeid=%"PRIi32"\n", nodeid);
        if(nodeid == smpt_id){
            debug_printf("CONF SMPT: in=0x%"PRIx64", out=0x%"PRIx64"\n", inaddr, outaddr);
            smpt_set_address(xphi, inaddr / SIZE16G, outaddr, 1);
        } else if(nodeid == iommu_id){
            debug_printf("CONF IOMMU: in=0x%"PRIx64", out=0x%"PRIx64"\n", inaddr, outaddr);
            if(inaddr < dmem->devaddr){
                dmem->devaddr = inaddr;
                iommu_needs_conf = true;
            }
        } else {
            debug_printf("%s:%d: Don't know how to config node %d. Ignoring.\n",
                    __FUNCTION__, __LINE__, nodeid);
        }
    }

    dmem->vbase = 0;
    if(iommu_needs_conf){
        // Fills in dmem->vbase
        err = driverkit_iommu_vspace_map_fixed_cl(xphi->iommu_client, dmem->mem,
                                                  VREGION_FLAGS_READ_WRITE, dmem);
        if(err_is_fail(err)){
            DEBUG_ERR(err, "map_fixed");
        }
        return err;
    } else {
        return SYS_ERR_OK;
    }
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
    err = frame_identify(mem, &id);
    if (err_is_fail(err)) {
        return err;
    }

    /* query the model */
    #ifdef __k1om__
    return LIB_ERR_NOT_IMPLEMENTED;
    #endif

    char conf[1024];
    int32_t knc_sock_id;
    err = xeon_phi_hw_model_lookup_nodeids(xphi->nodeid, &knc_sock_id, NULL, NULL, NULL, NULL, NULL);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "lookup nodeids");
        return err;
    }

    err = driverkit_hwmodel_get_map_conf(mem, knc_sock_id, conf, sizeof(conf), retaddr);
    assert(err_is_ok(err));

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
        // This is a bit unfortunate. We only generate dmem.vbase
        // if we setup the IOMMU. But since most caller dont care this works.
        assert(dmem.vbase);
        *local_retaddr = dmem.vbase;
    }

    return SYS_ERR_OK;
}
