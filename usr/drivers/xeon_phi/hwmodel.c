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

static errval_t lookup_phi_enums(int32_t nodeid, int32_t *iommu, int32_t *smpt)
{
    errval_t err;
    err = skb_execute_query(
        "state_get(S), "
        "xeon_phi_meta(S, %"PRIi32", _, _, SMPTID, IOMMUID),"
        "node_enum(S, SMPTID, E1, S1),"
        "node_enum(S1, IOMMUID, E2, NewS),"
        "write(E1), write(' '), write(E2), "
        "state_set(NewS).",
        nodeid);
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "query smpt");
        return err;
    }
    err = skb_read_output("%d %d", smpt, iommu);
    return err;
}

// Can install SMPT and IOMMU configs
static errval_t install_config(struct xeon_phi *xphi, char * conf, struct dmem *dmem){
    errval_t err;
    assert(!(skb_get_output() <= conf && conf <= skb_get_output() + 512));

    // Get configurable nodeids
    int32_t smpt_id, iommu_id;
    err = lookup_phi_enums(xphi->nodeid, &iommu_id, &smpt_id);
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
                                            genpaddr_t *retaddr,
                                            genvaddr_t *local_retaddr)
{
    errval_t err;
    struct xeon_phi *xphi = arg;
    assert(arg != NULL);
    assert(retaddr != NULL);

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

    debug_printf("%s:%d: translate request addr=0x%"PRIx64", size=%"PRIuGENSIZE"\n",
            __FUNCTION__, __LINE__, id.base, size);



    debug_printf(ALIAS_CONF_Q, mem_nodeid, addr, size, xphi->nodeid);
    err = skb_execute_query(ALIAS_CONF_Q, mem_nodeid, addr, size, xphi->nodeid);
    printf("SKB STD OUT: %s\n\n", skb_get_output());
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "alias_conf \n");
        return err;
    }

    // Determine conf line (second output line)
    char * confline = strstr(skb_get_output(), "\n");
    assert(confline);
    *confline = 0;
    confline++;
    confline = strdup(confline);

    printf("First line: %s, Next Line: %s\n", skb_get_output(), confline);

    struct hwmodel_name names[1];
    int conversions;
    driverkit_parse_namelist(skb_get_output(), names, &conversions);
    debug_printf("Conversions = %d\n", conversions);
    assert(conversions == 1);

    *retaddr = names[0].address;
    debug_printf("[knc] Translated address into Xeon Phi space: 0x%"PRIx64"\n",
            *retaddr);

           
    struct dmem dmem;
    dmem.size = id.bytes;
    dmem.mem = mem;
    err = install_config(xphi, confline, &dmem);
    assert(err_is_ok(err));

    /* TODO: only do this if IOMMU is present or needs to be changed */

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "TODO: CLEANUP!");
    }

    if(local_retaddr) {
        *local_retaddr = dmem.vbase;
    }

    free(confline);

    return SYS_ERR_OK;

}

//errval_t xeon_phi_hw_model_query_and_config(void *arg,
//                                            struct capref mem,
//                                            genpaddr_t *retaddr)
//{
//}
