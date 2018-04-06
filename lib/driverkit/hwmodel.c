/**
 * \file
 * \brief Driverkit module implementation.
 *
 * Contians helper functions to iterate over driver modules in a domain
 * and create driver instances from driver modules.
 */
/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>
#include <driverkit/iommu.h>
#include <driverkit/hwmodel.h>
#include <collections/hash_table.h>
#include <skb/skb.h>
#include "debug.h"


#define HWMODE_DEBUG_RAM_NODE 1

errval_t driverkit_hwmodel_frame_alloc(struct capref *dst,
                                                     size_t bytes, int32_t dstnode,
                                                     int32_t *nodes)
{
    if (dstnode != HWMODE_DEBUG_RAM_NODE) {
        return LIB_ERR_RAM_ALLOC_MS_CONSTRAINTS;
    }
    return frame_alloc(dst, bytes, NULL);
}


errval_t driverkit_hwmodel_vspace_map(int32_t nodeid, struct capref frame,
                                                    vregion_flags_t flags, struct dmem *dmem)
{
    errval_t err;

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err;
    }

    dmem->devaddr = id.base;
    dmem->size = id.bytes;

    return vspace_map_one_frame_attr((void **)&dmem->vbase, id.bytes, frame, flags, NULL,
                                     NULL);
}

errval_t driverkit_hwmodel_vspace_map_fixed(int32_t nodeid,
                                                          genvaddr_t addr,
                                                          struct capref frame,
                                                          vregion_flags_t flags,
                                                          struct dmem *dmem)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}


errval_t driverkit_hwmodel_vspace_alloc(int32_t nodeid,
                                                      genvaddr_t *addr)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}

int32_t driverkit_hwmodel_get_my_node_id(void)
{
    errval_t err;

    err = skb_client_connect();
    if (err_is_fail(err)) {
        return -1;
    }
    /*
     * XXX: this assumes the domain only runs on a single core!
     */
    static int32_t nodeid = -1;

    if(nodeid == -1){
        err = skb_execute_query("add_process_alloc(X), write(X)");
        if (err_is_fail(err)) {
            return -1;
        }
        err = skb_read_output("%d", &nodeid);
        assert(err_is_ok(err));
        DRIVERKIT_DEBUG("Instantiated new process model node, nodeid=%"PRIi32"\n",
                        nodeid);
    }
    return nodeid;
}

int32_t driverkit_hwmodel_lookup_node_id(const char *path)
{
    if (strncmp(path, "numanode", sizeof("numanode"))) {
        debug_printf("XXX just return %u for numanode node\n",
                     HWMODE_DEBUG_RAM_NODE);
        return HWMODE_DEBUG_RAM_NODE;
    } else if (strncmp(path, "ram", sizeof("ram"))) {
        debug_printf("XXX just return %u for numanode node\n",
                     HWMODE_DEBUG_RAM_NODE);
        return HWMODE_DEBUG_RAM_NODE;
    }

    return -1;
}

