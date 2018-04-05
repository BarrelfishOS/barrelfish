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
#include <driverkit/hwmodel.h>
#include <collections/hash_table.h>

#include "debug.h"


errval_t driverkit_hwmodel_frame_alloc(struct capref *dst,
                                                     size_t bytes, int32_t dstnode,
                                                     int32_t *nodes)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}


errval_t driverkit_hwmodel_vspace_map(int32_t nodeid, struct capref frame,
                                                    vregion_flags_t flags, struct dmem *dmem)
{
    return LIB_ERR_NOT_IMPLEMENTED;
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
    return -1;
}

int32_t driverkit_hwmodel_lookup_node_id(const char *path)
{
    return -1;
}

