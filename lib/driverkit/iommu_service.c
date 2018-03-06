/**
 * \file
 * \brief DriverKit IOMMU Service
 *
 * Contains functions to request mappings from the IOMMU service
 */
/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich.
 * Attn: Systems Group.
 */
#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <driverkit/iommu.h>
#include <skb/skb.h>

#include <if/iommu_defs.h>
#include <if/iommu_rpcclient_defs.h>

#include "debug.h"

static void export_callback(void *st, errval_t err, iref_t iref)
{
    debug_printf("[iommu svc] Service exported: '%s'\n", err_getstring(err));

    if (err_is_fail(err)) {
        return;
    }

    debug_printf("[iommu svc] Registering %" PRIuIREF " with '%s\n",
                 iref, DRIVERKIT_IOMMU_SERVICE_NAME);

    err = nameservice_register(DRIVERKIT_IOMMU_SERVICE_NAME, iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static errval_t connect_callback(void *cst, struct iommu_binding *b)
{
    //b->rx_vtbl = XX;
    b->st = NULL;
    return SYS_ERR_OK;
}

errval_t driverkit_iommu_service_init(void)
{
    debug_printf("[iommu svc] Initializing service\n");

    struct waitset *ws = get_default_waitset();
    return iommu_export(NULL, export_callback, connect_callback, ws,
                        IDC_EXPORT_FLAGS_DEFAULT);
}