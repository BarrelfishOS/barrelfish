/**
 * \file
 * \brief DriverKit IOMMU Client
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
#include <driverkit/driverkit.h>
#include <driverkit/control.h>
#include <skb/skb.h>

#include <if/dcontrol_defs.h>
#include <if/dcontrol_rpcclient_defs.h>

#include "debug.h"

errval_t driverkit_get_driver_ep_cap(struct driver_instance* drv, struct capref* cap, bool lmp)
{
    errval_t err, msgerr;

    DRIVERKIT_DEBUG("[dcontrol client] get_driver_ep_cap called.\n");
    if (drv->ctrl == NULL) {
        return DRIVERKIT_ERR_CONTROL_SERVICE_INIT;
    }

    err = slot_alloc(cap); 
    if (err_is_fail(err)) {
        return err;
    }

    assert(drv->ctrl);

    DRIVERKIT_DEBUG("[dcontrol client] Getting ep cap.\n");

    err = drv->ctrl->rpc_tx_vtbl.get_ep(drv->ctrl, lmp, cap, &msgerr);
    if (err_is_fail(err)) {
        slot_free(*cap);
        return err;
    }

    return msgerr;
}

