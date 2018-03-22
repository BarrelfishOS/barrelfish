/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
 * Attn: Systems Group.
 */

#ifndef DRIVERKIT_CONTROL_H
#define DRIVERKIT_CONTROL_H 1

#include <barrelfish/types.h>
#include <errors/errno.h>

/**
 * @brief creates a new endpoint to a driver instance
 *
 * @param drv       driver instance we want an endpoint to
 * @param cap       return cap i.e. the endpoint, slot will be allocted
 * @param lmp       type of the endpoint (true = LMP false = UMP)
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_get_driver_ep_cap(struct driver_instance* drv, struct capref* cap, bool lmp);

#endif // DRIVERKIT_CONTROL_H
