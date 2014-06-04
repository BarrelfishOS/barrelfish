/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
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
#include <barrelfish/spawn_client.h>

#include <xeon_phi/xeon_phi_messaging.h>

#include "xeon_phi.h"
#include "spawn.h"

/**
 * \brief handles the spawning of a new domain on the card
 *
 * \param phi   xeon phi data structure
 * \param data  payload of the received message
 *
 * \returns     SYS_ERR_OK on success
 */
errval_t xeon_phi_spawn_spawn(struct xeon_phi *phi,
                              struct xeon_phi_msg_spawn *data)
{


    errval_t err;

    char *argv[1];
    argv[0] = NULL;

    char name[sizeof(data->name)+15];
    snprintf(name, sizeof(data->name)+15, "%s", "xeon_phi_test");

    XSPAWN_DEBUG("Spawning %s\n", name);
    /*
     * TODO: handle arguments
     */
    domainid_t new_domain;
    err = spawn_program(data->core, name, argv, NULL, 0, &new_domain);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed spawn %s on core %d", data->name, data->core);
    }

    XSPAWN_DEBUG("Spawned %s\n", data->name);

    return SYS_ERR_OK;
}
