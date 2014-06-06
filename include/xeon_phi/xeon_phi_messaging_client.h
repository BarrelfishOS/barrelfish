/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_MESSAGING_CLIENT_H
#define XEON_PHI_MESSAGING_CLIENT_H


/**
 * \brief sends a OPEN command message over the Xeon Phi channel
 *
 * \param iface the name of the iface to talk to
 * \param frame capability representing the allocated frame
 * \param type  type of the channel
 *
 * \return SYS_ERR_OK on success
 */
errval_t xeon_phi_messaging_open(char *iface,
                                 struct capref frame,
                                 uint8_t type);

/**
 * \brief sends a SPAWN command message over the Xeon Phi channel
 *        to spawn a new domain on the other side
 *
 * \param core  core ID of the core to spawn the program on
 * \param name  path to the program to spawn
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_messaging_spawn(coreid_t core, char *name);

/**
 * \brief sends a KILL command over the Xeon Phi channel to terminated
 *        a previously spawned domain
 *
 * \param d the domain ID of the domain to terminate
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_messaging_kill(domainid_t d);

#endif // XEON_PHI_MESSAGING_CLIENT_H
