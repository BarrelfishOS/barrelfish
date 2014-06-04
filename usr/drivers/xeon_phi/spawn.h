/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_SPAWN_H_
#define XEON_PHI_SPAWN_H_

/**
 * \brief handles the spawning of a new domain on the card
 *
 * \param phi   xeon phi data structure
 * \param data  payload of the received message
 *
 * \returns     SYS_ERR_OK on success
 */
errval_t xeon_phi_spawn_spawn(struct xeon_phi *phi,
                              struct xeon_phi_msg_spawn *data);


/**
 * \brief handles the program termination of the domain
 *
 * \param phi    pointer ot the card meta data structure
 * \param domain the domain to terminate
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_spawn_kill(struct xeon_phi *phi,
                             domainid_t domain);

#endif /* XEON_PHI_SPAWN_H_ */
