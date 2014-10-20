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

#ifndef XEON_PHI_SERVICE_H_
#define XEON_PHI_SERVICE_H_

/**
 * \brief initializes the service
 *
 * \param iref  returns the iref of the initialized service
 *
 * \return SYS_ERR_OK on success
 */
errval_t service_init(struct xeon_phi *phi);

/**
 * \brief registers the local service with the other Xeon Phi drivers
 *        in the topology
 *
 * \param phi   pointer to the local card structure
 * \param irefs the irefs of the other cards
 * \param num   the number of irefs in the array
 */
errval_t service_register(struct xeon_phi *phi,
                          iref_t *irefs,
                          uint8_t num);

/**
 * \brief registers an intra card communication frame
 *
 * \param phi      the local xeon phi card
 * \param xphi_id  target xeon phi id
 * \param cap      capability of the messaging frame
 */
errval_t service_bootstrap(struct xeon_phi *phi,
                           uint8_t xphi_id,
                           struct capref frame);

/**
 * \brief starts the service request handling
 */
errval_t service_start(struct xeon_phi *phi);

#endif // XEON_PHI_SERVICE_H_
