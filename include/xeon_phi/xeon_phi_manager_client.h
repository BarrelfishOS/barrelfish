/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_MANAGER_CLIENT_H
#define XEON_PHI_MANAGER_CLIENT_H

#define XEON_PHI_MANAGER_SERVICE_NAME "xeon_phi_manager"

#define DEBUG_XPMC(x...) debug_printf(" XPMC | " x)

/**
 * \brief   registers the Xeon Phi driver card with the Xeon Phi Manager
 *          this function blocks until we have a connection to the manager
 *
 * \param   svc_iref    the iref of the drivers service
 * \param   id          the own card id
 * \param   num         returns the number of returned irefs / number of cards
 * \param   cards       returns the array of irefs
 *
 * \return SYS_ERR_OK on success
 */
errval_t xeon_phi_manager_client_register(iref_t svc_iref,
                                          uint8_t *id,
                                          uint8_t *num,
                                          iref_t **cards);

/**
 * \brief   deregisters the Xeon Phi driver with the Xeon Phi Manager
 *
 * \return SYS_ERR_OK on success
 */
errval_t xeon_phi_manager_client_deregister(void);

#endif // XEON_PHI_MANAGER_CLIENT_H
