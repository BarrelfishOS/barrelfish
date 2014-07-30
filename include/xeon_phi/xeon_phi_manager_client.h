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

/*
 * ----------------------------------------------------------------------------
 * This library is to be used solely by the Xeon Phi drivers to talk to the
 * Xeon Phi manager domain in order to setup the Xeon Phi IDs and the
 * inter-Xeon Phi driver connections.
 * ----------------------------------------------------------------------------
 */


/**
 * \brief registers the Xeon Phi driver card with the Xeon Phi Manager
 *
 * \param svc_iref  iref of the own exported Xeon Phi driver interface
 * \param id        returns the assigned Xeon Phi card ID
 * \param num       returns the size of the cards array
 * \param irefs     returns array of irefs to the other cards
 *
 * NOTE: this is a blocking function. The function will only return after
 *       the Xeon Phi manager connection has been fully established and the
 *       registration protocol has been executed.
 *
 * \returns SYS_ERR_OK on success
 *          errval on failure
 */
errval_t xeon_phi_manager_client_register(iref_t svc_iref,
                                          uint8_t *id,
                                          uint8_t *num,
                                          iref_t **irefs);

/**
 * \brief  deregisters the Xeon Phi driver with the Xeon Phi Manager
 *
 * \return SYS_ERR_OK on success
 *         errval on failure
 */
errval_t xeon_phi_manager_client_deregister(void);

/**
 * \brief looks up the iref of a Xeon Phi with the given id
 *
 * \param xid       Xeon Phi ID
 * \param svc_iref  the returned svc_iref of the xeon phi
 *
 * \returns SYS_ERR_OK on success
 */
errval_t xeon_phi_manager_lookup(xphi_id_t xid,
                                 iref_t *svc_iref);

#endif // XEON_PHI_MANAGER_CLIENT_H
