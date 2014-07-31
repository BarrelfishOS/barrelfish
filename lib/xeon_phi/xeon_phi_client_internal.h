/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_CLIENT_INTERNAL_H_
#define XEON_PHI_CLIENT_INTERNAL_H_


/**
 * \brief looks up the domain ID given the information
 *
 * \param iface     Interface name of the domain
 * \param retdom    returned domain id
 *
 * \returns SYS_ERR_OK on success,
 *          XEON_PHI_ERR_CLIENT_DOMAIN_VOID,
 *          errval on error
 */
errval_t xeon_phi_client_domain_lookup(const char *iface,
                                       xphi_dom_id_t *retdom);

/**
 * \brief looks up the domain ID given the information and waits
 *        until the domain registers
 *
 * \param iface     Interface name of the domain
 * \param retdom    returned domain id
 *
 * \returns SYS_ERR_OK on success,
 *          XEON_PHI_ERR_CLIENT_DOMAIN_VOID,
 *          errval on error
 */
errval_t xeon_phi_client_domain_wait(const char *iface,
                                     xphi_dom_id_t *retdom);

/**
 * \brief registers a a domain
 *
 * \param iface     Interface name of the domain
 * \param retdom    returned domain id
 *
 * \returns SYS_ERR_OK on success,
 *          errval on error
 */
errval_t xeon_phi_client_domain_register(const char *iface,
                                         xphi_dom_id_t dom);

#endif /* INTERNAL_H_ */
