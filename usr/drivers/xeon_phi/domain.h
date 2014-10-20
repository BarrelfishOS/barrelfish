/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_DOMAIN_H
#define XEON_PHI_DOMAIN_H

/**
 * \brief Non-blocking name service lookup
 *
 * \param iface     Name of the domain
 * \param retdomid  returns the Xeon Phi Domain ID
 */
errval_t domain_lookup(const char *iface,
                       xphi_dom_id_t *retdomid);

/**
 * \brief looks up the name and registers a callback
 *
 * \param iface     Name of the domain
 * \param retdomid  returns the Xeon Phi Domain ID
 * \param state     User state pointer
 */
errval_t domain_wait(const char *iface,
                     struct xnode *node,
                     void *state,
                     xphi_dom_id_t *retdom);

/**
 * \brief Register with name service
 *
 * \param iface     Name of the domain
 * \param retdomid  returns the Xeon Phi Domain ID
 */
errval_t domain_register(const char *iface,
                         xphi_dom_id_t domid);

#endif /* XEON_PHI_DOMAIN_H_ */
