/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_DOMAIN_H_
#define XEON_PHI_DOMAIN_H_


#define XEON_PHI_DOMAIN_DONT_CARE 0xFF

#define XEON_PHI_DOMAIN_HOST 0x10

/**
 * \brief builds the iface name representation
 *
 * \param name  Name of the domain
 * \param xid   Xeon Phi ID or XEON_PHI_DOMAIN_HOST the domain is runnig on
 * \param core  The core the domain is running on
 *
 * \returns string with the proper format
 */
char *xeon_phi_domain_build_iface(const char *name,
                                  xphi_id_t xid,
                                  coreid_t core);

/**
 * \brief Non-blocking name service lookup
 *
 * \param iface     Name of the domain
 * \param retdomid  returns the Xeon Phi Domain ID
 */
errval_t xeon_phi_domain_lookup(const char *iface,
                                xphi_dom_id_t *retdomid);

/**
 * \brief Blocking name service lookup
 *
 * \param iface     Name of the domain
 * \param retdomid  returns the Xeon Phi Domain ID
 */
errval_t xeon_phi_domain_blocking_lookup(const char *iface,
                                         xphi_dom_id_t *retdomid);

/**
 * \brief Register with name service
 *
 * \param iface     Name of the domain
 * \param retdomid  returns the Xeon Phi Domain ID
 */
errval_t xeon_phi_domain_register(const char *iface,
                                  xphi_dom_id_t domid);

#endif // XEON_PHI_DOMAIN_H_
