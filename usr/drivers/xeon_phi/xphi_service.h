/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef XEON_PHI_XPHI_SERVICE_H_
#define XEON_PHI_XPHI_SERVICE_H_


/**
 *
 */
errval_t xeon_phi_service_init(struct xeon_phi *phi);

errval_t xeon_phi_service_open_channel(struct capref cap,
                                       uint8_t type,
                                       uint64_t target_domain,
                                       xphi_dom_id_t src_domain);

#endif /* XEON_PHI_XPHI_SERVICE_H_ */
