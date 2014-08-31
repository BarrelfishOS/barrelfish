/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __XOMP_GATEWAY_CLIENT_H
#define	__XOMP_GATEWAY_CLIENT_H

/**
 * \brief requests the frame capability which belongs to the virtual addrss
 *
 * \param addr          virtual address of the mapped frame
 * \param ret_frame     returns the frame capability
 *
 * \return SYS_ERR_OK on sucess
 *         errval on failure
 */
errval_t xomp_gateway_get_memory(lpaddr_t addr,
                                struct capref *ret_frame);

/**
 * \brief connects to the gateway service of the first domain spawned on the node
 *
 * \return SYS_ERR_OK on success
 *         errval on failure
 */
errval_t xomp_gateway_bind_svc(void);


#endif	/* __XOMP_HELPER_CLIENT_H */
