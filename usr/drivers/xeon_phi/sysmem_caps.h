/**
 * \file
 * \brief Card side system memory caps manager
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef XEON_PHI_SYSMEM_CAPS_H_
#define XEON_PHI_SYSMEM_CAPS_H_

/*
 * XXX: This manager relies on the 1:1 mapping of the system memory
 *      in the system memory page tables!
 */

/**
 * \brief Initializes the capability manager of the system memory range
 *
 * \return SYS_ERR_OK on success,
 */
errval_t sysmem_cap_manager_init(struct capref sysmem_cap);

/**
 * \brief Returns a previously requested system memory capability to the
 *        cap manager
 */
errval_t sysmem_cap_return(struct capref frame);

/**
 * \brief Requests a certain system memory capability based on the base and
 *        length requirements
 *
 * \param base  the base address of the system memory (host address)
 * \param bits  the size of the requested capability in bits
 * \param frame capability representing the system memory frame
 */
errval_t sysmem_cap_request(lpaddr_t base,
                            uint8_t bits,
                            struct capref *frame);

#endif /* XEON_PHI_SYSMEM_CAPS_H_ */
