/**
 * \file
 * \brief User space memory access functions.
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USERACCESS_H
#define USERACCESS_H

/**
 * Type of access to a user space memory region.
 */
#define ACCESS_READ 0
#define ACCESS_WRITE 1

/**
 * Check the validity of the user space buffer.
 *
 * \param type   Type of access to check: ACCESS_WRITE or ACCESS_READ.
 * \param buffer Pointer to beginning of buffer.
 * \param size   Size of buffer.
 */
bool access_ok(uint8_t type, lvaddr_t buffer, size_t size);

/**
 * Walk page tables to check if we can access region
 * This needs to be implemented for every arch we support
 */
bool paging_is_region_valid(lvaddr_t base, size_t npages, uint8_t type);

#endif // USER_ACCESS_H
