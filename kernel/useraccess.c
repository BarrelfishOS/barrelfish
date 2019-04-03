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

#include <kernel.h>
#include <useraccess.h>
#include <barrelfish_kpi/paging_arch.h>

/**
 * Check the validity of the user space buffer.
 *
 * \param type   Type of access to check: ACCESS_WRITE or ACCESS_READ.
 * \param buffer Pointer to beginning of buffer.
 * \param size   Size of buffer in bytes.
 */
bool access_ok(uint8_t type, lvaddr_t buffer, size_t size)
{
    debug(SUBSYS_SYSCALL, "%s: buffer=%#"PRIxLVADDR", size=%zu\n", __FUNCTION__, buffer, size);
    // Check that the provided buffer is fully in user space, i.e. below MEMORY_OFFSET
    if (buffer + size >= MEMORY_OFFSET) {
        return false;
    }

    // check if we have valid ptentries for base .. base + npages
    return paging_is_region_valid(buffer, size, type);
}
