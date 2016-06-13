/**
 * \file
 * \brief Read the configuration base register (CBAR)
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <cp15.h>
#include <kernel.h>
#include <platform.h>

/* For all sensible implementations of ARMv7-A platforms, you can find the
 * base of the private peripheral region by reading the cp15 CBAR register. */

lpaddr_t platform_get_private_region(void) {
    return cp15_read_cbar();
}
