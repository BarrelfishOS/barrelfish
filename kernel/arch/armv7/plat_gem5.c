/**
 * \file
 * \brief Platform code for GEM5, modelling a Versatile Express board.
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <init.h>
#include <platform.h>

/* GEM5 implements 2GB of RAM. */
size_t platform_get_ram_size(void)
{
    assert(!mmu_is_enabled());
    return 2 * (1UL << 30);
}

/* GEM5 doesn't implement the CBAR, so we've got to hardcode the base address
 * of the private peripheral region. */
lpaddr_t platform_get_private_region(void) {
    return 0x2c000000; /* Hardcoded value for GEM5. */
}
