/**
 * \file
 * \brief Struct definition for the boot param struct supplied by the
 *        K1OM boot loader
 */

/*
 * Copyright (c) 2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 *
 * This is adapted from the Linux kernel (kernel.org)
 *
 */

#ifndef KERNEL_IST_BIOS_H
#define KERNEL_IST_BIOS_H

struct ist_info {
    uint32_t signature;
    uint32_t command;
    uint32_t event;
    uint32_t perf_level;
};

#endif /* KERNEL_IST_BIOS_H */
