/**
 * \file
 * \brief Platform information declarations
:qa
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_KPI_PLATFORM_H
#define BARRELFISH_KPI_PLATFORM_H

#include <barrelfish/static_assert.h>

enum pi_arch {
    PI_ARCH_X86,
    PI_ARCH_ARMV7A,
    PI_ARCH_ARMV8A,
};

enum pi_platform {
    // placeholder platform
    PI_PLATFORM_UNKNOWN,
    // x86 platforms
    PI_PLATFORM_PC,
    // armv7 platforms
    PI_PLATFORM_OMAP44XX,
    PI_PLATFORM_VEXPRESS,
    PI_PLATFORM_ZYNQ7,
    // armv8 platforms
    PI_PLATFORM_APM88XXXX,
    PI_PLATFORM_FVP,
    PI_PLATFORM_QEMU,
    PI_PLATFORM_CN88XX,
};

/* Must be at least as large as all architectures' structs. */
#define PI_ARCH_INFO_SIZE 255

struct arch_info_armv7 {
    /* The number of cores in the system, as reported by the GIC.  The cores
     * in the primary cluster will be numbered sequentially, those in other
     * clusters (big.little) probably aren't. */
    uint32_t ncores;

    /* The various identification registers. */
    uint32_t midr, ctr, id_pfr0, id_pfr1;
    uint32_t id_dfr0, id_afr0;
};
STATIC_ASSERT_SIZEOF(struct arch_info_armv7, 28);

STATIC_ASSERT(PI_ARCH_INFO_SIZE >= sizeof(struct arch_info_armv7), \
              "Overflowed PI_ARCH_INFO_SIZE");

struct arch_info_armv8 {
    /* The number of cores in the system, as reported by the GIC.  The cores
     * in the primary cluster will be numbered sequentially, those in other
     * clusters (big.little) probably aren't. */
    uint32_t ncores;

    /* The various identification registers. */
    uint32_t midr, ctr, id_pfr0, id_pfr1;
    uint32_t id_dfr0, id_afr0;
};
STATIC_ASSERT_SIZEOF(struct arch_info_armv8, 28);

STATIC_ASSERT(PI_ARCH_INFO_SIZE >= sizeof(struct arch_info_armv8), \
              "Overflowed PI_ARCH_INFO_SIZE");

/// Struct that can be used to request/parse platform information
struct platform_info {
    enum pi_arch           arch;       // the architecture
    enum pi_platform       platform;   // the platfrom

    /* Some platforms e.g. ARMv7 need the CPU driver to probe the hardware for
     * them, e.g. for the number of CPUs. */
    union {
        struct arch_info_armv7 armv7;
        struct arch_info_armv8 armv8;
    } arch_info;
};

#endif
