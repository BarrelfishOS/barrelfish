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
    PI_PLATFORM_TMAS,
};

/// Struct that can be used to request/parse platform information
struct platform_info {
    enum pi_arch           arch;       // the architecture
    enum pi_platform       platform;   // the platfrom
};

#endif
