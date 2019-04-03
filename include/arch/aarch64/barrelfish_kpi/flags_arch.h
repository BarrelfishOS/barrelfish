/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_AARCH64_BARRELFISH_KPI_FLAGS_H
#define ARCH_AARCH64_BARRELFISH_KPI_FLAGS_H

#define AARCH64_MODE_EL0T   0x0
#define AARCH64_MODE_EL1T   0x4
#define AARCH64_MODE_EL1H   0x5

#define AARCH64_MODE_USR    AARCH64_MODE_EL0T
#define AARCH64_MODE_FIQ    0x11
#define AARCH64_MODE_IRQ    0x12
#define AARCH64_MODE_SVC    0x13
#define AARCH64_MODE_ABT    0x17
#define AARCH64_MODE_UND    0x1b
#define AARCH64_MODE_SYS    0x1f
#define AARCH64_MODE_MASK   0x1f
#define AARCH64_MODE_PRIV   0x0f

#define CPSR_IF_MASK    0xc0
#define CPSR_I_MASK     0x80
#define CPSR_F_MASK     0x40

#endif // ARCH_AARCH64_BARRELFISH_KPI_FLAGS_H
