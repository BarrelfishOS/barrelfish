/**
 * \file
 * \brief Architecture specific CPU bits.
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef AARCH64_BARRELFISH_CPU_H
#define AARCH64_BARRELFISH_CPU_H

#if __ARM_ARCH_8A__
#define CURRENT_CPU_TYPE CPU_ARM8
#else
#error "must define CURRENT_CPU_TYPE"
#endif

#endif
