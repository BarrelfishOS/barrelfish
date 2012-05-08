/**
 * \file
 * \brief Architecture specific CPU bits.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_KPI_CPU_H
#define BARRELFISH_KPI_CPU_H

#ifndef __ASSEMBLER__

enum cpu_type {
    CPU_X86_64,
    CPU_X86_32,
    CPU_SCC,
    CPU_ARM, // XXX: Which ARMs to add here?
    CPU_TYPE_NUM // must be last
};
#endif

// XXX: Code that needs these includes should includes should include it directly
#include <barrelfish_kpi/generic_arch.h>

#endif // BARRELFISH_KPI_CPU_H
