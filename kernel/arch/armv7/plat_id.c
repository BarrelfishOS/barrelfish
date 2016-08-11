/*
 * Copyright (c) 2009-2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <cp15.h>
#include <platform.h>

void
armv7_get_info(struct arch_info_armv7 *ai) {
    ai->ncores  = platform_get_core_count();
    ai->midr    = cp15_read_midr();
    ai->ctr     = cp15_read_ctr();
    ai->id_pfr0 = cp15_read_id_pfr0();
    ai->id_pfr1 = cp15_read_id_pfr1();
    ai->id_dfr0 = cp15_read_id_dfr0();
    ai->id_afr0 = cp15_read_id_afr0();
}
