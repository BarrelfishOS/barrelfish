/**
 * \file
 * \brief ROM-controlled core boot for the OMAP44xx
 */

/*
 * Copyright (c) 2009-2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <boot_protocol.h>
#include <cp15.h>
#include <dev/omap/omap44xx_cortexa9_wugen_dev.h>
#include <omap44xx_map.h>

/* The boot driver entry point. */
extern char start;

/* Wake the second core, so that it advances into the same wfe loop as all
 * other ARM systems. */
void
plat_advance_aps(void) {
    omap44xx_cortexa9_wugen_t wugen;
    omap44xx_cortexa9_wugen_initialize(&wugen,
            (mackerel_addr_t)OMAP44XX_MAP_CORTEXA9_WUGEN);

    // Set address where the other core should jump
    omap44xx_cortexa9_wugen_aux_core_boot_1_wr(&wugen, (uint32_t)&start);

    // Tell ROM code to start other core
    omap44xx_cortexa9_wugen_aux_core_boot_0_cpu1_status_wrf(&wugen, 1);

    // Send signal to app core to start
    sev();
}
