/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MMCHS2_H
#define MMCHS2_H

#include <barrelfish/barrelfish.h>
#include <dev/omap/omap44xx_mmchs1_dev.h>
#include <dev/omap/omap44xx_l3init_cm2_dev.h>
#include <dev/omap/omap44xx_ckgen_cm2_dev.h>
#include <dev/omap/omap44xx_l4per_cm2_dev.h>

#include "mmchs_debug.h"
#include "omap44xx_cm2.h"
#include "omap44xx_ctrlmod.h"
#include "i2c.h"
#include "twl6030.h"

#define DBUF_SIZE (10*4096)

struct mmchs_driver_state {
    uint64_t level;

    omap44xx_l3init_cm2_t l3init_cm2;
    omap44xx_l4per_cm2_t l4per_cm2;
    omap44xx_ckgen_cm2_t clkgen_cm2;
    omap44xx_sysctrl_padconf_core_t ctrlmod;
    omap44xx_mmchs1_t mmchs;
    ti_twl6030_t twl;

    struct capref* caps;


    char dbuf[DBUF_SIZE];
};

void mmchs_init(struct mmchs_driver_state*);
errval_t mmchs_read_block(struct mmchs_driver_state*, size_t block_nr, void *buffer);
errval_t mmchs_write_block(struct mmchs_driver_state*, size_t block_nr, void *buffer);

void init_service(struct mmchs_driver_state*);

#endif // MMCHS2_H
