/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#ifndef __OMAP44XX_CM2_H__
#define __OMAP44XX_CM2_H__

#include <barrelfish/barrelfish.h>
#include <dev/omap/omap44xx_l3init_cm2_dev.h>
#include <dev/omap/omap44xx_ckgen_cm2_dev.h>
#include <dev/omap/omap44xx_l4per_cm2_dev.h>

struct mmchs_driver_state;

void cm2_enable_i2c(struct mmchs_driver_state* st, size_t i2c_index);
int  cm2_get_hsmmc1_base_clock(struct mmchs_driver_state* st);
void cm2_enable_hsmmc1(struct mmchs_driver_state*);
void cm2_init(struct mmchs_driver_state*);

#endif /* __OMAP44XX_CM2_H__ */
