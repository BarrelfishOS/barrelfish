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
#include <dev/omap/omap44xx_l4per_cm2_dev.h>


//#define CM2_DEBUG_ENABLE 1

#if defined(CM2_DEBUG_ENABLE) || defined(GLOBAL_DEBUG)
#define CM2_DEBUG(x...) printf(x)
#else
#define CM2_DEBUG(x...) ((void)0)
#endif

struct cm2_driver_state {
    size_t level;
    struct capref cap;
    omap44xx_l3init_cm2_t l3init_cm2;
    omap44xx_l4per_cm2_t l4per_cm2;
};

/* cm2.c */
void cm2_enable_i2c(struct cm2_driver_state* st, size_t i2c_index);
int  cm2_get_hsmmc1_base_clock(struct cm2_driver_state* st);
void cm2_enable_hsmmc1(struct cm2_driver_state*);
void cm2_init(struct cm2_driver_state*);

/* service.c */
void cm2_init_service(struct cm2_driver_state* st, iref_t* iref);

#endif /* __OMAP44XX_CM2_H__ */
