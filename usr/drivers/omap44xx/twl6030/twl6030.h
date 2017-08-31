/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */
#ifndef __TI_TWL6030_H__
#define __TI_TWL6030_H__

#include <barrelfish/barrelfish.h>
#include <errors/errno.h>
#include <if/cm2_defs.h>

// I2C Host controller id
#define I2C_HC 0

//#define TWL_SERIVCE_DEBUG 1

#if defined(TWL_SERIVCE_DEBUG) || defined(MMCHS_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define TWL_DEBUG(x...) printf(x)
#else
#define TWL_DEBUG(x...) ((void)0)
#endif

uint8_t _ti_twl6030_id1_read_8(void *d, size_t off);
void _ti_twl6030_id1_write_8(void *d, size_t off, uint8_t regval);
#define ti_twl6030_id1_read_8(dev, off) _ti_twl6030_id1_read_8(dev, off)
#define ti_twl6030_id1_write_8(dev, off, regval) _ti_twl6030_id1_write_8(dev, off, regval)
#include <dev/ti_twl6030_dev.h>

struct twl6030_driver_state {
    size_t level;
    struct capref cap;
    ti_twl6030_t twl;
    struct cm2_binding* cm2_binding;
};

void ti_twl6030_init(struct twl6030_driver_state*);
errval_t ti_twl6030_set_vmmc_vsel(ti_twl6030_t twl, int millis);
void ti_twl6030_vmmc_pr(ti_twl6030_t twl);
void ti_twl6030_vmmc_on(ti_twl6030_t twl);
void ti_twl6030_vmmc_off(ti_twl6030_t twl);

/* service.c */
void twl6030_init_service(struct twl6030_driver_state* st, iref_t* iref);


#endif // __TI_TWL6030_H__
