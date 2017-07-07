/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */
#ifndef __TI_I2C_H__
#define __TI_I2C_H__

#include <barrelfish/types.h>
#include <errors/errno.h>

// there are 4 GP i2c controllers on the pandaboard
#define I2C_COUNT 4

enum i2c_flags {
    I2C_RD     = 0x1,
    I2C_WR     = 0x2,
    I2C_NOSTOP = 0x4,
};

struct i2c_msg {
    // should not exceed 10 bits
    uint16_t slave;
    enum i2c_flags flags;
    uint16_t length;
    uint8_t *buf;
};

struct twl6030_driver_state;
void ti_i2c_init(struct twl6030_driver_state* st, int i);
errval_t ti_i2c_transfer(int i, struct i2c_msg *msgs, size_t msgcount);
lpaddr_t i2c_get_pbase(size_t dev);

#endif // __TI_I2C_H__
