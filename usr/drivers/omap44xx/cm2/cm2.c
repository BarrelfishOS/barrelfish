/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */
#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>

#include "cm2.h"

void cm2_enable_hsmmc1(struct cm2_driver_state* st)
{
    omap44xx_l3init_cm2_cm_l3init_clkstctrl_clktrctrl_wrf(&st->l3init_cm2, 0x2);
    omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_modulemode_wrf(&st->l3init_cm2, 0x2);
    while (omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_idlest_rdf(&st->l3init_cm2) != 0x0);
}

void cm2_enable_i2c(struct cm2_driver_state* st, size_t i2c_index)
{
    assert (i2c_index < 4);

    omap44xx_l4per_cm2_cm_l4per_i2c_clkctrl_modulemode_wrf(&st->l4per_cm2, i2c_index, 0x2);
    while (omap44xx_l4per_cm2_cm_l4per_i2c_clkctrl_idlest_rdf(&st->l4per_cm2, i2c_index)
            != 0x0);
}

void cm2_init(struct cm2_driver_state* st)
{
    lvaddr_t l3init_vaddr;
    errval_t err = map_device_cap(st->cap, &l3init_vaddr);
    assert(err_is_ok(err));
    omap44xx_l3init_cm2_initialize(&st->l3init_cm2, (mackerel_addr_t)l3init_vaddr);
    omap44xx_l4per_cm2_initialize(&st->l4per_cm2, (mackerel_addr_t)l3init_vaddr);
}

int cm2_get_hsmmc1_base_clock(struct cm2_driver_state* st)
{
    return omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_clksel_rdf(&st->l3init_cm2) == 0x0 ?
           64000000 : 96000000;
}
