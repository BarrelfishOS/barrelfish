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

#include <arch/arm/omap44xx/device_registers.h>
#include <dev/omap/omap44xx_l3init_cm2_dev.h>
#include <dev/omap/omap44xx_ckgen_cm2_dev.h>
#include <dev/omap/omap44xx_l4per_cm2_dev.h>

#include "omap44xx_cm2.h"

static omap44xx_l3init_cm2_t l3init_cm2;
static omap44xx_l4per_cm2_t l4per_cm2;
static omap44xx_ckgen_cm2_t clkgen_cm2;

#define CM2_BUF_SIZE (64*1024)
static char cm2_buf[CM2_BUF_SIZE];

/*
 * \brief Enable the clock
 *
 * See Free BSD: omap4_prcm_clks.c - omap4_clk_generic_activate()
 *
 * Module should be "fully functional" if ...
 * .. IDLEST reads as 0
 * .. STBYST reads as 0 (x)
 * Both of these fields are read-only values ..
 *
 * (x) won't enable here, but I saw this enabled once ..
 *     I guess this is happening some time later ..
 *     I think we can flip this bit by disabling standby mode on the MMC
 */
void cm2_enable_hsmmc1(void)
{
    omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_clksel_wrf(&l3init_cm2, 0x0);

    // Handling clock dependencies!
    //omap44xx_ckgen_cm2_cm_div_m4_dpll_per_pr(cm2_buf, CM2_BUF_SIZE, &ckgen_cm2);
    //printf("%s", cm2_buf);

    // Disabling clock manually
    omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_modulemode_wrf(&l3init_cm2, 0x0);

    // Waiting for clock to be disabled ...
    while (omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_idlest_rdf(&l3init_cm2) != 0x3);

    if (omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_idlest_rdf(&l3init_cm2)==0x0) {
        // Don't mess with the power controler the MMC is not powered up yet.
        printf("cm2: Clock for HSMMC1 is already enabled\n");
        return;
    }

    printf("cm2: Powering on HSMMC1... ");
    omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_modulemode_wrf(&l3init_cm2, 0x2);

    volatile int cm2loop = 0;
    while (omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_idlest_rdf(&l3init_cm2)!=0) {
        if (++cm2loop>1000) {
            assert(!"cm2: MMC power won't come up... "
                   "Don't know what to do, IDLEST and STBYST are ro");
        }
    }
    printf("done (%d cycles).\n", cm2loop);

    if (omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_stbyst_rdf(&l3init_cm2)==0x1) {
        printf("cm2: module MMC1 is in standby state.\n");
    }
}

void cm2_enable_i2c(size_t i2c_index)
{
    assert (i2c_index < 4);

    //omap44xx_l4per_prm_pm_l4per_pwrstctrl_prf(cm2_buf, 1024-1, &l4per_prm);
    //printf("%s\n", cm2_buf);

    omap44xx_l4per_cm2_cm_l4per_i2c_clkctrl_modulemode_wrf(&l4per_cm2, i2c_index, 0x2);

    // wait for module to get into functional state
    while(omap44xx_l4per_cm2_cm_l4per_i2c_clkctrl_idlest_rdf(&l4per_cm2, i2c_index)
            != 0x0);
}

void cm2_init(void)
{
    lvaddr_t l3init_vaddr;
    errval_t err = map_device_register(OMAP44XX_CM2, 0x1000, &l3init_vaddr);
    assert(err_is_ok(err));
    omap44xx_l3init_cm2_initialize(&l3init_cm2, (mackerel_addr_t)l3init_vaddr);
    
    lvaddr_t clkgen_vaddr;
    err = map_device_register(OMAP44XX_CLKGEN_CM2, 0x1000, &clkgen_vaddr);
    assert(err_is_ok(err));
    omap44xx_ckgen_cm2_initialize(&clkgen_cm2, (mackerel_addr_t)clkgen_vaddr);

    lvaddr_t l4per_vaddr;
    err = map_device_register(OMAP44XX_L4PER_CM2, 0x1000, &l4per_vaddr);
    assert(err_is_ok(err));
    omap44xx_l4per_cm2_initialize(&l4per_cm2, (mackerel_addr_t)l4per_vaddr);
}

int cm2_get_hsmmc1_base_clock(void)
{
    return omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_clksel_rdf(&l3init_cm2) == 0x0 ?
           64000000 : 96000000;
}

/*
 * \brief Print CM2 registers
 */
void cm2_debug_print(void)
{
    omap44xx_l3init_cm2_pr(cm2_buf, 1024-1, &l3init_cm2);
    printf("%s\n", cm2_buf);

    omap44xx_ckgen_cm2_pr(cm2_buf, 1024-1, &clkgen_cm2);
    printf("%s\n", cm2_buf);

    omap44xx_l4per_cm2_pr(cm2_buf, 1024-1, &l4per_cm2);
    printf("%s\n", cm2_buf);
}

void cm2_print_standby_state(void)
{
    omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_pr(cm2_buf, 1024-1, &l3init_cm2);
    printf("%s\n", cm2_buf);
}

