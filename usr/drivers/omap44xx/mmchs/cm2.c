/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <kernel.h>
#include <paging_kernel_arch.h>

#include <arm_hal.h>
#include <omap44xx_cm2.h>
#include <dev/omap/omap44xx_cm2_dev.h>

static int cm2_clock_enabled = 0;
static omap44xx_cm2_t cm2;

#define CM2_BUF_SIZE (64*1024)
char cm2_buf[CM2_BUF_SIZE];

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
    cm2_clock_enabled = 1;

    omap44xx_cm2_CM_L3INIT_HSMMC1_CLKCTRL_CLKSEL_wrf(&cm2, 0x0);

    // Handling clock dependencies!
    omap44xx_cm2_CM_DIV_M4_DPLL_PER_pr(cm2_buf, CM2_BUF_SIZE, &cm2);
    printf("%s", cm2_buf);

    // Disabling clock manually
    omap44xx_cm2_CM_L3INIT_HSMMC1_CLKCTRL_MODULEMODE_wrf(&cm2, 0x0);
    
    // Waiting for clock to be disabled ...
    while (omap44xx_cm2_CM_L3INIT_HSMMC1_CLKCTRL_IDLEST_rdf(&cm2)!=0x3);

    // Don't mess with the power controler unless we really believe 
    // that the MMC is not powered up yet.
    //
    if (omap44xx_cm2_CM_L3INIT_HSMMC1_CLKCTRL_IDLEST_rdf(&cm2)==0x0) {

        printf("cm2: Clock for HSMMC1 is already enabled\n");
        return;
    }

    printf("cm2: Powering on HSMMC1 .. ");
    omap44xx_cm2_CM_L3INIT_HSMMC1_CLKCTRL_MODULEMODE_wrf(&cm2, 0x2);

    volatile int cm2loop = 0;
    while (omap44xx_cm2_CM_L3INIT_HSMMC1_CLKCTRL_IDLEST_rdf(&cm2)!=0) {

        //        mmchs_wait_msec(1);

        if (++cm2loop>1000) {
            printf("failed\n");
            panic("cm2: MMC power won't come up .. "
                  "Don't know what to do, IDLEST and STBYST are ro");
        }
    }
    printf("done (%d cycles)\n", cm2loop);

    if (omap44xx_cm2_CM_L3INIT_HSMMC1_CLKCTRL_STBYST_rdf(&cm2)==0x1) {
        printf("cm2: module MMC1 is in standby state\n");
    }
}

void cm2_enable_i2c(int i2c_index)
{
    switch(i2c_index) {
        case 1:
            // enable i2c module
            omap44xx_cm2_l4per_i2c1_clkctrl_modulemode_wrf(&cm2,
                    omap44xx_cm2_modmode_swenabled);
            // wait for module to get into functional state
            while(omap44xx_cm2_l4per_i2c1_clkctrl_idlest_rdf(&cm2)
                    !=omap44xx_cm2_idlest_functional);
            break;
        case 2:
            // enable i2c module
            omap44xx_cm2_l4per_i2c2_clkctrl_modulemode_wrf(&cm2,
                    omap44xx_cm2_modmode_swenabled);
            // wait for module to get into functional state
            while(omap44xx_cm2_l4per_i2c2_clkctrl_idlest_rdf(&cm2)
                    !=omap44xx_cm2_idlest_functional);
            break;
        case 3:
            // enable i2c module
            omap44xx_cm2_l4per_i2c3_clkctrl_modulemode_wrf(&cm2,
                    omap44xx_cm2_modmode_swenabled);
            // wait for module to get into functional state
            while(omap44xx_cm2_l4per_i2c3_clkctrl_idlest_rdf(&cm2)
                    !=omap44xx_cm2_idlest_functional);
            break;
        case 4:
            // enable i2c module
            omap44xx_cm2_l4per_i2c4_clkctrl_modulemode_wrf(&cm2,
                    omap44xx_cm2_modmode_swenabled);
            // wait for module to get into functional state
            while(omap44xx_cm2_l4per_i2c4_clkctrl_idlest_rdf(&cm2)
                    !=omap44xx_cm2_idlest_functional);
            break;
        default:
            printk(LOG_WARN, "don't know how to enable clocks for I2C%d\n", i2c_index);
            break;
    }
}

void cm2_init(void)
{
    mackerel_addr_t cm2_vaddr = omap_dev_map(CM2_PADDR);
    mackerel_addr_t cm2_clkgen_vaddr = omap_dev_map(CM2_CLKGEN_PADDR);
    mackerel_addr_t cm2_l4per_vaddr = omap_dev_map(CM2_L4PER_PADDR);

    omap44xx_cm2_initialize(&cm2, cm2_vaddr, cm2_clkgen_vaddr, cm2_l4per_vaddr);
    
}

int cm2_get_hsmmc1_base_clock(void)
{
    return omap44xx_cm2_CM_L3INIT_HSMMC1_CLKCTRL_CLKSEL_rdf(&cm2) == 0x0 ?
        64000000 : 96000000;
}

/*
 * \brief Print CM2 registers
 */
void print_cm2(void)
{
    char buf[1024];
    omap44xx_cm2_pr(buf, 1024-1, &cm2);

    printf("%s\n", buf);
}

void cm2_print_standby_state(void)
{
    if (omap44xx_cm2_CM_L3INIT_HSMMC1_CLKCTRL_STBYST_rdf(&cm2)==0x0) {
        printf("cm2: mmc1 is functional (not in standby state)\n");
    } else if (omap44xx_cm2_CM_L3INIT_HSMMC1_CLKCTRL_STBYST_rdf(&cm2)==0x0) {
        printf("cm2: mmc1 is in stand-by state\n");
    } else {
        panic("Undefined standby state for MMC1");
    }
}

