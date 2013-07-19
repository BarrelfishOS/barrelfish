/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/inthandler.h>
#include <barrelfish/waitset.h>

#include <driverkit/driverkit.h>

#include <arm_hal.h>
#include <gic.h>

#include "ti_twl6030.h"
#include "omap44xx_ctrlmod.h"

#define SYSCTRL_PADCONF_CORE 0x4a100000u
static omap44xx_sysctrl_padconf_core_t ctrlmod;

/*
 * \brief Initialization of control module
 */
void ctrlmod_init(void)
{
    lvaddr_t vaddr;
    errval_t err = map_device_register(SYSCTRL_PADCONF_CORE, 0x1000, &vaddr);
    assert(err_is_ok(err));

    // Initialize Mackerel
    omap44xx_sysctrl_padconf_core_initialize(&ctrlmod, (mackerel_addr_t) vaddr); 
}

static bool pbias_got_irq = false;
/*
 * We need to configure the extended-drain I/O pads to the right voltage and
 * turn on the external power supply (TWL6030 on the pandaboard)
 */
void sdmmc1_enable_power(void)
{
    // compare with Table 18-109 in OMAP TRM, p3681
    // Step 1: software must keep PWRDNZ low when setting up voltages
    printf("%s: Step 1\n", __FUNCTION__);
    omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_pwrdnz_wrf(&ctrlmod, 0x0);
    omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pwrdnz_wrf(&ctrlmod, 0x0);

    // Step 2: preliminary settings for MMC1_PBIAS and MMC1 I/O cell
    printf("%s: Step 2\n", __FUNCTION__);
    //  1. turn of hiz mode
    omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_hiz_mode_wrf(&ctrlmod, 0x0);

    //  2. setup PBIAS_IRQ (MA_IRQ_75)
    errval_t err = inthandler_setup_arm(pbias_handle_irq, NULL, PBIAS_IRQ);
    assert(err_is_ok(err));

    //  3. pad multiplexing -- looks ok when dumping pad registers, so I'm
    //  not doing anything right now -SG
    //  4. set MMC1 speed control to 26MHz@30pF (0x0) -- alternative 65MHz@30pF (0x1)
    omap44xx_sysctrl_padconf_core_control_mmc1_sdmmc1_dr0_speedctrl_wrf(&ctrlmod,
            0x0);
    omap44xx_sysctrl_padconf_core_control_mmc1_sdmmc1_dr1_speedctrl_wrf(&ctrlmod,
            0x0);
    omap44xx_sysctrl_padconf_core_control_mmc1_sdmmc1_dr2_speedctrl_wrf(&ctrlmod,
            0x0);
    //  5. set MMC1 pullup strength to 10-50kOhm (0x1) -- alt. 50-110kOhm (0x0)
    omap44xx_sysctrl_padconf_core_control_mmc1_sdmmc1_pustrength_grp0_wrf(&ctrlmod,
            0x0);
    omap44xx_sysctrl_padconf_core_control_mmc1_sdmmc1_pustrength_grp1_wrf(&ctrlmod,
            0x0);
    omap44xx_sysctrl_padconf_core_control_mmc1_sdmmc1_pustrength_grp2_wrf(&ctrlmod,
            0x0);
    omap44xx_sysctrl_padconf_core_control_mmc1_sdmmc1_pustrength_grp3_wrf(&ctrlmod,
            0x0);

    // Step 3: Program desired SDMMC1_VDDS for MMC I/O in I2C attached power
    // controller TODO? -- assuming 3.0V for now, manual says reset value is
    // 3.0V -SG
    // controller (3.0V)
    //ti_twl6030_vmmc_off();
    err = ti_twl6030_set_vmmc_vsel(3000);
    assert(err_is_ok(err));

    // Step 4: Set VMODE bit according to Step 3 (0x1 == 3.0V)
    printf("%s: Step 4\n", __FUNCTION__);
    omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_vmode_wrf(&ctrlmod, 0x1);

    // Step 5: wait for SDMMC1_VDDS voltage to stabilize TODO
    // might already be stable after reset? -SG
    //ti_twl6030_vmmc_pr();
    //mmchs_wait_msec(1000);

    // Step 6: Disable PWRDNZ mode for MMC1_PBIAS and MMC1 I/O cell
    printf("%s: Step 6\n", __FUNCTION__);
    omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_pwrdnz_wrf(&ctrlmod, 0x1);
    omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pwrdnz_wrf(&ctrlmod, 0x1);

    //ti_twl6030_vmmc_on();

    printf("%s:%d: wait until supply_hi_out is 0x1\n", __FUNCTION__, __LINE__);
    while(omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_supply_hi_out_rdf(&ctrlmod)
          != 0x1) {}
    printf("%s:%d: done waiting for suply hi out\n", __FUNCTION__, __LINE__);

    // Step 7: Store SUPPLY_HI_OUT bit
    uint8_t supply_hi_out = 
        omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_supply_hi_out_rdf(&ctrlmod);
    printf("%s: Step 7: supply_hi_out = %d\n", __FUNCTION__, supply_hi_out);
    printf("%s: Step 7: vmode_error = %d\n", __FUNCTION__,
            omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_vmode_error_rdf(&ctrlmod));

    // Wait for Interrupt
    //printf("Waiting for pbias Interrupt (id=%d)\n", PBIAS_IRQ);
    //while(!pbias_got_irq) {
    //    event_dispatch(get_default_waitset());
    //}

    printf("%s: Step 8\n", __FUNCTION__);

    // Step 8: check VMODE_ERROR and set PWRDNZ if error
    if (omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_vmode_error_rdf(&ctrlmod)) {
        printf("got VMODE error\n");
        omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pwrdnz_wrf(&ctrlmod, 0x0);
        omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_pwrdnz_wrf(&ctrlmod, 0x0);
    }
    
    // Step 9: check if SUPPLY_HI_OUT corresponds to SDMMC1_VDDS (3.0V)
    if (supply_hi_out != 0x1) {
        printf("SDMMC1_VDDS seems to be != 3.0V\n");
        // TODO: redo setting SDMMC1_VDDS
    } else {
        // supply_hi_out should be 0x1 (3.0V)
        assert(supply_hi_out == 0x1);
        // set VMODE bit to supply_hi_out
        omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_vmode_wrf(&ctrlmod, supply_hi_out);
    }

    // Step 12: clear PBIAS IRQ
    pbias_got_irq = 0;

    char buf[2048];
    omap44xx_sysctrl_padconf_core_control_pbiaslite_pr(buf, 2047, &ctrlmod);
    printf("%s:%d: %s\n", __FUNCTION__, __LINE__, buf);

    ti_twl6030_vmmc_pr();
}

void pbias_handle_irq(void *args)
{
    printf("got pbias interrupt\n");
    // set got-irq flag
    pbias_got_irq = true;
}

