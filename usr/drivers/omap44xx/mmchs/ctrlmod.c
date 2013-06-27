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
#include <gic.h>
#include <ti_twl6030.h>

#include <omap44xx_ctrlmod.h>

#define SYSCTRL_GENERAL_CORE 0x4a002000u
#define SYSCTRL_GENERAL_WKUP 0x4a30c000u
#define SYSCTRL_PADCONF_CORE 0x4a100000u
#define SYSCTRL_PADCONF_WKUP 0x4a31e000u

static omap44xx_ctrlmod_t ctrlmod;

/*
 * \brief Initialization of control module
 */
void ctrlmod_init(void)
{
    lvaddr_t vaddr[4];

    // 4K each, the regions are page aligned
    lpaddr_t addr[4] = {
        SYSCTRL_GENERAL_CORE,
        SYSCTRL_GENERAL_WKUP,
        SYSCTRL_PADCONF_CORE,
        SYSCTRL_PADCONF_WKUP
    };

    // Map
    for (int i=0; i<4; i++) {
        vaddr[i] = paging_map_device(addr[i], ARM_L1_SECTION_BYTES);
        vaddr[i]  += (addr[i] & ARM_L1_SECTION_MASK);
    }

    // Initialize Mackerel
    omap44xx_ctrlmod_initialize(&ctrlmod,
                                (mackerel_addr_t) vaddr[0], 
                                (mackerel_addr_t) vaddr[1], 
                                (mackerel_addr_t) vaddr[2], 
                                (mackerel_addr_t) vaddr[3]);
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
    printk(LOG_NOTE, "%s: Step 1\n", __FUNCTION__);
    omap44xx_ctrlmod_PBIASLITE_MMC1_PBIASLITE_PWRDNZ_wrf(&ctrlmod, 0x0);
    omap44xx_ctrlmod_PBIASLITE_MMC1_PWRDNZ_wrf(&ctrlmod, 0x0);

    // Step 2: preliminary settings for MMC1_PBIAS and MMC1 I/O cell
    printk(LOG_NOTE, "%s: Step 2\n", __FUNCTION__);
    //  1. turn of hiz mode
    omap44xx_ctrlmod_PBIASLITE_MMC1_PBIASLITE_HIZ_MODE_wrf(&ctrlmod, 0x0);
    //  2. setup PBIAS_IRQ (MA_IRQ_75)
    gic_enable_interrupt(PBIAS_IRQ,
                         GIC_IRQ_CPU_TRG_BSP,
                         0,//                         PIC_IRQ_PRIO_LOWEST,
                         GIC_IRQ_LEVEL_SENSITIVE,
                         GIC_IRQ_1_TO_N);
    //  3. pad multiplexing -- looks ok when dumping pad registers, so I'm
    //  not doing anything right now -SG
    //  4. set MMC1 speed control to 26MHz@30pF (0x0) -- alternative 65MHz@30pF (0x1)
    omap44xx_ctrlmod_CONTROL_MMC1_SDMMC1_SPEEDCTRL_wrf(&ctrlmod,
            omap44xx_ctrlmod_mmc1_spd_slow);
    //  5. set MMC1 pullup strength to 10-50kOhm (0x1) -- alt. 50-110kOhm (0x0)
    omap44xx_ctrlmod_CONTROL_MMC1_SDMMC1_PUSTRENGTH_wrf(&ctrlmod,
            omap44xx_ctrlmod_mmc1_pstr_weak);

    // Step 3: Program desired SDMMC1_VDDS for MMC I/O in I2C attached power
    // controller TODO? -- assuming 3.0V for now, manual says reset value is
    // 3.0V -SG
    // controller (3.0V)
    errval_t err = ti_twl6030_set_vmmc_vsel(3000);
    assert(err_is_ok(err));

    // Step 4: Set VMODE bit according to Step 3 (0x1 == 3.0V)
    printk(LOG_NOTE, "%s: Step 4\n", __FUNCTION__);
    omap44xx_ctrlmod_PBIASLITE_MMC1_PBIASLITE_VMODE_wrf(&ctrlmod, omap44xx_ctrlmod_vlt_hi);

    // Step 5: wait for SDMMC1_VDDS voltage to stabilize TODO
    // might already be stable after reset? -SG
    ti_twl6030_vmmc_pr();

    // Step 6: Disable PWRDNZ mode for MMC1_PBIAS and MMC1 I/O cell
    printk(LOG_NOTE, "%s: Step 6\n", __FUNCTION__);
    omap44xx_ctrlmod_PBIASLITE_MMC1_PBIASLITE_PWRDNZ_wrf(&ctrlmod, 0x1);
    omap44xx_ctrlmod_PBIASLITE_MMC1_PWRDNZ_wrf(&ctrlmod, 0x1);

    // Step 7: Store SUPPLY_HI_OUT bit
    uint8_t supply_hi_out = 
        omap44xx_ctrlmod_PBIASLITE_MMC1_PBIASLITE_SUPPLY_HI_OUT_rdf(&ctrlmod);
    printk(LOG_NOTE, "%s: Step 7: supply_hi_out = %d\n", __FUNCTION__, supply_hi_out);
    printk(LOG_NOTE, "%s: Step 7: vmode_error = %d\n", __FUNCTION__,
            omap44xx_ctrlmod_PBIASLITE_MMC1_PBIASLITE_VMODE_ERROR_rdf(&ctrlmod));

    // Wait for Interrupt
    while(!pbias_got_irq) { }

    printk(LOG_NOTE, "%s: Step 8\n", __FUNCTION__);

    // Step 8: check VMODE_ERROR and set PWRDNZ if error
    if (omap44xx_ctrlmod_PBIASLITE_MMC1_PBIASLITE_VMODE_ERROR_rdf(&ctrlmod)) {
        printk(LOG_NOTE, "got VMODE error\n");
        omap44xx_ctrlmod_PBIASLITE_MMC1_PWRDNZ_wrf(&ctrlmod, 0x0);
        omap44xx_ctrlmod_PBIASLITE_MMC1_PBIASLITE_PWRDNZ_wrf(&ctrlmod, 0x0);
    }
    
    // Step 9: check if SUPPLY_HI_OUT corresponds to SDMMC1_VDDS (3.0V)
    if (supply_hi_out != omap44xx_ctrlmod_vlt_hi) {
        printk(LOG_NOTE, "SDMMC1_VDDS seems to be != 3.0V\n");
        // TODO: redo setting SDMMC1_VDDS
    } else {
        // supply_hi_out should be 0x1 (3.0V)
        assert(supply_hi_out == omap44xx_ctrlmod_vlt_hi);
        // set VMODE bit to supply_hi_out
        omap44xx_ctrlmod_PBIASLITE_MMC1_PBIASLITE_VMODE_wrf(&ctrlmod, supply_hi_out);
    }

    // Step 12: clear PBIAS IRQ
    gic_ack_irq(PBIAS_IRQ);
    pbias_got_irq = 0;
}

void pbias_handle_irq(void)
{
    printk(LOG_NOTE, "got pbias interrupt");

    // set got-irq flag
    
    pbias_got_irq = true;

    // Activate interrupts again .. 
    __asm volatile ("CPSIE aif");
}

