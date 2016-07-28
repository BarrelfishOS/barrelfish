/**
 * \file
 * \brief Platform code for the Cortex-A9 processors on TI OMAP44xx SoCs.
 */

/*
 * Copyright (c) 2009-2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <cache.h>
#include <platform.h>
#include <serial.h>
#include <assert.h>
#include <errors/errno.h>
#include <a9mpcore_map.h>
#include <omap44xx_map.h>
#include <omap_uart.h>
#include <cp15.h>
#include <a9_scu.h>
#include <a9_gt.h>
#include <gic.h>
#include <init.h>
#include <global.h>
#include <paging_kernel_arch.h>
#include <dev/omap/omap44xx_id_dev.h>
#include <dev/omap/omap44xx_emif_dev.h>
#include <dev/omap/omap44xx_cortexa9_wugen_dev.h>
#include <dev/omap/omap44xx_ckgen_cm1_dev.h>
#include <dev/omap/omap44xx_ckgen_prm_dev.h>
#include <dev/cortex_a9_pit_dev.h>

#define MSG(format, ...) printk( LOG_NOTE, "OMAP44xx: "format, ## __VA_ARGS__ )

/*****************************************************************************
 *
 * Implementation of serial.h
 *
 *****************************************************************************/

errval_t serial_init(unsigned port, bool initialize_hw)
{
    lvaddr_t base = paging_map_device(uart_base[port], uart_size[port]);
    omap_uart_init(port, base, initialize_hw);
    return SYS_ERR_OK;
};

/*
 * Print system identification.   MMU is NOT yet enabled.
 * Use Mackerel to print the identification from the system
 * configuration block.  Documentation in the OMAP4460 TRM p. 18.6.2 
 */
void platform_print_id(void)
{
    char buf[64];
    omap44xx_id_t id;
    omap44xx_id_initialize(&id,
            (mackerel_addr_t) OMAP44XX_MAP_L4_CFG_SYSCTRL_GENERAL_CORE);

    omap44xx_id_codevals_prtval(buf, 63, omap44xx_id_code_rawrd(&id));
    MSG("This is a %s\n", buf);

    omap44xx_id_stp_prtval(buf, 63, omap44xx_id_prod1_st_rdf(&id));
    MSG("Speed grade: %s\n", buf);

    size_t sz = platform_get_ram_size();
    MSG("We have %uMb of DRAM\n", sz / 1024 / 1024);
}

void platform_get_info(struct platform_info *pi)
{
    pi->arch     = PI_ARCH_ARMV7A;
    pi->platform = PI_PLATFORM_OMAP44XX;
    armv7_get_info(&pi->arch_info.armv7);
}

/**
 * Read the details of a memory bank (0 or 1)
 */
static size_t bank_size(int bank, lpaddr_t base)
{
    int rowbits;
    int colbits;
    int rowsize;
    omap44xx_emif_t emif;
    omap44xx_emif_initialize(&emif, (mackerel_addr_t)base);

    assert( bank == 1 || bank == 2 );

    if (!omap44xx_emif_status_phy_dll_ready_rdf(&emif)) {
        MSG(" EMIF%d doesn't seem to have any DDRAM attached.\n", bank);
        return 0;
    }

    rowbits = omap44xx_emif_sdram_config_rowsize_rdf(&emif) + 9;
    colbits = omap44xx_emif_sdram_config_pagesize_rdf(&emif) + 9;
    rowsize = omap44xx_emif_sdram_config2_rdbsize_rdf(&emif) + 5;

    MSG(" EMIF%d ready, %d-bit rows, %d-bit cols, %d-byte row buffer\n",
            bank, rowbits, colbits, 1<<rowsize);

    return (1 << (rowbits + colbits + rowsize));
}

/**
 * Calculate the size of available RAM by reading each bank
 */
size_t platform_get_ram_size(void)
{
    assert(!paging_mmu_enabled());
    return bank_size(1, OMAP44XX_MAP_EMIF1) + bank_size(2, OMAP44XX_MAP_EMIF2);
}

/**
 * Notify the BSP that this AP has booted. 
 */
//
// Pages in memory to use for posting information.  Must be in RAM.
//
#define AP_WAIT_PHYS    ((lpaddr_t)0x80020000)
#define AP_GLOBAL_PHYS  ((lpaddr_t)0x80021000)
#define AP_STARTING_UP  4422
#define AP_STARTED      6633

/**
 * \brief Boot an arm app core
 *
 * \param core_id   ID of the core to try booting
 * \param entry     Entry address for new kernel in the destination
 *                  architecture's lvaddr_t
 *
 * \returns Zero on successful boot, non-zero (error code) on failure
 */
// XXX: panic() messes with GCC, remove attribute when code works again!
__attribute__((noreturn))
int platform_boot_aps(coreid_t core_id, genvaddr_t gen_entry)
{
    panic("Broken.\n");
#if 0
    assert(paging_mmu_enabled());
    lvaddr_t entry = (lvaddr_t) gen_entry;

    /* pointer to the pseudo-lock used to detect boot up of new core */
    volatile uint32_t *ap_wait = (uint32_t*)local_phys_to_mem(AP_WAIT_PHYS);
    *ap_wait = AP_STARTING_UP;
    //cp15_invalidate_d_cache();
    

    // map AUX_CORE_BOOT section
    static lvaddr_t aux_core_boot = 0;
    if (aux_core_boot == 0)
        aux_core_boot = paging_map_device( OMAP44XX_MAP_CORTEXA9_WUGEN, 
                                           OMAP44XX_MAP_CORTEXA9_WUGEN_SIZE );

    omap44xx_cortexa9_wugen_t wugen;
    omap44xx_cortexa9_wugen_initialize( &wugen, (mackerel_addr_t)aux_core_boot);

    //write entry address of new kernel to SYSFLAG reg
    // Set address where the other core should jump
    debug(SUBSYS_STARTUP, "setting aux_core_boot_1 to 0x%"PRIxLVADDR"\n", entry);
    omap44xx_cortexa9_wugen_aux_core_boot_1_wr(&wugen, entry);

    // Tell ROM code to start other core
    debug(SUBSYS_STARTUP, "aux_core_boot_0 |= 1<< 2\n");
    omap44xx_cortexa9_wugen_aux_core_boot_0_cpu1_status_wrf(&wugen, 0x1);

    // send signal to app core to start
    debug(SUBSYS_STARTUP, "sending event to other core(s?)\n");
    __asm__ volatile ("SEV");

    debug(SUBSYS_STARTUP, "waiting for response\n");
    while( omap44xx_cortexa9_wugen_aux_core_boot_0_cpu1_status_rdf(&wugen) != 0x2)
        ;
    debug(SUBSYS_STARTUP, "booted CPU%hhu\n", core_id);
    return 0;
#endif
}

void platform_notify_bsp(void)
{
    // Tell the BSP that we are started up
    // See Section 27.4.4 in the OMAP44xx manual for how this should work.
    // We do this early, to avoid having to map the registers
    assert(paging_mmu_enabled());
    omap44xx_cortexa9_wugen_t wugen;
    omap44xx_cortexa9_wugen_initialize(&wugen,
            (mackerel_addr_t)OMAP44XX_MAP_CORTEXA9_WUGEN);
    omap44xx_cortexa9_wugen_aux_core_boot_0_cpu1_status_wrf(&wugen, 0x2);
    volatile uint32_t *ap_wait = (uint32_t*)local_phys_to_mem(AP_WAIT_PHYS);
    /* XXX - this needs a good looking into. */
    //__sync_synchronize();
    *((volatile lvaddr_t *)ap_wait) = AP_STARTED;
}

uint32_t tsc_hz = 0;
uint32_t sys_clk;

static lvaddr_t ckgen_cm1_base= 0;
static omap44xx_ckgen_cm1_t ckgen_cm1;
static lvaddr_t ckgen_prm_base= 0;
static omap44xx_ckgen_prm_t ckgen_prm;

#define IN_MHZ(f) ((f) / 1000 / 1000)
#define KHZ_DIGIT(f) (((f) / 1000 / 100) % 10)

void
a9_probe_tsc(void) {
    /* Read the base clock frequency, SYS_CLK. */
    ckgen_prm_base=
        paging_map_device(OMAP44XX_MAP_L4_CKGEN_PRM,
                          OMAP44XX_MAP_L4_CKGEN_PRM_SIZE);
    omap44xx_ckgen_prm_initialize(&ckgen_prm,
            (mackerel_addr_t)ckgen_prm_base);

    switch(omap44xx_ckgen_prm_cm_sys_clksel_sys_clksel_rdf(&ckgen_prm)) {
        case omap44xx_ckgen_prm_SYS_CLKSEL_0:
            panic("sys_clksel_status is uninitialised.\n");

        case omap44xx_ckgen_prm_SYS_CLKSEL_1:
            sys_clk= 12000 * 1000; /* 12MHz */
            break;
        case omap44xx_ckgen_prm_SYS_CLKSEL_3:
            sys_clk= 16800 * 1000; /* 16.8MHz */
            break;
        case omap44xx_ckgen_prm_SYS_CLKSEL_4:
            sys_clk= 19200 * 1000; /* 19.2MHz */
            break;
        case omap44xx_ckgen_prm_SYS_CLKSEL_5:
            sys_clk= 26000 * 1000; /* 26MHz */
            break;
        case omap44xx_ckgen_prm_SYS_CLKSEL_7:
            sys_clk= 38400 * 1000; /* 38.4MHz */
            break;

        default:
            panic("sys_clksel_status is invalid.\n");
    }

    MSG("SYS_CLK is %"PRIu32".%01"PRIu32"MHz\n",
        IN_MHZ(sys_clk), KHZ_DIGIT(sys_clk));

    /* This is the main clock generator. */
    ckgen_cm1_base=
        paging_map_device(OMAP44XX_MAP_L4_CKGEN_CM1,
                          OMAP44XX_MAP_L4_CKGEN_CM1_SIZE);
    omap44xx_ckgen_cm1_initialize(&ckgen_cm1,
            (mackerel_addr_t)ckgen_cm1_base);

    if(omap44xx_ckgen_cm1_cm_clkmode_dpll_mpu_dpll_en_rdf(&ckgen_cm1)
            != omap44xx_ckgen_cm1_DPLL_EN_LM) {
        panic("MPU DPLL seems to be disabled.\n");
    }

    uint32_t mult= /* This is M */
        omap44xx_ckgen_cm1_cm_clksel_dpll_mpu_dpll_mult_rdf(&ckgen_cm1);
    uint32_t divisor = /* This is N+1 */
        omap44xx_ckgen_cm1_cm_clksel_dpll_mpu_dpll_div_rdf(&ckgen_cm1) + 1;

    MSG("M = %"PRIx32", N = %"PRIx32"\n", mult, divisor - 1);

    uint64_t f_dpll= ((uint64_t)sys_clk * 2 * mult) / divisor;

    MSG("f_dpll = %"PRIu64"\n", f_dpll);

    /* See TI SWPU235AB Figures 3-40 and 3-50. */
    bool dcc_en=
        omap44xx_ckgen_cm1_cm_clksel_dpll_mpu_dcc_en_rdf(&ckgen_cm1);
    uint32_t f_cpu;
    if(dcc_en) {
        MSG("MPU DPLL is in >=1GHz mode.\n");

        /* In high-speed mode, the divisor is hardcoded to 1, and an
         * additional divide-by-two stage is bypassed. */
        assert(f_dpll <= (uint64_t)UINT32_MAX);
        f_cpu= (uint32_t)f_dpll;
    }
    else {
        MSG("MPU DPLL is in <1GHz mode.\n");

        /* Otherwise, f_dpll is divided by two, and then by M2, the
         * post-oscillator divider. */
        int m2=
            omap44xx_ckgen_cm1_cm_div_m2_dpll_mpu_dpll_clkout_div_rdf(&ckgen_cm1);
        MSG("m2 = %d\n", m2);
        assert(m2 != 0);
        f_cpu= (f_dpll / 2) / m2;
    }

    /* The CPU clock is divided once more to generate the peripheral clock. */
    uint32_t f_periph= f_cpu / 2;

    MSG("CPU frequency %"PRIu32".%01"PRIu32"MHz, "
        "PERIPHCLK %"PRIu32".%01"PRIu32"MHz\n",
        IN_MHZ(f_cpu), KHZ_DIGIT(f_cpu),
        IN_MHZ(f_periph), KHZ_DIGIT(f_periph));

    tsc_hz= f_periph;
}
