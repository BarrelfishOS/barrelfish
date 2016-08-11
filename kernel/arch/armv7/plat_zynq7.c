/**
 * \file
 * \brief Platform code for the Xilinx Zynq7000-series SoCs
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <a9_gt.h>
#include <a9_scu.h>
#include <a9mpcore_map.h>
#include <assert.h>
#include <cp15.h>
#include <dev/cortex_a9_pit_dev.h>
#include <dev/zynq7/zynq_slcr_dev.h>
#include <errors/errno.h>
#include <gic.h>
#include <global.h>
#include <init.h>
#include <paging_kernel_arch.h>
#include <platform.h>
#include <serial.h>
#include <zynq7_map.h>
#include <zynq_uart.h>

#define MSG(format, ...) printk( LOG_NOTE, "ZYNQ7: "format, ## __VA_ARGS__ )

/*****************************************************************************
 *
 * Implementation of serial.h
 *
 *****************************************************************************/

/*
 * Initialize the serial ports
 */
errval_t
serial_init(unsigned port, bool initialize_hw) {
    assert(paging_mmu_enabled());
    assert(port < serial_num_physical_ports);
    lvaddr_t base = paging_map_device(uart_base[port], uart_size[port]);
    zynq_uart_init(port, base, initialize_hw);
    return SYS_ERR_OK;
};

/* System control registers, after MMU initialisation. */
static lvaddr_t slcr_base= 0;
static zynq_slcr_t slcr_dev;

/* XXX - centralise platform device management on ARMv7. */
static zynq_slcr_t *
get_slcr(void) {
    /* If it's not yet mapped, do so. */
    if(slcr_base == 0) {
        slcr_base= paging_map_device(ZINQ7_SYS_CTRL_BASEADDR, BASE_PAGE_SIZE);
        zynq_slcr_initialize(&slcr_dev, (mackerel_addr_t)slcr_base);
    }

    return &slcr_dev;
}

/* Print system identification. MMU is NOT yet enabled. */
void
platform_print_id(void) {
    assert(!paging_mmu_enabled());

    zynq_slcr_t slcr_early;
    zynq_slcr_initialize(&slcr_early,
                         (mackerel_addr_t)ZINQ7_SYS_CTRL_BASEADDR);

#if 0
    int family=       zynq_slcr_PSS_IDCODE_FAMILY_rdf(&slcr);
    int subfamily=    zynq_slcr_PSS_IDCODE_SUBFAMILY_rdf(&slcr);
    int manufacturer= zynq_slcr_PSS_IDCODE_MANUFACTURER_ID_rdf(&slcr);
    zynq_slcr_devcode_t device= zynq_slcr_PSS_IDCODE_DEVICE_CODE_rdf(&slcr);

    /* See Zynq 7000 TRM p1631. */
    if(family != 0x1b || subfamily != 0x9 || manufacturer != 0x49) {
        panic("This doesn't look like a Zynq\n");
    }
#endif

    char buf[1024];
    zynq_slcr_PSS_IDCODE_pr(buf, 1023, &slcr_early);

    printf("This is a Zynq.\n");
    printf("%s", buf);
}

void
platform_get_info(struct platform_info *pi) {
    pi->arch     = PI_ARCH_ARMV7A;
    pi->platform = PI_PLATFORM_ZYNQ7;
    armv7_get_info(&pi->arch_info.armv7);
}

/* The zc706 has 2GB of RAM beginning at address 0. */
size_t
platform_get_ram_size(void) {
    return (ZYNQ7_DDR_MEM_HIGHADDR - ZYNQ7_DDR_MEM_BASEADDR) + 1;
}

/**
 * Notify the BSP that this AP has booted. 
 */

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
int
platform_boot_aps(coreid_t core_id, genvaddr_t gen_entry) {
    panic("Unimplemented.\n");
    //return 0;
}

// XXX: panic() messes with GCC, remove attribute when code works again!
__attribute__((noreturn))
void
platform_notify_bsp(void) {
    panic("Unimplemented.\n");
}

/* Clocks on the Zynq processor subsystem are derived from PS_CLK, which is
 * 33.33333MHz on the zc706.  This should be command-line configurable for
 * other boards. */
uint32_t ps_clk = 33333330;
uint32_t tsc_hz = 0;

void
a9_probe_tsc(void) {
    zynq_slcr_t *slcr= get_slcr();

    /* Figure out which PLL is driving the CPU. */
    zynq_slcr_clksrc_t clksrc= zynq_slcr_ARM_CLK_CTRL_SRCSEL_rdf(slcr);
    MSG("CPU clock is derived from %s\n", zynq_slcr_clksrc_describe(clksrc));

    /* Read the appropriate control register. */
    zynq_slcr_pll_ctrl_t pll_ctrl;
    switch(clksrc) {
        case zynq_slcr_iopll0:
        case zynq_slcr_iopll1:
            pll_ctrl= zynq_slcr_IO_PLL_CTRL_rd(slcr);
            break;
        case zynq_slcr_armpll:
            pll_ctrl= zynq_slcr_ARM_PLL_CTRL_rd(slcr);
            break;
        case zynq_slcr_ddrpll:
            pll_ctrl= zynq_slcr_DDR_PLL_CTRL_rd(slcr);
            break;
        default:
            panic("Invalid PLL type.\n");
    }

    /* Pinstrap PLL bypass setting. */
    bool bypass_pin=   zynq_slcr_BOOT_MODE_PLL_BYPASS_rdf(slcr);
    /* Software bypass override (force). */
    bool bypass_force= zynq_slcr_pll_ctrl_PLL_BYPASS_FORCE_extract(pll_ctrl);
    /* Bypass control source. */
    bool bypass_src=   zynq_slcr_pll_ctrl_PLL_BYPASS_QUAL_extract(pll_ctrl);

    /* Find the PLL output frequency. */
    uint32_t src_clk;
    bool bypass= bypass_force || (bypass_src && bypass_pin);
    if(bypass) {
        MSG(" PLL is bypassed.\n");
        src_clk= ps_clk;
    }
    else {
        uint32_t M= zynq_slcr_pll_ctrl_PLL_FDIV_extract(pll_ctrl);
        MSG(" PLL multiplier, M=%"PRIu32".\n", M);
        src_clk= ps_clk * M;
    }
    MSG(" PLL frequency is %"PRIu32"kHz.\n", src_clk/1000);

    /* The CPU clock is divided again. */
    uint32_t divisor= zynq_slcr_ARM_CLK_CTRL_DIVISOR_rdf(slcr);
    uint32_t cpu_clk= src_clk / divisor;
    MSG(" CPU frequency is %"PRIu32"kHz.\n", cpu_clk/1000);

    /* The timers run at half the core frequency. */
    tsc_hz= cpu_clk / 2;
    MSG(" Timer frequency is %"PRIu32"kHz.\n", tsc_hz/1000);

    /* The next step in the clock chain, for the fast IO peripherals, can be
     * either a factor or 2, or of 3. */
    uint32_t divisor2;
    if(zynq_slcr_CLK_621_TRUE_CLK_621_TRUE_rdf(slcr)) divisor2= 3;
    else                                              divisor2= 2;

    MSG(" Central interconnect frequency is %"PRIu32"kHz.\n",
        cpu_clk / divisor2 / 1000);
    MSG(" AHB frequency is %"PRIu32"kHz.\n",
        cpu_clk / divisor2 / 2 / 1000);
}
