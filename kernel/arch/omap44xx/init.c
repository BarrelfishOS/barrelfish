/*
 * Copyright (c) 2009-2013, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

/**
 * \file
 * \brief CPU driver init code for the OMAP44xx series SoCs.
 * interface found in /kernel/include/serial.h
 */

#include <kernel.h>
#include <string.h>
#include <init.h>
#include <exceptions.h>
#include <exec.h>
#include <offsets.h>
#include <paging_kernel_arch.h>
#include <phys_mmap.h>
#include <serial.h>
#include <stdio.h>
#include <arm_hal.h>
#include <getopt/getopt.h>
#include <cp15.h>
#include <elf/elf.h>
#include <arm_core_data.h>
#include <startup_arch.h>
#include <kernel_multiboot.h>
#include <global.h>

#include <omap44xx_map.h>
#include <dev/omap/omap44xx_id_dev.h>
#include <dev/omap/omap44xx_emif_dev.h>
#include <dev/omap/omap44xx_gpio_dev.h>

/// Round up n to the next multiple of size
#define ROUND_UP(n, size)           ((((n) + (size) - 1)) & (~((size) - 1)))

/**
 * Used to store the address of global struct passed during boot across kernel
 * relocations.
 */
//static uint32_t addr_global;
/**
 * \brief Kernel stack.
 *
 * This is the one and only kernel stack for a kernel instance.
 */
uintptr_t kernel_stack[KERNEL_STACK_SIZE / sizeof(uintptr_t)] __attribute__ ((aligned(8)));

/**
 * Boot-up L1 page table for addresses up to 2GB (translated by TTBR0)
 */
//XXX: We reserve double the space needed to be able to align the pagetable
//	   to 16K after relocation
static union arm_l1_entry boot_l1_low[2 * ARM_L1_MAX_ENTRIES] __attribute__ ((aligned(ARM_L1_ALIGN)));
static union arm_l1_entry * aligned_boot_l1_low;
/**
 * Boot-up L1 page table for addresses >=2GB (translated by TTBR1)
 */
//XXX: We reserve double the space needed to be able to align the pagetable
//	   to 16K after relocation
static union arm_l1_entry boot_l1_high[2 * ARM_L1_MAX_ENTRIES] __attribute__ ((aligned(ARM_L1_ALIGN)));
static union arm_l1_entry * aligned_boot_l1_high;

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define CONSTRAIN(x, a, b) MIN(MAX(x, a), b)

//
// Kernel command line variables and binding options
//

static int timeslice = 5;  //interval in ms in which the scheduler gets called

static struct cmdarg cmdargs[] = {
    {
        "consolePort",
        ArgType_UInt,
        {
            .uinteger = &serial_console_port
        }
    },
    {
        "debugPort",
        ArgType_UInt,
        {
            .uinteger = &serial_debug_port
        }
    },
    {
        "loglevel",
        ArgType_Int,
        {
            .integer = &kernel_loglevel
        }
    },
    {
        "logmask",
        ArgType_Int,
        {
            .integer = &kernel_log_subsystem_mask
        }
    },
    {
        "timeslice",
        ArgType_Int,
        {
            .integer = &timeslice
        }
    },
    {
        NULL,
        0,
        {
            NULL
        }
    }
};

static inline void __attribute__ ((always_inline))
relocate_stack(lvaddr_t offset)
{
    __asm volatile (
            "add	sp, sp, %[offset]\n\t" ::[offset] "r" (offset)
    );
}

static inline void __attribute__ ((always_inline))
relocate_got_base(lvaddr_t offset)
{
    __asm volatile (
            "add	r10, r10, %[offset]\n\t" ::[offset] "r" (offset)
    );
}

#ifndef __gem5__
static void enable_cycle_counter_user_access(void)
{
    /* enable user-mode access to the performance counter*/
    __asm volatile ("mcr p15, 0, %0, C9, C14, 0\n\t" :: "r"(1));

    /* disable counter overflow interrupts (just in case)*/
    __asm volatile ("mcr p15, 0, %0, C9, C14, 2\n\t" :: "r"(0x8000000f));
}
#endif

extern void paging_map_device_section(uintptr_t ttbase, lvaddr_t va,
        lpaddr_t pa);

static void paging_init(void)
{
    // configure system to use TTBR1 for VAs >= 2GB
    uint32_t ttbcr;
    ttbcr = cp15_read_ttbcr();
    ttbcr |= 1;
    cp15_write_ttbcr(ttbcr);

    // make sure pagetables are aligned to 16K
    aligned_boot_l1_low =
            (union arm_l1_entry *) ROUND_UP((uintptr_t)boot_l1_low, ARM_L1_ALIGN);
    aligned_boot_l1_high =
            (union arm_l1_entry *) ROUND_UP((uintptr_t)boot_l1_high, ARM_L1_ALIGN);

    lvaddr_t vbase = MEMORY_OFFSET, base = 0;

    for (size_t i = 0; i < ARM_L1_MAX_ENTRIES / 2; i++, base +=
            ARM_L1_SECTION_BYTES, vbase += ARM_L1_SECTION_BYTES) {
        // create 1:1 mapping
        //		paging_map_kernel_section((uintptr_t)aligned_boot_l1_low, base, base);
        paging_map_device_section((uintptr_t) aligned_boot_l1_low, base, base);

        // Alias the same region at MEMORY_OFFSET (gem5 code)
        // create 1:1 mapping for pandaboard
        //		paging_map_device_section((uintptr_t)boot_l1_high, vbase, vbase);
        /* if(vbase < 0xc0000000) */
        paging_map_device_section((uintptr_t) aligned_boot_l1_high, vbase,
                vbase);
    }

    // Activate new page tables
    cp15_write_ttbr1((lpaddr_t) aligned_boot_l1_high);
    cp15_write_ttbr0((lpaddr_t) aligned_boot_l1_low);
}

void kernel_startup_early(void)
{
    const char *cmdline;
    assert(glbl_core_data != NULL);
    cmdline = MBADDR_ASSTRING(glbl_core_data->cmdline);
    parse_commandline(cmdline, cmdargs);
    timeslice = CONSTRAIN(timeslice, 1, 20);
}

#define KERNEL_DEBUG_USB 0

/*
 * initialize the USB functionality of the pandaboard
 */
static void hsusb_init(void)
{
    printf("  > hsusb_init()...\n");

    /*
     * Global Initialization of the OMAPP44xx USB Sub System
     */
    printf("  >  > USB TTL reset...");

    /*
     * Reset USBTTL
     * USBTLL_SYSCONFIG = 0x2
     */
    *((volatile uint32_t*) (0x4A062010)) = (uint32_t) (0x1 << 1);

    /*
     * wait till reset is done
     */
    while (!((*((volatile uint32_t*) (0x4A062014))) & 0x1)) {
        printf("%c", 0xE);
    }
    /*
     * USBTLL_SYSCONFIG
     *  - Setting ENAWAKEUP
     *  - Setting SIDLEMODE
     *  - Setting CLOCKACTIVITY
     */
    *((volatile uint32_t*) (0x4A062010)) = (uint32_t) ((0x1 << 2) | (0x1 << 3)
            | (0x1 << 8));

    printf("OK\n");

    /*
     * USBTLL_IRQENABLE:
     *  - all interrupts
     */
    *((volatile uint32_t*) (0x4A06201C)) = (uint32_t) (0x7);

    printf("  >  > USB host controller reset...");

    /*
     * per form a reset on the USB host controller module
     * this resets both EHCI and OCHI controllers
     *
     * UHH_SYSCONFIG = 0x1
     */
    *((volatile uint32_t*) (0x4A064010)) = (uint32_t) (0x1);

    /*
     * wait till reset is done
     * UHH_SYSSTATUS = 0x6
     */
    while (((*((volatile uint32_t*) (0x4A064014))) & 0x6) != 0x6) {
        printf("%c", 0xE);
    }

    /* enable some USB host features
     * UHH_SYSCONFIG
     *  - STANDBYMODE
     *  - IDLEMODE
     */
    *((volatile uint32_t*) (0x4A064010)) = (uint32_t) ((0x1 << 2) | (0x1 << 4));

    printf("OK\n");

    printf("  >  > Setting USB host configuration values...");

    /*
     * setting the host configuration to external PHY and enable
     * the burst types, app start clk
     *
     * UHH_HOSTCONFIG
     *  - APP_START_CLK
     *  - ENAINCR_x
     */
    *((volatile uint32_t*) (0x4A064040)) =
            (uint32_t) ((0x7 << 2) | (0x1 << 31));

    printf("OK\n");

    printf("  > done.\n");
}

// GPIO numbers for enabling the USB hub on the pandaboard
#define HSUSB_HUB_POWER 1
#define HSUSB_HUB_RESET 30

#define SCRM_AUXCLK3 0x4A30A31C
#define PAD0_FREF_CLK3_OUT 0x4A31E058
#define PAD1_KPD_COL2 0x4A100184
#define PAD0_GPMC_WAIT1  0x4A10008C

/*
 * Initialize the high speed usb hub on the pandaboard
 */
static void usb_power_on(void)
{
    printf("usb_power_on()... \n");

    // mackerel device state variables
    omap44xx_gpio_t g1;
    omap44xx_gpio_t g2;

    /*
     * mackerel device intialization for GPIO1 and GPIO2
     */
    omap44xx_gpio_initialize(&g1, (mackerel_addr_t) OMAP44XX_MAP_L4_WKUP_GPIO1);
    omap44xx_gpio_initialize(&g2, (mackerel_addr_t) OMAP44XX_MAP_L4_PER_GPIO2);

    printf("  > forward the AUXCLK3 to GPIO_WK31\n");
    /*
     * the USB hub needs the FREF_CLK3_OUT to be 19.2 MHz and that this
     * clock goes to the GPIO_WK31 out.
     * Assume that the sys clock is 38.4 MHz so we use a divider of 2
     *
     * Bit  8: is the enable bit
     * Bit 16: is the divider bit (here for two)
     */
    *((volatile uint32_t*) (SCRM_AUXCLK3)) = (uint32_t) ((1 << 16) | (1 << 8));

    /*
     * Forward the clock to the GPIO_WK31 pin
     *  - muxmode = fref_clk3_out (0x0)
     *  - no pullup/down (0x0)
     *  - no input buffer (0x0)
     *  - no wake up (0x0)
     */
    *((volatile uint32_t*) (PAD0_FREF_CLK3_OUT)) = (uint32_t) (0x0000);

    printf("  > reset external USB hub and PHY\n");

    /*
     * Perform a reset on the USB hub i.e. drive the GPIO_1 pin to low
     * and enable the dataout for the this pin in GPIO
     */uint32_t gpoi_1_oe = omap44xx_gpio_oe_rd(&g1)
            & (~(1UL << HSUSB_HUB_POWER));
    omap44xx_gpio_oe_wr(&g1, gpoi_1_oe);

    omap44xx_gpio_cleardataout_wr(&g1, (1UL << HSUSB_HUB_POWER));

    /*
     * forward the data outline to the USB hub by muxing the
     * CONTROL_CORE_PAD0_KPD_COL1_PAD1_KPD_COL2 into mode 3 (gpio_1)
     */
    *((volatile uint32_t*) (PAD1_KPD_COL2)) = (uint32_t) (0x0003 << 16);

    /*
     * Perform a reset on the USB phy i.e. drive GPIO_62 to low
     *
     * HSUSB_HUB_RESET: 0 = Hub & Phy held in reset     1 = Normal operation.
     */uint32_t gpoi_2_oe = omap44xx_gpio_oe_rd(&g2)
            & (~(1UL << HSUSB_HUB_RESET));
    omap44xx_gpio_oe_wr(&g2, gpoi_2_oe);

    omap44xx_gpio_cleardataout_wr(&g2, (1UL << HSUSB_HUB_RESET));

    /*
     * forward the data on gpio_62 pin to the output by muxing
     *  CONTROL_CORE_PAD0_GPMC_WAIT1_PAD1_GPMC_WAIT2 to mode 0x3
     */
    *((volatile uint32_t*) (PAD0_GPMC_WAIT1)) = (uint32_t) (0x0003);

    /* delay to give the hardware time to reset TODO: propper delay*/
    for (int j = 0; j < 4000; j++) {
        printf("%c", 0xE);
    }

    assert((*((volatile uint32_t*)(PAD0_GPMC_WAIT1))) == (uint32_t)(0x0003));
    assert(
            (*((volatile uint32_t*)(PAD1_KPD_COL2))) == (uint32_t)((0x0003)<<16));
    assert((*((volatile uint32_t*)(PAD0_FREF_CLK3_OUT))) == (uint32_t)(0x0000));

    hsusb_init();

    printf("  > enable the external USB hub and PHY\n");

    /* power on the USB subsystem */
    omap44xx_gpio_setdataout_wr(&g1, (1UL << HSUSB_HUB_POWER));


    /* enable the USB HUB */
    omap44xx_gpio_setdataout_wr(&g2, (1UL << HSUSB_HUB_RESET));

    for (int j = 0; j < 4000; j++) {
            printf("%c", 0xE);
        }

    printf("  > performing softreset on the USB PHY\n");

    assert(!(0x2 & (*((volatile uint32_t*)(0x4A310134)))));
    assert((0x2 & (*((volatile uint32_t*)(0x4A31013C)))));

    /* soft reset the pyh */
    *((volatile uint32_t*) (0x4A064CA4)) = (uint32_t) ((0x1 << 5) | (0x5 << 16)
            | (0x2 << 22) | (0x1 << 24) | (0x1 << 31));
    while (*((volatile uint32_t*) (0x4A064CA4)) & (1 << 31)) {
        printf("%c", 0xE);
    }

    try_again:
    /* wait till reset is done */
    *((volatile uint32_t*) (0x4A064CA4)) = (uint32_t) ((0x5 << 16)
                | (0x3 << 22) | (0x1 << 24) | (0x1 << 31));
        while (*((volatile uint32_t*) (0x4A064CA4)) & (1 << 31)) {
            printf("%c", 0xE);
        }
        if (*((volatile uint32_t*) (0x4A064CA4)) & (1 << 31) & (0x1<<5)) {
            /* reset is not done */
            goto try_again;
        }

    /* read the debug register */
    *((volatile uint32_t*) (0x4A064CA4)) = (uint32_t) ((0x15 << 16)
            | (0x3 << 22) | (0x1 << 24) | (0x1 << 31));
    while (*((volatile uint32_t*) (0x4A064CA4)) & (1 << 31)) {
        printf("%c", 0xE);

    }

    printf("  > ULPI line state = %s\n",
            (*((volatile uint32_t*) (0x4A064CA4))) & 0x1 ?
                    "Connected" : "Disconnected");

    /*
     * reading out the vendor ID of the ULPI receiver
     * This ID must be 0x24 according to the SMSC USB3320 data sheet
     */
    *((volatile uint32_t*) (0x4A064CA4)) = (uint32_t) ((0x00 << 16)
            | (0x3 << 22) | (0x1 << 24) | (0x1 << 31));
    while (*((volatile uint32_t*) (0x4A064CA4)) & (1 << 31)) {
        printf("%c", 0xE);
    }
    assert(0x24 == ((*((volatile uint32_t*) (0x4A064CA4))) & 0xFF));

    printf("done.\n");
}

static void prcm_init(void)
{
    printf("prcm_init()...\n");

    printf("  > CM_SYS_CLKSEL=38.4MHz \n");
    /*
     * Set the system clock to 38.4 MHz
     * CM_SYS_CLKSEL = 0x7
     */
    *((volatile uint32_t*) (0x4A306110)) = (uint32_t) (0x7);

    if (!(*((volatile uint32_t*) (0x4A306110)))) {
        printf("WARNING: Could not set SYS_CLK\n");
        return;
    }

    /* ALTCLKSRC */
    *((volatile uint32_t*) (0x4A30A110)) = (uint32_t) (0x1 | (0x3 << 2));

    printf("  > Enabling L4PER interconnect clock\n");
    /* CM_L4PER_CLKSTCTRL */
    *((volatile uint32_t*) (0x4A009400)) = (uint32_t) (0x2);

    printf("  > Enabling GPIOi clocks\n");
    /* CM_L4PER_GPIO2_CLKCTRL */
    *((volatile uint32_t*) (0x4A009460)) = (uint32_t) (0x1);
    /* CM_L4PER_GPIO3_CLKCTRL */
    *((volatile uint32_t*) (0x4A009468)) = (uint32_t) ((0x1 << 8) | (0x1));
    /* CM_L4PER_GPIO4_CLKCTRL */
    *((volatile uint32_t*) (0x4A009470)) = (uint32_t) (0x1);
    /* CM_L4PER_GPIO5_CLKCTRL */
    *((volatile uint32_t*) (0x4A009478)) = (uint32_t) (0x1);
    /* CM_L4PER_GPIO6_CLKCTRL */
    *((volatile uint32_t*) (0x4A009480)) = (uint32_t) (0x1);
    /* CM_L4PER_HDQ1W_CLKCTRL */
    *((volatile uint32_t*) (0x4A009488)) = (uint32_t) (0x2);
    /* CM_WKUP_GPIO1_CLKCTRL */
    *((volatile uint32_t*) (0x4A008E00)) = (uint32_t) (0x1);
#if KERNEL_DEBUG_USB
    printf("/* CM_L4PER_PIO2_CLKCTRL */ %p \n",
            *((volatile uint32_t*) (0x4A009460)));
    printf("/* CM_L4PER_GPIO3_CLKCTRL */ %p \n",
            *((volatile uint32_t*) (0x4A009468)));
    printf("/* CM_L4PER_GPIO4_CLKCTRL */ %p \n",
            *((volatile uint32_t*) (0x4A009470)));
    printf("/* CM_L4PER_GPIO5_CLKCTRL */ %p \n",
            *((volatile uint32_t*) (0x4A009478)));
    printf("/* CM_L4PER_GPIO6_CLKCTRL */ %p \n",
            *((volatile uint32_t*) (0x4A009480)));
    printf("/* CM_L4PER_HDQ1W_CLKCTRL */ %p \n",
            *((volatile uint32_t*) (0x4A009488)));
    printf("/* CM_WKUP_GPIO1_CLKCTRL */ %p \n",
            *((volatile uint32_t*) (0x4A008E00)));
#endif
    printf("  > Enabling L3INIT USB clocks\n");
    /* CM_L3INIT_HSI_CLKCTRL */
    *((volatile uint32_t*) (0x4A009338)) = (uint32_t) (0x1);
    /* CM_L3INIT_HSUSBHOST_CLKCTRL */
    *((volatile uint32_t*) (0x4A009358)) = (uint32_t) (0x2 | (0xFF00)
            | 0x3 << 24);
    /* CM_L3INIT_HSUSBOTG_CLKCTRL */
    *((volatile uint32_t*) (0x4A009360)) = (uint32_t) (0x1);
    /* CM_L3INIT_HSUSBTLL_CLKCTRL */
    *((volatile uint32_t*) (0x4A009368)) = (uint32_t) ((0x1 << 8) | (0x1 << 9)
            | 0x1);
    /* CM_L3INIT_FSUSB_CLKCTRL */
    *((volatile uint32_t*) (0x4A0093D0)) = (uint32_t) (0x2);
    /* CM_L3INIT_USBPHY_CLKCTRL */
    *((volatile uint32_t*) (0x4A0093E0)) = (uint32_t) (0x301);
#if KERNEL_DEBUG_USB
    printf("/* CM_L3INIT_HSI_CLKCTRL */%p \n",
            *((volatile uint32_t*) (0x4A009338)));
    printf("/* CM_L3INIT_HSUSBHOST_CLKCTRL */%p \n",
            *((volatile uint32_t*) (0x4A009358)));
    printf("/* CM_L3INIT_HSUSBOTG_CLKCTRL */%p \n",
            *((volatile uint32_t*) (0x4A009360)));
    printf("/* CM_L3INIT_HSUSBTLL_CLKCTRL */%p \n",
            *((volatile uint32_t*) (0x4A009368)));
    printf("/* CM_L3INIT_FSUSB_CLKCTRL */%p \n",
            *((volatile uint32_t*) (0x4A0093D0)));
    printf("/* CM_L3INIT_USBPHY_CLKCTRL */%p \n",
            *((volatile uint32_t*) (0x4A0093E0)));
#endif
    printf("done.\n");
}

#define M0              0
#define M3              3
#define M4              4

#define OMAP_PIN_OUTPUT                 0
#define OMAP_INPUT_EN                  (1 << 8)
#define OMAP_PULL_ENA                  (1 << 3)
#define OMAP_PIN_INPUT_PULLDOWN        (OMAP_PULL_ENA | OMAP_INPUT_EN)

static void set_muxconf_regs(void)
{
    printf("set_muxconf_regs()...");
    uint16_t mux_1, mux_2;

    /* CONTROL_PADCONF_CORE_SYSCONFIG */
    *((volatile uint32_t*) (0x4A100010)) = (uint32_t) (0x1 << 2);
    /* CONTROL_PADCONF_WKUP_SYSCONFIG */
    *((volatile uint32_t*) (0x4A31E010)) = (uint32_t) (0x1 << 2);
    /* CONTROL_GEN_CORE_SYSCONFIG */
    *((volatile uint32_t*) (0x4A002010)) = (uint32_t) (0x1 << 2);
    /* CONTROL_GEN_WKUP_SYSCONFIG */
    *((volatile uint32_t*) (0x4A30C010)) = (uint32_t) (0x1 << 2);

    /* USBB1_ULPITLL_CLK */
    mux_1 = OMAP_PIN_INPUT_PULLDOWN | M4;
    mux_2 = *((volatile uint32_t*) (0x4A1000C0)) & 0xFFFF;
    *((volatile uint32_t*) (0x4A1000C0)) = (uint32_t) (mux_1 << 16 | mux_2);

    /* USBB1_ULPITLL_DIR */
    mux_1 = OMAP_PIN_INPUT_PULLDOWN | M4;
    /* USBB1_ULPITLL_STP */
    mux_2 = OMAP_PIN_OUTPUT | M4;
    *((volatile uint32_t*) (0x4A1000C4)) = (uint32_t) (mux_1 << 16 | mux_2);

    /* USBB1_ULPITLL_NXT */
    mux_1 = OMAP_PIN_INPUT_PULLDOWN | M4;
    /* USBB1_ULPITLL_DAT0 */
    mux_2 = OMAP_PIN_INPUT_PULLDOWN | M4;
    *((volatile uint32_t*) (0x4A1000C8)) = (uint32_t) (mux_1 << 16 | mux_2);

    /* USBB1_ULPITLL_DAT1 */
    mux_1 = OMAP_PIN_INPUT_PULLDOWN | M4;
    /* USBB1_ULPITLL_DAT2 */
    mux_2 = OMAP_PIN_INPUT_PULLDOWN | M4;
    *((volatile uint32_t*) (0x4A1000CC)) = (uint32_t) (mux_1 << 16 | mux_2);

    /* USBB1_ULPITLL_DAT3 */
    mux_1 = OMAP_PIN_INPUT_PULLDOWN | M4;
    /* USBB1_ULPITLL_DAT4 */
    mux_2 = OMAP_PIN_INPUT_PULLDOWN | M4;
    *((volatile uint32_t*) (0x4A1000D0)) = (uint32_t) (mux_1 << 16 | mux_2);

    /* USBB1_ULPITLL_DAT5 */
    mux_1 = OMAP_PIN_INPUT_PULLDOWN | M4;
    /* USBB1_ULPITLL_DAT6 */
    mux_2 = OMAP_PIN_INPUT_PULLDOWN | M4;
    *((volatile uint32_t*) (0x4A1000D4)) = (uint32_t) (mux_1 << 16 | mux_2);

    /* USBB1_ULPITLL_DAT7 */
    mux_2 = OMAP_PIN_INPUT_PULLDOWN | M4;
    mux_1 = (*((volatile uint32_t*) (0x4A1000C0)) >> 16) & 0xFFFF;
    *((volatile uint32_t*) (0x4A1000D8)) = (uint32_t) (mux_1 << 16 | mux_2);

#if KERNEL_DEBUG_USB
    printf("/* USBB1_ULPITLL_CLK */ %p\n",
            *((volatile uint32_t*) (0x4A1000C0)));

    printf("/* USBB1_ULPITLL_DIR */ %p\n",
            *((volatile uint32_t*) (0x4A1000C4)));

    printf("/* USBB1_ULPITLL_NXT */ %p\n",
            *((volatile uint32_t*) (0x4A1000C8)));

    printf("/* USBB1_ULPITLL_DAT1 */ %p\n",
            *((volatile uint32_t*) (0x4A1000CC)));

    printf("/* USBB1_ULPITLL_DAT3 */ %p\n",
            *((volatile uint32_t*) (0x4A1000D0)));

    printf("/* USBB1_ULPITLL_DAT5 */ %p\n",
            *((volatile uint32_t*) (0x4A1000D4)));

    printf("/* USBB1_ULPITLL_DAT7 */ %p\n",
            *((volatile uint32_t*) (0x4A1000D8)));
#endif
    printf("done\n");
}

/**
 * \brief Continue kernel initialization in kernel address space.
 *
 * This function resets paging to map out low memory and map in physical
 * address space, relocating all remaining data structures. It sets up exception handling,
 * initializes devices and enables interrupts. After that it
 * calls arm_kernel_startup(), which should not return (if it does, this function
 * halts the kernel).
 */
static void __attribute__ ((noinline,noreturn)) text_init(void)
{
    errval_t errval;



    // Map-out low memory
    if (glbl_core_data->multiboot_flags & MULTIBOOT_INFO_FLAG_HAS_MMAP) {

        struct arm_coredata_mmap *mmap =
                (struct arm_coredata_mmap *) local_phys_to_mem(
                        glbl_core_data->mmap_addr);

        paging_arm_reset(mmap->base_addr, mmap->length);
        printf("paging_arm_reset: base: 0x%"PRIx64", length: 0x%"PRIx64".\n",
                mmap->base_addr, mmap->length);
    } else {
        panic("need multiboot MMAP\n");
    }

    exceptions_init();

    printf("invalidate cache\n");
    cp15_invalidate_i_and_d_caches_fast();

    printf("invalidate TLB\n");
    cp15_invalidate_tlb();

    printf("startup_early\n");

    kernel_startup_early();
    printf("kernel_startup_early done!\n");

    //initialize console
    serial_init(serial_console_port);

    printf("Barrelfish CPU driver starting on ARMv7 OMAP44xx"
            " Board id 0x%08"PRIx32"\n", hal_get_board_id());
    printf("The address of paging_map_kernel_section is %p\n",
            paging_map_kernel_section);

    errval = serial_debug_init();
    if (err_is_fail(errval)) {
        printf("Failed to initialize debug port: %d", serial_debug_port);
    }

    my_core_id = hal_get_cpu_id();
    printf("cpu id %d\n", my_core_id);

    // Test MMU by remapping the device identifier and reading it using a
    // virtual address
    lpaddr_t id_code_section = OMAP44XX_MAP_L4_CFG_SYSCTRL_GENERAL_CORE
            & ~ARM_L1_SECTION_MASK;
    lvaddr_t id_code_remapped = paging_map_device(id_code_section,
            ARM_L1_SECTION_BYTES);
    omap44xx_id_t id;
    omap44xx_id_initialize(&id,
            (mackerel_addr_t) (id_code_remapped
                    + (OMAP44XX_MAP_L4_CFG_SYSCTRL_GENERAL_CORE
                            & ARM_L1_SECTION_MASK)));

    char buf[200];
    omap44xx_id_code_pr(buf, 200, &id);
    printf("Using MMU, %s", buf);

    gic_init();
    printf("gic_init done\n");

    if (hal_cpu_is_bsp()) {

        scu_initialize();
        uint32_t omap_num_cores = scu_get_core_count();
        printf("Number of cores in system: %"PRIu32"\n", omap_num_cores);

        // ARM Cortex A9 TRM section 2.1
        if (omap_num_cores > 4)
            panic("ARM SCU doesn't support more than 4 cores!");

        // init SCU if more than one core present
        if (omap_num_cores > 1) {
            scu_enable();
        }
    }

    tsc_init();
    printf("tsc_init done --\n");
#ifndef __gem5__
    enable_cycle_counter_user_access();
    reset_cycle_counter();
#endif



    // tell BSP that we are started up
    // XXX NYI: See Section 27.4.4 in the OMAP44xx manual for how this
    // should work.

    arm_kernel_startup();
}

/**
 * Use Mackerel to print the identification from the system
 * configuration block.
 */
static void print_system_identification(void)
{
    char buf[800];
    omap44xx_id_t id;
    omap44xx_id_initialize(&id,
            (mackerel_addr_t) OMAP44XX_MAP_L4_CFG_SYSCTRL_GENERAL_CORE);
    omap44xx_id_pr(buf, 799, &id);
    printf("%s", buf);
    omap44xx_id_codevals_prtval(buf, 799, omap44xx_id_code_rawrd(&id));
    printf("Device is a %s\n", buf);
}

static size_t bank_size(int bank, lpaddr_t base)
{
    int rowbits;
    int colbits;
    int rowsize;
    omap44xx_emif_t emif;
    omap44xx_emif_initialize(&emif, (mackerel_addr_t) base);
    if (omap44xx_emif_status_phy_dll_ready_rdf(&emif)) {
        rowbits = omap44xx_emif_sdram_config_rowsize_rdf(&emif) + 9;
        colbits = omap44xx_emif_sdram_config_pagesize_rdf(&emif) + 9;
        rowsize = omap44xx_emif_sdram_config2_rdbsize_rdf(&emif) + 5;
        printf("EMIF%d: ready, %d-bit rows, %d-bit cols, %d-byte row buffer\n",
                bank, rowbits, colbits, 1 << rowsize);
        return (1 << (rowbits + colbits + rowsize));
    } else {
        printf("EMIF%d doesn't seem to have any DDRAM attached.\n", bank);
        return 0;
    }
}

static void size_ram(void)
{
    size_t sz = 0;
    sz = bank_size(1, OMAP44XX_MAP_EMIF1) + bank_size(2, OMAP44XX_MAP_EMIF2);
    printf("We seem to have 0x%08lx bytes of DDRAM: that's %s.\n", sz,
            sz == 0x40000000 ? "about right" : "unexpected");
}

/*
 * Does work for both LEDs now.
 */
static void set_leds(void)
{
    omap44xx_gpio_t g;
    uint32_t r, nr;

    printf("Flashing LEDs\n");

    omap44xx_gpio_initialize(&g, (mackerel_addr_t) OMAP44XX_MAP_L4_WKUP_GPIO1);
    // Output enable
    r = omap44xx_gpio_oe_rd(&g) & (~(1 << 8));
    omap44xx_gpio_oe_wr(&g, r);
    // Write data out
    r = omap44xx_gpio_dataout_rd(&g) & (~(1 << 8));
    nr = r | (1 << 8);
    for (int i = 0; i < 5; i++) {
        omap44xx_gpio_dataout_wr(&g, r);
        for (int j = 0; j < 2000; j++) {
            printf("%c", 0xE);
        }
        omap44xx_gpio_dataout_wr(&g, nr);
        for (int j = 0; j < 2000; j++) {
            printf("%c", 0xE);
        }
    }

    omap44xx_gpio_initialize(&g, (mackerel_addr_t) OMAP44XX_MAP_L4_PER_GPIO4);

    /*
     * TODO: write as mackerel
     */
    volatile uint32_t *pad_mux = (uint32_t *) 0x4A1000F4;
    *pad_mux = ((*pad_mux) & ~(0x7 << 16)) | (0x3 << 16);

    // Output enable
    r = omap44xx_gpio_oe_rd(&g) & (~(1 << 14));
    omap44xx_gpio_oe_wr(&g, r);
    // Write data out
    r = omap44xx_gpio_dataout_rd(&g);
    nr = r | (1 << 14);
    for (int i = 0; i < 5; i++) {
        omap44xx_gpio_dataout_wr(&g, r);
        for (int j = 0; j < 2000; j++) {
            printf("%c", 0xE);
        }
        omap44xx_gpio_dataout_wr(&g, nr);
        for (int j = 0; j < 2000; j++) {
            printf("%c", 0xE);
        }
    }
}

/**
 * Entry point called from boot.S for bootstrap processor.
 * if is_bsp == true, then pointer points to multiboot_info
 * else pointer points to a global struct
 */
void arch_init(void *pointer)
{
    struct arm_coredata_elf *elf = NULL;

    serial_early_init(serial_console_port);

    if (hal_cpu_is_bsp()) {
        struct multiboot_info *mb = (struct multiboot_info *) pointer;
        elf = (struct arm_coredata_elf *) &mb->syms.elf;
        memset(glbl_core_data, 0, sizeof(struct arm_core_data));
        glbl_core_data->start_free_ram =
                ROUND_UP(max(multiboot_end_addr(mb), (uintptr_t)&kernel_final_byte),
                        BASE_PAGE_SIZE);

        glbl_core_data->mods_addr = mb->mods_addr;
        glbl_core_data->mods_count = mb->mods_count;
        glbl_core_data->cmdline = mb->cmdline;
        glbl_core_data->mmap_length = mb->mmap_length;
        glbl_core_data->mmap_addr = mb->mmap_addr;
        glbl_core_data->multiboot_flags = mb->flags;

        memset(&global->locks, 0, sizeof(global->locks));

        /*
         * pandaboard related USB setup
         */
        prcm_init();
        set_muxconf_regs();
        usb_power_on();

    } else {
        global = (struct global *) GLOBAL_VBASE;
        memset(&global->locks, 0, sizeof(global->locks));
        struct arm_core_data *core_data =
                (struct arm_core_data*) ((lvaddr_t) &kernel_first_byte
                        - BASE_PAGE_SIZE);
        glbl_core_data = core_data;
        glbl_core_data->cmdline = (lpaddr_t) &core_data->kernel_cmdline;
        my_core_id = core_data->dst_core_id;
        elf = &core_data->elf;
    }

    // XXX: print kernel address for debugging with gdb
    printf("Barrelfish OMAP44xx CPU driver starting at addr 0x%"PRIxLVADDR"\n",
            local_phys_to_mem((uint32_t) &kernel_first_byte));



    print_system_identification();
    size_ram();

    if (1) {
        set_leds();
    }



    paging_init();
    cp15_enable_mmu();
    printf("MMU enabled\n");

    text_init();
}
