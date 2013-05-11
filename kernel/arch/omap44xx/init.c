/*
 * Copyright (c) 2009,2012, ETH Zurich. All rights reserved.
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
"consolePort", ArgType_UInt, {
.uinteger = &serial_console_port
}
}, {
"debugPort", ArgType_UInt, {
.uinteger = &serial_debug_port
}
}, {
"loglevel", ArgType_Int, {
.integer = &kernel_loglevel
}
}, {
"logmask", ArgType_Int, {
.integer = &kernel_log_subsystem_mask
}
}, {
"timeslice", ArgType_Int, {
.integer = &timeslice
}
}, {
NULL, 0, {
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
    //gic_init();
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

// GPIO numbers for enabling the USB hub on the pandaboard
#define HSUSB_HUB_POWER 1
#define HSUSB_HUB_RESET 30

/*
 * Initialize the high speed usb hub on the pandaboard
 */
static void usb_power_on(void)
{
    printf("usb subsystem: power on routine...\n");
    /*
     * Set the system clock to 38.4 MHz
     */
    volatile uint32_t *CM_SYS_CLKSEL = (uint32_t *) 0x4A306110;
    *CM_SYS_CLKSEL = (0x7);

    /*
     * enable the GPIO2 Clock
     */
    volatile uint32_t *CM_L4PER_GPIO2_CLKCTRL = (uint32_t *) 0x4A009460;
    *CM_L4PER_GPIO2_CLKCTRL = (0x1 << 8) | (0x1);

    /*
     * enable the auxilary clock
     */
    volatile uint32_t *auxclk3 = (uint32_t *) 0x4A30A31C;
    *auxclk3 = (0x1 << 8) | (0x1 << 16);

    /*
     * Set the output of the pin to the fref_clk3_out
     *
     * Field: CONTROL_WKUP_PAD0_FREF_CLK3_OUT_PAD1_FREF_CLK4_REQ[2:0]
     * Value: 0x0: Select fref_clk3_out
     */
    volatile uint16_t *dpad_mux3 = (uint16_t *) 0x4A31E058;
    *dpad_mux3 = 0x0000;

    // mackerel device state variables
    omap44xx_gpio_t g1;
    omap44xx_gpio_t g2;

    /*
     * mackerel device intialization for GPIO1 and GPIO2
     */
    omap44xx_gpio_initialize(&g1, (mackerel_addr_t) OMAP44XX_MAP_L4_WKUP_GPIO1);
    omap44xx_gpio_initialize(&g2, (mackerel_addr_t) OMAP44XX_MAP_L4_PER_GPIO2);

    /*
     * HUB POWER CONTROL
     *
     * HSUSB_HUB_POWER:  1 = Enable Hub LDO,     0 = Power Down Hub LDO
     *
     * Enable to the Hub 3.3V LDO @ U9 (note: this GPIO goes through a voltage
     * translator to provide adequate Vih margin on the LDO enable input).
     */

    /*
     * Select the mux mode 3 to drive HSUSB_HUB_POWER to the output
     * CONTROL_CORE_PAD0_KPD_COL1[3:0] = 0x3; Select gpio_1
     */
    volatile uint16_t *dpad_mux = (uint16_t *) 0x4A100186;
    *dpad_mux = 0x0003;

    // output enable on GPIO 1
    uint32_t gpoi_1_oe = omap44xx_gpio_oe_rd(&g1) & (~(1UL << HSUSB_HUB_POWER));
    gpoi_1_oe &= (~(1UL << 31));
    omap44xx_gpio_oe_wr(&g1, gpoi_1_oe);

    // RESET: clear the data out on the HSUSB_HUB_POWER pin to disable the power output
    omap44xx_gpio_cleardataout_wr(&g1, 1UL << HSUSB_HUB_POWER);

    /*
     * HUB OPERATION MODE
     *
     * Operation mode for the HS USB Hub
     *
     * HSUSB_HUB_RESET: 0 = Hub & Phy held in reset     1 = Normal operation.
     */

    /*
     * Select the correct mux mode for the gpio_62
     * CONTROL_CORE_PAD0_GPMC_WAIT1_PAD1_GPMC_WAIT2[2:0] = 0x3: Select gpio_62
     */
    volatile uint16_t *dpad_mux2 = (uint16_t *) 0x4A10008C;
    *dpad_mux2 = 0x3;

    // enable output on HSUSB_HUB_RESET
    uint32_t gpoi_2_oe = omap44xx_gpio_oe_rd(&g1) & (~(1UL << HSUSB_HUB_RESET));
    omap44xx_gpio_oe_wr(&g2, gpoi_2_oe);

    // perform a RESET on the hub
    omap44xx_gpio_cleardataout_wr(&g2, (1UL << HSUSB_HUB_RESET));

    /*
     * wait some time till the reset is done
     */
    for (int j = 0; j < 4000; j++) {
        printf("%c", 0xE);
    }

    // change hub state to operational
    omap44xx_gpio_setdataout_wr(&g2, (1UL << HSUSB_HUB_RESET));

    // enable power
    omap44xx_gpio_setdataout_wr(&g1, (1UL << HSUSB_HUB_POWER));

}

/*
 * initialize the USB functionality of the pandaboard
 */
static void hsusb_init(void)
{
    printf("hsusb initialization routine");

    //
    void *l4_cfg_base = (void *) (0x4A000000);
    volatile uint32_t *reg;

    /*
     * CM_L3INIT_STATICDEP: Enabling static dependencies
     */
    reg = (uint32_t *) (l4_cfg_base + 0x9304);
    *reg = 0xFFFF;
    printf("  CM_L3INIT_STATICDEP [%p, %x]\n", reg, *reg);

    /*
     * CM_L3INIT_HSUSBHOST_CLKCTRL: enabling the HS USB host clock
     */
    reg = (uint32_t *) (l4_cfg_base + 0x9358);
    *reg = (0x2 | (0x1 << 15) | (0x3 << 8));
    printf("  CM_L3INIT_HSUSBHOST_CLKCTRL [%p, %x]\n", reg, *reg);

    /*
     * CM_L3INIT_HSUSBTLL_CLKCTRL: enabling the HS USB TTL clock
     */
    reg = (uint32_t *) (l4_cfg_base + 0x9368);
    *reg = (0x1 << 8) | (0x1 << 9) | 0x1;
    printf("  CM_L3INIT_HSUSBTLL_CLKCTRL [%p, %x]\n", reg, *reg);

    /*
     * CM_L3INIT_USBPHY_CLKCTRL: enabling the USBPHY clock
     */
    reg = (uint32_t *) (l4_cfg_base + 0x93E0);
    *reg = (0x1 << 8) | 0x1;
    printf("  CM_L3INIT_USBPHY_CLKCTRL [%p, %x]\n", reg, *reg);

#define OMAP_PULL_ENA                   (1 << 3)
#define OMAP_INPUT_EN                   (1 << 8)
#define OMAP_MUX_CLEAR                  (~0x7)
#define OMAP_MUX_ULPI_PHY               (0x4)
#define OMAP_MUX_ULPI_TTL               (0x0)
#define OMAP_INPUT_PULLDOWN         (OMAP_PULL_ENA | OMAP_INPUT_EN)

    /*
     * Selecting the output mode 4 for the pins to ULPI PHY
     */
    printf("  Setting output mux modes: \n");

    volatile uint16_t *USBB1 = (uint16_t *) (l4_cfg_base + 0x1000C2);

    *USBB1 = OMAP_INPUT_PULLDOWN | OMAP_MUX_ULPI_PHY;
    printf("   - setting mux mode %p to %x \n", USBB1,
            OMAP_INPUT_PULLDOWN | OMAP_MUX_ULPI_PHY);
    USBB1++;
    *USBB1 = OMAP_MUX_ULPI_PHY;
    printf("   - setting mux mode %p to %x \n", USBB1, OMAP_MUX_ULPI_PHY);
    USBB1++;
    for (uint16_t i = 0; i < 10; i++) {
        printf("   - setting mux mode %p to %x \n", USBB1,
                OMAP_INPUT_PULLDOWN | OMAP_MUX_ULPI_PHY);
        *USBB1 = OMAP_INPUT_PULLDOWN | OMAP_MUX_ULPI_PHY;
        USBB1++;
    }

    /*
     * Global Initialization of the OMAPP44xx USB Sub System
     */
    printf("  Global initialization of HS USB Subsystem\n");
    void *usb_base = (void *)0x4A062000;
    printf("   - USB TTL reset...");
    /*
     * Reset USBTTL
     */
    volatile uint32_t *USBTLL_SYSCONFIG = (uint32_t *) (usb_base + 0x10);
    *USBTLL_SYSCONFIG = ((*USBTLL_SYSCONFIG) | 0x2);

    /*
     * wait till reset is done
     */
    volatile uint32_t *USBTLL_SYSSTATUS = (uint32_t *) (usb_base + 0x14);
    while (!((*USBTLL_SYSSTATUS) & 0x1))
        ;

    printf("OK\n");

    /*
     * enable interrupts on USBTTL
     */
    volatile uint32_t *USBTLL_IRQENABLE = (uint32_t *) (usb_base + 0x1C);
    *USBTLL_IRQENABLE = (*USBTLL_IRQENABLE) | 0x3;

    printf("   - Host controller reset...");
    /*
     * host controller reset
     */
    volatile uint32_t *UHH_SYSCONFIG = (uint32_t *) (usb_base + 0x2010);
    *UHH_SYSCONFIG = (*UHH_SYSCONFIG) | 0x1;

    /*
     * wait till reset is done
     */
    volatile uint32_t * UHH_SYSSTATUS = (uint32_t *) (usb_base + 0x2014);
    while (((*UHH_SYSSTATUS) & 0x6) != 0x6)
        ;
    printf("OK\n");

    printf("   - Setting USB host configuration values...");

    /*
     * setting the host configuration to external PHY and enable
     * the burst types
     */
    volatile uint32_t *UHH_HOSTCONFIG = (uint32_t *) (usb_base + 0x2040);
    *UHH_HOSTCONFIG = (0xF << 2);  // enable the burst types

    /*
     * Channel 0:
     * Port 0 goes to the USB/Ethernet device i.e. external phy
     *
     * Enable channel, select UL
     */
    volatile uint32_t *TLL_CHANNEL_CONF_0 = (uint32_t *) (usb_base + 0x40);
    *TLL_CHANNEL_CONF_0 = (0x1 << 3) | 0x1;

    /*
     * Channel 1:
     * PORT 1 is unused so far... disable the channel
     */
    volatile uint32_t *TLL_CHANNEL_CONF_1 = (uint32_t *) (usb_base + 0x44);
    *TLL_CHANNEL_CONF_1 = 0x0;

    /*
     * set the clock to be always running
     */
    volatile uint32_t *TLL_SHARED_CONF = (uint32_t *) (usb_base + 0x30);
    *TLL_SHARED_CONF = 0x1;

    printf("OK\n");


    printf("hsusb initialization done.\n");
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

    // pandaboard usb initialization
    usb_power_on();
    hsusb_init();

    if (1) {
        set_leds();
    }

    paging_init();
    cp15_enable_mmu();
    printf("MMU enabled\n");
    text_init();
}
