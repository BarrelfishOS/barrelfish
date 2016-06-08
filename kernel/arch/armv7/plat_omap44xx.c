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
#include <platform.h>
#include <serial.h>
#include <assert.h>
#include <errors/errno.h>
#include <omap44xx_map.h>
#include <omap_uart.h>
#include <cp15.h>
#include <a9_scu.h>
#include <a9_gt.h>
#include <arm_hal.h>
#include <gic.h>
#include <init.h>
#include <global.h>
#include <paging_kernel_arch.h>
#include <dev/omap/omap44xx_id_dev.h>
#include <dev/omap/omap44xx_gpio_dev.h>
#include <dev/omap/omap44xx_emif_dev.h>
#include <dev/omap/omap44xx_cortexa9_wugen_dev.h>
#include <dev/cortex_a9_pit_dev.h>

#define MSG(format, ...) printk( LOG_NOTE, "OMAP44xx: "format, ## __VA_ARGS__ )


/********************************************************************************
 *
 * Implementation of serial.h
 *
 *******************************************************************************/

//
// Serial console and debugger interfaces
//
#define NUM_UARTS 4
unsigned int serial_console_port = 2;
unsigned int serial_debug_port = 2;
unsigned int serial_num_physical_ports = NUM_UARTS;

static lpaddr_t uart_base[NUM_UARTS] = {
    OMAP44XX_MAP_L4_PER_UART1,
    OMAP44XX_MAP_L4_PER_UART2,
    OMAP44XX_MAP_L4_PER_UART3,
    OMAP44XX_MAP_L4_PER_UART4
};

static size_t uart_size[NUM_UARTS] = {
    OMAP44XX_MAP_L4_PER_UART1_SIZE,
    OMAP44XX_MAP_L4_PER_UART2_SIZE,
    OMAP44XX_MAP_L4_PER_UART3_SIZE,
    OMAP44XX_MAP_L4_PER_UART4_SIZE
};

/*
 * Initialize the serial ports
 */
errval_t serial_early_init(unsigned port)
{
    // assert(port < NUM_UARTS);
    if (port >= serial_num_physical_ports) { 
	serial_num_physical_ports = port + 1;
    }
    spinlock_init(&global->locks.print);
    omap_uart_early_init(port, uart_base[port], uart_size[port]);
    return SYS_ERR_OK;
}

errval_t serial_init(unsigned port, bool initialize_hw)
{
    assert(port < serial_num_physical_ports);
    assert(mmu_is_enabled());
    omap_uart_init(port, initialize_hw);
    return SYS_ERR_OK;
};

void serial_putchar(unsigned port, char c)
{
    assert(port < serial_num_physical_ports);
    omap_uart_putchar(port, c);
}

char serial_getchar(unsigned port)
{
    assert(port < serial_num_physical_ports);
    return omap_uart_getchar(port);
}

/********************************************************************************
 *
 * Where's the GIC?
 *
 *******************************************************************************/
//
// Interrupt controller offsets from the "private memory area"
// Offsets taken from ARM Cortex A9 MPCore TRM Table 1-3
//
static lpaddr_t get_private_region(void);
static lpaddr_t get_private_region(void)
{
    assert(mmu_is_enabled());
    static lpaddr_t private_region = 0;
    if (private_region == 0) {
	private_region = cp15_read_cbar();
    }
    return private_region;
}

#define DIST_OFFSET     0x1000 // Interrupt Distributor
#define CPU_OFFSET 	0x0100 // Interrupt controller interface

lpaddr_t platform_get_distributor_address(void)
{
    assert(mmu_is_enabled());
    return (get_private_region() + DIST_OFFSET);
}

lpaddr_t platform_get_gic_cpu_address(void)
{
    assert(mmu_is_enabled());
    return (get_private_region() + CPU_OFFSET);
}

/********************************************************************************
 *
 * Where's the Snoop Control Unit?
 *
 *******************************************************************************/

void platform_init_scu(void)
{
    assert(mmu_is_enabled());
    a9_scu_init( get_private_region() );
    if (a9_scu_core_count() > 1) {
	a9_scu_enable();
    }
}

size_t platform_get_core_count(void)
{
    return a9_scu_core_count();
}

/*
 * Print system identification.   MMU is NOT yet enabled.
 * Use Mackerel to print the identification from the system
 * configuration block.  Documentation in the OMAP4460 TRM p. 18.6.2 
 */
static void set_leds(void);
void platform_print_id(void)
{
    char buf[800];
    omap44xx_id_t id;
    omap44xx_id_initialize(&id,
            (mackerel_addr_t) OMAP44XX_MAP_L4_CFG_SYSCTRL_GENERAL_CORE);
    omap44xx_id_codevals_prtval(buf, 799, omap44xx_id_code_rawrd(&id));
    printf("Device: this is a %s\n", buf);
    omap44xx_id_pr(buf, 799, &id);
    // printf("%s\n", buf);
    set_leds();
}

/*
 * Say hello by flashing both LEDs.
 */
static void set_leds(void)
{
    uint32_t r, nr;
    omap44xx_gpio_t g;
    omap44xx_gpio_initialize(&g, (mackerel_addr_t) OMAP44XX_MAP_L4_WKUP_GPIO1);
    // Output enable
    r = omap44xx_gpio_oe_rd(&g) & (~(1 << 8));
    omap44xx_gpio_oe_wr(&g, r);
    // Write data out
    r = omap44xx_gpio_dataout_rd(&g) & (~(1 << 8));
    nr = r | (1 << 8);
    for (int i = 0; i < 5; i++) {
        omap44xx_gpio_dataout_wr(&g, r);
        for (int j = 0; j < 20; j++) {
            printf("%c", 0xE);
        }
        omap44xx_gpio_dataout_wr(&g, nr);
        for (int j = 0; j < 20; j++) {
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
        for (int j = 0; j < 20; j++) {
            printf("%c", 0xE);
        }
        omap44xx_gpio_dataout_wr(&g, nr);
        for (int j = 0; j < 20; j++) {
            printf("%c", 0xE);
        }
    }
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
    assert(!mmu_is_enabled());
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
int platform_boot_aps(coreid_t core_id, genvaddr_t gen_entry)
{
    assert(mmu_is_enabled());
    lvaddr_t entry = (lvaddr_t) gen_entry;

    /* pointer to the pseudo-lock used to detect boot up of new core */
    volatile uint32_t *ap_wait = (uint32_t*)local_phys_to_mem(AP_WAIT_PHYS);
    *ap_wait = AP_STARTING_UP;
    cp15_invalidate_d_cache();

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
}

void platform_notify_bsp(void)
{
    // Tell the BSP that we are started up
    // See Section 27.4.4 in the OMAP44xx manual for how this should work.
    // We do this early, to avoid having to map the registers
    assert(mmu_is_enabled());
    omap44xx_cortexa9_wugen_t wugen;
    omap44xx_cortexa9_wugen_initialize( &wugen, (mackerel_addr_t)OMAP44XX_MAP_CORTEXA9_WUGEN);
    omap44xx_cortexa9_wugen_aux_core_boot_0_cpu1_status_wrf(&wugen, 0x2);
    volatile uint32_t *ap_wait = (uint32_t*)local_phys_to_mem(AP_WAIT_PHYS);
    //__sync_synchronize();
    *((volatile lvaddr_t *)ap_wait) = AP_STARTED;
}

/*
 * Global timer: use the Cortex-A9 Global Timer (see ARM TRM 4.4.1)
 */
#define GT_OFFSET      0x0200
void gt_init(void)
{
    a9_gt_init(get_private_region() + GT_OFFSET);
}
uint64_t gt_read(void) { return a9_gt_read(); }
uint32_t gt_read_low(void) { return a9_gt_read_low(); }
uint32_t gt_read_high(void) { return a9_gt_read_high(); }

/*
 * Time slice counter
 */

// clock rate hardcoded to 2GHz
static const uint32_t tsc_hz = 2000000000;
#define TSC_OFFSET	0x600
#define LOCAL_TIMER_IRQ 29
static cortex_a9_pit_t tsc;

void tsc_init(int timeslice)
{
    lvaddr_t base = paging_map_device( get_private_region() + TSC_OFFSET, 0x200 );
    cortex_a9_pit_initialize(&tsc, (mackerel_addr_t)base);

    // write load
    uint32_t load = (100000000); // in cycles [ should be around 10 per second ]
    cortex_a9_pit_TimerLoad_wr(&tsc, load);

    //configure tsc
    cortex_a9_pit_TimerControl_prescale_wrf(&tsc, 0);
    cortex_a9_pit_TimerControl_int_enable_wrf(&tsc, 1);
    // XXX Disable interrupts, to ease debugging init startup
    //cortex_a9_pit_TimerControl_int_enable_wrf(&tsc, 0);
    cortex_a9_pit_TimerControl_auto_reload_wrf(&tsc, 1);
    cortex_a9_pit_TimerControl_timer_enable_wrf(&tsc, 1);
    gic_enable_interrupt( LOCAL_TIMER_IRQ, 0, 0, 0, 0);
}

uint32_t tsc_read(void)
{
    // Timers count down so invert it.
    return ~cortex_a9_pit_TimerCounter_rd(&tsc);
}

uint32_t tsc_get_hz(void)
{
    return tsc_hz;
}

#define LOCAL_TIMER_IRQ 29

static uint32_t local_timer_counter = 0;
bool pit_handle_irq(uint32_t irq)
{
//    printf("pit_handle_irq %d\n", irq);

    switch(irq) {
    case LOCAL_TIMER_IRQ:
        gic_ack_irq(irq);
        local_timer_counter++;
        return 1;

    default:
        return 0;
    }
}
