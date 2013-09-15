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
#include <spinlock.h>
#include <stdio.h>
#include <arm_hal.h>
#include <getopt/getopt.h>
#include <cp15.h>
#include <elf/elf.h>
#include <arm_core_data.h>
#include <startup_arch.h>
#include <kernel_multiboot.h>
#include <global.h>
#include <arch/armv7/start_aps.h> // AP_WAIT_*, AUX_CORE_BOOT_*  and friends
#include <cortexm3_heteropanda.h>

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
uintptr_t kernel_stack[KERNEL_STACK_SIZE/sizeof(uintptr_t)]
__attribute__ ((aligned(8)));

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define CONSTRAIN(x, a, b) MIN(MAX(x, a), b)

//
// Kernel command line variables and binding options
//

static int timeslice  = 5; //interval in ms in which the scheduler gets called

static struct cmdarg cmdargs[] = {
    { "consolePort",    ArgType_UInt, { .uinteger = &serial_console_port}},
    { "debugPort",      ArgType_UInt, { .uinteger = &serial_debug_port}},
    { "loglevel",       ArgType_Int, { .integer = &kernel_loglevel }},
    { "logmask",        ArgType_Int, { .integer = &kernel_log_subsystem_mask }},
    { "timeslice",      ArgType_Int, { .integer = &timeslice }},
    {NULL, 0, {NULL}}
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

extern void paging_map_device_section(uintptr_t ttbase, lvaddr_t va, lpaddr_t pa);

/**
 * Create initial (temporary) page tables.
 *
 * We use 1MB (ARM_L1_SECTION_BYTES) pages (sections) with a single-level table.
 * This allows 1MB*4k (ARM_L1_MAX_ENTRIES) = 4G per pagetable.
 *
 * Hardware details can be found in:
 * ARM Architecture Reference Manual, ARMv7-A and ARMv7-R edition
 *   B3: Virtual Memory System Architecture (VMSA)
 */
static void paging_init(void)
{
    /*
     * page tables:
     *  l1_low  for addresses  < 2GB  (translated by TTBR0)
     *  l1_high for addresses  >= 2GB (translated by TTBR1)
     *
     * XXX: We reserve double the space needed to be able to align the
     * pagetable to 16K after relocation.
     */
     static union arm_l1_entry l1_low[2*ARM_L1_MAX_ENTRIES];
     static union arm_l1_entry l1_high[2*ARM_L1_MAX_ENTRIES];

    /**
     * TTBCR: Translation Table Base Control register.
     *  TTBCR.N is bits[2:0]
     * In a TLB miss TTBCR.N determines whether TTBR0 or TTBR1 is used as the
     * base address for the translation table walk in memory:
     *  N == 0 -> always use TTBR0
     *  N >  0 -> if VA[31:32-N] > 0 use TTBR1 else use TTBR0
     *
     * TTBR0 is typically used for processes-specific addresses
     * TTBR1 is typically used for OS addresses that do not change on context
     *       switch
     *
     * set TTBCR.N = 1 to use TTBR1 for VAs >= MEMORY_OFFSET (=2GB)
     */
    #define TTBCR_N 1
    uint32_t ttbcr = cp15_read_ttbcr();
    ttbcr =  (ttbcr & ~7) | TTBCR_N;
    cp15_write_ttbcr(ttbcr);
    STATIC_ASSERT(1UL<<(32-TTBCR_N) == MEMORY_OFFSET, "");
    #undef TTBCR_N


    /**
     * in omap44xx, physical memory (PHYS_MEMORY_START) is the same with the the
     * offset of mapped physical memory within virtual address space
     * (PHYS_MEMORY_START), so we just create identity mappings.
     */
    STATIC_ASSERT(MEMORY_OFFSET == PHYS_MEMORY_START, "");
    uintptr_t l1_low_aligned = ROUND_UP((uintptr_t)l1_low, ARM_L1_ALIGN);
    uintptr_t l1_high_aligned = ROUND_UP((uintptr_t)l1_high, ARM_L1_ALIGN);
    lvaddr_t vbase = MEMORY_OFFSET, base =  0;
    for (size_t i=0; i < ARM_L1_MAX_ENTRIES/2; i++) {
	    paging_map_device_section(l1_low_aligned, base, base);
	    paging_map_device_section(l1_high_aligned, vbase, vbase);

        base += ARM_L1_SECTION_BYTES;
        vbase += ARM_L1_SECTION_BYTES;
    }

    // Activate new page tables
    cp15_write_ttbr1(l1_high_aligned);
    cp15_write_ttbr0(l1_low_aligned);
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
static void  __attribute__ ((noinline,noreturn)) text_init(void)
{
    errval_t errval;

    if ((glbl_core_data->multiboot_flags & MULTIBOOT_INFO_FLAG_HAS_MMAP)) {
        // BSP core: set final page tables
        struct arm_coredata_mmap *mmap = (struct arm_coredata_mmap *)
            local_phys_to_mem(glbl_core_data->mmap_addr);
        paging_arm_reset(mmap->base_addr, mmap->length);
        //printf("paging_arm_reset: base: 0x%"PRIx64", length: 0x%"PRIx64".\n", mmap->base_addr, mmap->length);
    } else {
        // AP core
        //  FIXME: Not sure what to do, so map the whole memory for now
        paging_arm_reset(PHYS_MEMORY_START, 0x40000000);
    }

    exceptions_init();

    //printf("invalidate cache\n");
    cp15_invalidate_i_and_d_caches_fast();
    //printf("invalidate TLB\n");
    cp15_invalidate_tlb();

    //printf("startup_early\n");
    kernel_startup_early();
    //printf("kernel_startup_early done!\n");

    //initialize console
    serial_init(serial_console_port);
    spinlock_init();

    printf("Barrelfish CPU driver starting on ARMv7 OMAP44xx"
           " Board id 0x%08"PRIx32"\n", hal_get_board_id());
    printf("The address of paging_map_kernel_section is %p\n",
           paging_map_kernel_section);

    errval = serial_debug_init();
    if (err_is_fail(errval)) {
            printf("Failed to initialize debug port: %d", serial_debug_port);
    }

    if (my_core_id != hal_get_cpu_id()) {
        printf("** setting my_core_id (="PRIuCOREID") to match hal_get_cpu_id() (=%u)\n");
        my_core_id = hal_get_cpu_id();
    }

    // Test MMU by remapping the device identifier and reading it using a
    // virtual address
    lpaddr_t id_code_section = OMAP44XX_MAP_L4_CFG_SYSCTRL_GENERAL_CORE & ~ARM_L1_SECTION_MASK;
    lvaddr_t id_code_remapped = paging_map_device(id_code_section,
                                                  ARM_L1_SECTION_BYTES);
    omap44xx_id_t id;
    omap44xx_id_initialize(&id, (mackerel_addr_t)(id_code_remapped +
	     (OMAP44XX_MAP_L4_CFG_SYSCTRL_GENERAL_CORE & ARM_L1_SECTION_MASK)));

    char buf[200];
    omap44xx_id_code_pr(buf,200,&id);
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
    omap44xx_id_initialize(&id, (mackerel_addr_t)OMAP44XX_MAP_L4_CFG_SYSCTRL_GENERAL_CORE);
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
    omap44xx_emif_initialize(&emif, (mackerel_addr_t)base);

    if (!omap44xx_emif_status_phy_dll_ready_rdf(&emif)) {
        printf("EMIF%d doesn't seem to have any DDRAM attached.\n", bank);
        return 0;
    }

    rowbits = omap44xx_emif_sdram_config_rowsize_rdf(&emif) + 9;
    colbits = omap44xx_emif_sdram_config_pagesize_rdf(&emif) + 9;
    rowsize = omap44xx_emif_sdram_config2_rdbsize_rdf(&emif) + 5;

    printf("EMIF%d: ready, %d-bit rows, %d-bit cols, %d-byte row buffer\n",
            bank, rowbits, colbits, 1<<rowsize);

    return (1 << (rowbits + colbits + rowsize));
}

static void size_ram(void)
{
    size_t sz = 0;
    sz = bank_size(1, OMAP44XX_MAP_EMIF1) + bank_size(2, OMAP44XX_MAP_EMIF2);
    printf("We seem to have 0x%08lx bytes of DDRAM: that's %s.\n",
            sz, sz == 0x40000000 ? "about right" : "unexpected" );
}

/*
 * Doesn't work yet on the second LED for some reason...
 */
static void set_leds(void)
{
    uint32_t r, nr;
    omap44xx_gpio_t g;
    //char buf[8001];

    omap44xx_gpio_initialize(&g, (mackerel_addr_t)OMAP44XX_MAP_L4_WKUP_GPIO1);
    // Output enable
    r = omap44xx_gpio_oe_rd(&g) & (~(1<<8));
    omap44xx_gpio_oe_wr(&g,r);
    // Write data out
    r = omap44xx_gpio_dataout_rd(&g);
    nr = r  |(1<<8); 
    for(int i = 0; i < 5; i++) {
	omap44xx_gpio_dataout_wr(&g,r);
	for(int j = 0; j < 2000; j++) { 
	    printf(".");
	}
	omap44xx_gpio_dataout_wr(&g,nr);
	for(int j = 0; j < 2000; j++) { 
	    printf(".");
	}
    }
    return;

    omap44xx_gpio_initialize(&g, (mackerel_addr_t)OMAP44XX_MAP_L4_PER_GPIO4);

    // Output enable
    r = omap44xx_gpio_oe_rd(&g) & (~(1<<14));
    omap44xx_gpio_oe_wr(&g,r);
    // Write data out
    r = omap44xx_gpio_dataout_rd(&g);
    nr = r  |(1<<14); 
    for(int i = 0; i < 100; i++) {
	omap44xx_gpio_dataout_wr(&g,r);
	for(int j = 0; j < 2000; j++) { 
	    printf(".");
	}
	omap44xx_gpio_dataout_wr(&g,nr);
	for(int j = 0; j < 2000; j++) { 
	    printf(".");
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

    serial_early_init(serial_console_port);
    spinlock_early_init();//from here on we can safely use printf

    if (hal_cpu_is_bsp()) {
        struct multiboot_info *mb = pointer;

        memset(glbl_core_data, 0, sizeof(struct arm_core_data));

        size_t max_addr = max(multiboot_end_addr(mb), (uintptr_t)&kernel_final_byte);
        glbl_core_data->start_free_ram = ROUND_UP(max_addr, BASE_PAGE_SIZE);
        glbl_core_data->mods_addr = mb->mods_addr;
        glbl_core_data->mods_count = mb->mods_count;
        glbl_core_data->cmdline = mb->cmdline;
        glbl_core_data->mmap_length = mb->mmap_length;
        glbl_core_data->mmap_addr = mb->mmap_addr;
        glbl_core_data->multiboot_flags = mb->flags;

        memset(&global->locks, 0, sizeof(global->locks));
        
#ifdef HETEROPANDA
        //boot up a cortex-m3 core
        
        cortex_m3_early_init();
        //set up and run heteropanda_slave image on cortex-m3
        //XXX: HACK: to find out where the heteropanda_slave image starts, molly will
        //just write the address into multiboot_info->mem_lower (which is otherwise unused)
        void* start_slave = (void*) (mb->mem_lower);
        prepare_and_start_m3(start_slave);    
        //printf("entering endless loop so the M3 can work unhindered.\n");
        //while(1){}
#endif  //HETEROPANDA

    } else {
        global = (struct global *)GLOBAL_VBASE;
        // zeroing locks for the app core seems bogus to me --AKK
        //memset(&global->locks, 0, sizeof(global->locks));

        // our core data (struct arm_core_data) is placed one page before the
        // first byte of the kernel image
        glbl_core_data = (struct arm_core_data *)
                            ((lpaddr_t)&kernel_first_byte - BASE_PAGE_SIZE);
        glbl_core_data->cmdline = (lpaddr_t)&glbl_core_data->kernel_cmdline;
        my_core_id = glbl_core_data->dst_core_id;

        // tell BSP that we are started up
        // See Section 27.4.4 in the OMAP44xx manual for how this should work.
        // we do this early, to avoid having to map the registers
        lpaddr_t aux_core_boot_0 = AUX_CORE_BOOT_0;
        lpaddr_t ap_wait = AP_WAIT_PHYS;

        *((volatile lvaddr_t *)aux_core_boot_0) = 2<<2;
        //__sync_synchronize();
        *((volatile lvaddr_t *)ap_wait) = AP_STARTED;

    }

    // XXX: print kernel address for debugging with gdb
    printf("Barrelfish OMAP44xx CPU driver starting at addr 0x%"PRIxLVADDR" on core %"PRIuCOREID"\n",
            local_phys_to_mem((lpaddr_t)&kernel_first_byte), my_core_id);

    if (hal_cpu_is_bsp()) {
        print_system_identification();
        size_ram();
    }

    if (0) {
        set_leds();
    }

    paging_init();
    cp15_enable_mmu();
    cp15_enable_alignment();
    printf("MMU enabled\n");
    text_init();
}
