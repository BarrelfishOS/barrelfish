/*
 * Copyright (c) 2009-2016, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.  If
 * you do not find this file, copies can be found by writing to: ETH Zurich
 * D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich, Attn: Systems Group.
 */

/**
 * \file
 * \brief CPU driver init code for ARMv7A cores.
 */
#include <kernel.h>

#include <barrelfish_kpi/arm_core_data.h>
#include <bitmacros.h>
#include <coreboot.h>
#include <cp15.h>
#include <dev/cpuid_arm_dev.h>
#include <exceptions.h>
#include <getopt/getopt.h>
#include <gic.h>
#include <global.h>
#include <init.h>
#include <kcb.h>
#include <kernel_multiboot.h>
#include <offsets.h>
#include <paging_kernel_arch.h>
#include <platform.h>
#include <serial.h>
#include <startup_arch.h>
#include <stdio.h>
#include <string.h>

/*
 * Forward declarations
 */

static void bsp_init( void *pointer );
static void nonbsp_init( void *pointer );

#define MSG(format, ...) printk( LOG_NOTE, "ARMv7-A: "format, ## __VA_ARGS__ )

static bool is_bsp = false;

//
// Kernel command line variables and binding options
//

uint32_t periphclk = 0;
uint32_t periphbase = 0;

static struct cmdarg cmdargs[] = {
    { "consolePort", ArgType_UInt, { .uinteger = (void *)0 } },
    { "debugPort",   ArgType_UInt, { .uinteger = (void *)0 } },
    { "loglevel",    ArgType_Int,  { .integer  = (void *)0 } },
    { "logmask",     ArgType_Int,  { .integer  = (void *)0 } },
    { "timeslice",   ArgType_Int,  { .integer  = (void *)0 } },
    { "periphclk",   ArgType_UInt, { .uinteger = (void *)0 } },
    { "periphbase",  ArgType_UInt, { .uinteger = (void *)0 } },
    { NULL, 0, { NULL } }
};

static void
init_cmdargs(void) {
    cmdargs[0].var.uinteger= &serial_console_port;
    cmdargs[1].var.uinteger= &serial_debug_port;
    cmdargs[2].var.integer=  &kernel_loglevel;
    cmdargs[3].var.integer=  &kernel_log_subsystem_mask;
    cmdargs[4].var.integer=  &kernel_timeslice;
    cmdargs[5].var.uinteger= &periphclk;
    cmdargs[6].var.uinteger= &periphbase;
}

/**
 * \brief Is this the BSP?
 * \return True iff the current core is the bootstrap processor.
 */
bool cpu_is_bsp(void)
{
    return is_bsp;
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
void __attribute__ ((noinline,noreturn)) arch_init(void *pointer)
{
    /* Now we're definitely executing inside the kernel window, with
     * translation and caches available, and all pointers relocated to their
     * correct virtual address. */

    /* Reinitialise the serial port, as it may have moved, and we need to map
     * it into high memory. */
    serial_console_init(true);

    MSG("arch_init_2 entered.\n");
    errval_t errval;
    assert(core_data != NULL);
    assert(paging_mmu_enabled());

    if(cp15_get_cpu_id() == 0) is_bsp = true;

    if(cpu_is_bsp()) bsp_init( pointer );
    else nonbsp_init(pointer);

    MSG("Initializing exceptions.\n");

    /* Initialise the exception stack pointers. */
    exceptions_load_stacks();

    /* Select high vectors */
    uint32_t sctlr= cp15_read_sctlr();
    sctlr|= BIT(13);
    cp15_write_sctlr(sctlr);

    // Relocate the KCB into our new address space
    kcb_current = (struct kcb *)local_phys_to_mem((lpaddr_t) kcb_current);

    MSG("Parsing command line\n");
    init_cmdargs();
    parse_commandline(MBADDR_ASSTRING(core_data->cmdline), cmdargs);
    kernel_timeslice = min(max(kernel_timeslice, 20), 1);

    MSG("Barrelfish CPU driver starting on ARMv7\n");

    errval = serial_debug_init();
    if (err_is_fail(errval)) {
        MSG("Failed to initialize debug port: %d", serial_debug_port);
    }
    MSG("Debug port initialized.\n");

    if (my_core_id != cp15_get_cpu_id()) {
        MSG("** setting my_core_id (="PRIuCOREID
            ") to match cp15_get_cpu_id() (=%u)\n");
        my_core_id = cp15_get_cpu_id();
    }
    MSG("Set my core id to %d\n", my_core_id);

    MSG("Initializing the GIC\n");
    gic_init();
    MSG("gic_init done\n");

    if (cpu_is_bsp()) {
        platform_revision_init();
	    MSG("%"PRIu32" cores in system\n", platform_get_core_count());
    }

    MSG("Enabling timers\n");
    timers_init(kernel_timeslice);

    MSG("Enabling cycle counter user access\n");
    /* enable user-mode access to the performance counter */
    __asm volatile ("mcr p15, 0, %0, C9, C14, 0\n\t" :: "r"(1));
    /* disable counter overflow interrupts (just in case) */
    __asm volatile ("mcr p15, 0, %0, C9, C14, 2\n\t" :: "r"(0x8000000f));

    MSG("Setting coreboot spawn handler\n");
    coreboot_set_spawn_handler(CPU_ARM7, platform_boot_aps);

    MSG("Calling arm_kernel_startup\n");
    arm_kernel_startup();
}

/**
 * \brief Initialization for the BSP (the first core to be booted). 
 * \param pointer address of \c multiboot_info
 */
static void bsp_init( void *pointer ) 
{
    struct multiboot_info *mb = pointer;

    /* Squirrel away a pointer to the multiboot header, and use the supplied
     * command line. */
    core_data->multiboot_header = (lpaddr_t)mb;
    core_data->cmdline          = mb->cmdline;

    /* The spinlocks are in the BSS, and thus already zeroed, but
     it's polite to explicitly initialize them here... */
    spinlock_init(&global->locks.print);
    
    MSG("We seem to be a BSP; multiboot info:\n");
    MSG(" mods_addr is 0x%"PRIxLVADDR"\n",       (lvaddr_t)mb->mods_addr);
    MSG(" mods_count is 0x%"PRIxLVADDR"\n",      (lvaddr_t)mb->mods_count);
    MSG(" cmdline is 0x%"PRIxLVADDR"\n",         (lvaddr_t)mb->cmdline);
    MSG(" cmdline reads '%s'\n",                 mb->cmdline);
    MSG(" mmap_length is 0x%"PRIxLVADDR"\n",     (lvaddr_t)mb->mmap_length);
    MSG(" mmap_addr is 0x%"PRIxLVADDR"\n",       (lvaddr_t)mb->mmap_addr);
    MSG(" multiboot_flags is 0x%"PRIxLVADDR"\n", (lvaddr_t)mb->flags);
}

/**
 * \brief Initialization on a non-BSP (second and successive cores). 
 * \param pointer address of the global structure set up by \c bsp_init
 */
static void nonbsp_init( void *pointer )
{
    panic("Unimplemented.\n");

#if 0
    MSG("We seem to be an AP.\n");
    // global = (struct global *)GLOBAL_VBASE;

    // Our core data (struct arm_core_data) is placed one page before the
    // first byte of the kernel image
    core_data = (struct arm_core_data *)
	((lpaddr_t)&kernel_first_byte - BASE_PAGE_SIZE);
    core_data->cmdline = (lpaddr_t)&core_data->kernel_cmdline;
    kcb_current = (struct kcb*) (lpaddr_t)core_data->kcb;
    my_core_id = core_data->dst_core_id;

    // Tell the BSP that we are started up
    platform_notify_bsp();

    // Print kernel address for debugging with gdb
    MSG("Barrelfish non-BSP CPU driver starting at addr 0x%"PRIxLVADDR" on core %"PRIuCOREID"\n",
	   local_phys_to_mem((lpaddr_t)&kernel_first_byte), my_core_id);
#endif
}
