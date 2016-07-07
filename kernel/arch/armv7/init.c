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
static void __attribute__ ((noinline,noreturn)) arch_init_2(void *pointer);

static void bsp_init( void *pointer );
static void nonbsp_init( void *pointer );

#define MSG(format, ...) printk( LOG_NOTE, "ARMv7-A: "format, ## __VA_ARGS__ )

static bool mmu_enabled = false;
static bool is_bsp = false;

//
// Kernel command line variables and binding options
//

uint32_t periphclk = 0;
uint32_t periphbase = 0;

static struct cmdarg cmdargs[] = {
    { "consolePort", ArgType_UInt, { .uinteger = &serial_console_port } },
    { "debugPort",   ArgType_UInt, { .uinteger = &serial_debug_port } },
    { "loglevel",    ArgType_Int,  { .integer  = &kernel_loglevel } },
    { "logmask",     ArgType_Int,  { .integer  = &kernel_log_subsystem_mask } },
    { "timeslice",   ArgType_Int,  { .integer  = &kernel_timeslice } },
    { "periphclk",   ArgType_UInt, { .uinteger = &periphclk } },
    { "periphbase",  ArgType_UInt, { .uinteger = &periphbase } },
    { NULL, 0, { NULL } }
};


/**
 * \brief Is the MMU enabled yet?  
 * \return True iff the MMU has been enabled.
 *
 * Used for assertion checking in platform code
 */
bool mmu_is_enabled(void)
{
    return mmu_enabled;
}

/**
 * \brief Is this the BSP?
 * \return True iff the current core is the bootstrap processor.
 */
bool cpu_is_bsp(void)
{
    return is_bsp;
}

/* Print a little information about the processor, and check that it supports
 * the features we require. */
static bool
check_cpuid(void) {
    uint32_t midr= cp15_read_midr();

    /* XXX - Mackerel should give nice strings to print. */
    MSG("This is an ");

    if(cpuid_arm_midr_architecture_extract((uint8_t *)&midr) != 0xf) {
        printf("unsupported ARMv6 or earlier core\n");
        return false;
    }

    switch(cpuid_arm_midr_implementer_extract((uint8_t *)&midr)) {
        case cpuid_arm_impl_arm:
            printf("ARM ");
            switch(cpuid_arm_midr_part_extract((uint8_t *)&midr)) {
                case cpuid_arm_part_a7:
                    printf("Cortex-A7 ");
                    break;
                case cpuid_arm_part_a8:
                    printf("Cortex-A8 ");
                    break;
                case cpuid_arm_part_a9:
                    printf("Cortex-A9 ");
                    break;
                case cpuid_arm_part_a15:
                    printf("Cortex-A15 ");
                    break;
                case cpuid_arm_part_a17:
                    printf("Cortex-A17 ");
                    break;
                case cpuid_arm_part_a53:
                    printf("Cortex-A53 ");
                    break;
                case cpuid_arm_part_a57:
                    printf("Cortex-A57 ");
                    break;
                case cpuid_arm_part_a72:
                    printf("Cortex-A72 ");
                    break;
                case cpuid_arm_part_a73:
                    printf("Cortex-A73 ");
                    break;
            }
            printf("r%dp%d\n",
                   cpuid_arm_midr_variant_extract((uint8_t *)&midr),
                   cpuid_arm_midr_revision_extract((uint8_t *)&midr));
            break;

        case cpuid_arm_impl_dec:
            printf("Unknown DEC core\n");
            break;
        case cpuid_arm_impl_motorola:
            printf("Unknown Motorola core\n");
            break;
        case cpuid_arm_impl_qualcomm:
            printf("Unknown Qualcomm core\n");
            break;
        case cpuid_arm_impl_marvell:
            printf("Unknown Marvell core\n");
            break;
        case cpuid_arm_impl_intel:
            printf("Unknown Intel core\n");
            break;

        default:
            printf("Unknown manufacturer's core\n");
            break;
    }

    uint32_t id_pfr1= cp15_read_id_pfr1();
    if(cpuid_arm_id_pfr1_security_extract((uint8_t *)&id_pfr1) ==
       cpuid_arm_sec_ni) {
        MSG("  Security extensions required but not implemented\n");
        return false;
    }
    else {
        MSG("  Security extensions implemented\n");
    }

    MSG("  Virtualisation extensions ");
    if(cpuid_arm_id_pfr1_virtualisation_extract((uint8_t *)&id_pfr1) ==
       cpuid_arm_ftr_i) {
        printf("implemented.\n");
    }
    else {
        printf("not implemented.\n");
    }

    MSG("  Generic timer ");
    if(cpuid_arm_id_pfr1_generic_timer_extract((uint8_t *)&id_pfr1) ==
       cpuid_arm_ftr_i) {
        printf("implemented.\n");
    }
    else {
        printf("not implemented.\n");
    }

    return true;
}

/**
 * \brief Entry point called from boot.S for the kernel. 
 *
 * \param pointer address of \c multiboot_info on the BSP; or the
 * global structure if we're on an AP. 
 */
void arch_init(void *pointer)
{
    // Do early initialization of the serial port given by a
    // command-line option. 
    serial_early_init(serial_console_port);

    check_cpuid();
    platform_print_id();

    // Print kernel address for debugging with gdb 
    MSG("First byte of kernel at 0x%"PRIxLVADDR"\n",
            local_phys_to_mem((uint32_t)&kernel_first_byte));

    MSG("Initializing paging...\n");
    paging_init();
    MSG("MMU enabled\n");
    mmu_enabled = true;

    arch_init_2(pointer);
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
static void __attribute__ ((noinline,noreturn)) arch_init_2(void *pointer)
{
    MSG("arch_init_2 entered.\n");
    errval_t errval;
    assert(core_data != NULL);
    assert(mmu_is_enabled());

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

    // Invalidate caches and TLBs
    cp15_invalidate_i_and_d_caches_fast();
    cp15_invalidate_tlb();

    // Relocate the KCB into our new address space
    kcb_current = (struct kcb *)local_phys_to_mem((lpaddr_t) kcb_current);

    MSG("Parsing command line\n");
    parse_commandline(MBADDR_ASSTRING(core_data->cmdline), cmdargs);
    kernel_timeslice = min(max(kernel_timeslice, 20), 1);

    MSG("Reinitializing console.\n");
    serial_console_init(true);

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
