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
#include <barrelfish_kpi/flags_arch.h>
#include <bitmacros.h>
#include <boot_protocol.h>
#include <coreboot.h>
#include <cp15.h>
#include <dev/cpuid_arm_dev.h>
#include <elf/elf.h>
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

#if 0
static void bsp_init( void *pointer );
static void nonbsp_init( void *pointer );
#endif

#define MSG(format, ...) printk( LOG_NOTE, "ARMv7-A: "format, ## __VA_ARGS__ )

/* This is the kernel stack, allocated in the BSS. */
uintptr_t kernel_stack[KERNEL_STACK_SIZE/sizeof(uintptr_t)]
    __attribute__((aligned(8)));

static bool is_bsp = false;

//
// Kernel command line variables and binding options
//

uint32_t periphclk = 0;
uint32_t periphbase = 0;
uint32_t timerirq = 0;
uint32_t cntfrq = 0;

static struct cmdarg cmdargs[] = {
    { "consolePort", ArgType_UInt, { .uinteger = (void *)0 } },
    { "debugPort",   ArgType_UInt, { .uinteger = (void *)0 } },
    { "loglevel",    ArgType_Int,  { .integer  = (void *)0 } },
    { "logmask",     ArgType_Int,  { .integer  = (void *)0 } },
    { "timeslice",   ArgType_Int,  { .integer  = (void *)0 } },
    { "periphclk",   ArgType_UInt, { .uinteger = (void *)0 } },
    { "periphbase",  ArgType_UInt, { .uinteger = (void *)0 } },
    { "timerirq"  ,  ArgType_UInt, { .uinteger = (void *)0 } },
    { "cntfrq"  ,    ArgType_UInt, { .uinteger = (void *)0 } },
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
    cmdargs[7].var.uinteger= &timerirq;
    cmdargs[8].var.uinteger= &cntfrq;
}

/**
 * \brief Is this the BSP?
 * \return True iff the current core is the bootstrap processor.
 */
bool cpu_is_bsp(void)
{
    return is_bsp;
}

#define EXCEPTION_MODE_STACK_BYTES       256

/*
 * Exception mode stacks
 *
 * These are small stacks used to figure out where to spill registers. As
 * these are banked functions are expected to leave them as found (ie. so they
 * do not need to be reset next time around).
 */
char abt_stack[EXCEPTION_MODE_STACK_BYTES] __attribute__((aligned(8)));
char irq_stack[EXCEPTION_MODE_STACK_BYTES] __attribute__((aligned(8)));
char fiq_stack[EXCEPTION_MODE_STACK_BYTES] __attribute__((aligned(8)));
char undef_stack[EXCEPTION_MODE_STACK_BYTES] __attribute__((aligned(8)));
char svc_stack[EXCEPTION_MODE_STACK_BYTES] __attribute__((aligned(8)));

void set_stack_for_mode(uint8_t mode, void *sp_mode);

/**
 * Initialise the banked exception-mode stack registers.
 *
 * The kernel doesn't actually need separate stacks for different modes, as
 * it's reentrant, but it's useful for debugging in-kernel faults.
 */
static void
exceptions_load_stacks(void) {
    set_stack_for_mode(ARM_MODE_ABT, abt_stack   + EXCEPTION_MODE_STACK_BYTES);
    set_stack_for_mode(ARM_MODE_IRQ, irq_stack   + EXCEPTION_MODE_STACK_BYTES);
    set_stack_for_mode(ARM_MODE_FIQ, fiq_stack   + EXCEPTION_MODE_STACK_BYTES);
    set_stack_for_mode(ARM_MODE_UND, undef_stack + EXCEPTION_MODE_STACK_BYTES);
    set_stack_for_mode(ARM_MODE_SVC, svc_stack   + EXCEPTION_MODE_STACK_BYTES);
}

/**
 * \brief Continue kernel initialization in kernel address space.
 *
 * This function sets up exception handling, initializes devices and enables
 * interrupts. After that it calls arm_kernel_startup(), which should not
 * return (if it does, this function halts the kernel).
 */
void
arch_init(struct arm_core_data *boot_core_data,
          struct armv7_boot_record *bootrec) {
    /* Now we're definitely executing inside the kernel window, with
     * translation and caches available, and all pointers relocated to their
     * correct virtual address.  The low mappings are still enabled, but we
     * shouldn't be accessing them any longer, no matter where RAM is located.
     * */

    /* There's no boot record iff we're the first core to boot. */
    is_bsp= (bootrec == NULL);

    /* Save our core data. */
    core_data= boot_core_data;

    /* Let the paging code know where the kernel page tables are.  Note that
     * paging_map_device() won't work until this is called. */
    paging_load_pointers(core_data);

    /* Reinitialise the serial port, as it may have moved, and we need to map
     * it into high memory. */
    /* XXX - reread the args to update serial_console_port. */
    serial_console_init(is_bsp);

    /* Load the global lock address. */
    global= (struct global *)core_data->global;

    /* Select high vectors */
    uint32_t sctlr= cp15_read_sctlr();
    sctlr|= BIT(13);
    cp15_write_sctlr(sctlr);

    my_core_id = cp15_get_cpu_id();

    MSG("Barrelfish CPU driver starting on ARMv7\n");
    MSG("Core data at %p\n", core_data);
    MSG("Global data at %p\n", global);
    MSG("Boot record at %p\n", bootrec);
    errval_t errval;
    assert(core_data != NULL);
    assert(paging_mmu_enabled());

    if(!is_bsp) {
        MSG("APP core.\n");
        platform_notify_bsp(&bootrec->done);
    }

    /* Read the build ID, and store it. */
    const char *build_id_name=
        ((const char *)&build_id_nhdr) + sizeof(struct Elf32_Nhdr);

    if(build_id_nhdr.n_type != NT_GNU_BUILD_ID ||
       build_id_nhdr.n_namesz != 4 ||
       strncmp(build_id_name, "GNU", 4)) {
        panic("Build ID missing or corrupted.\n");
    }

    if(build_id_nhdr.n_descsz > MAX_BUILD_ID) {
        panic("Build ID is more than %zu bytes.\n", MAX_BUILD_ID);
    }

    core_data->build_id.length= build_id_nhdr.n_descsz;
    const char *build_id_data= build_id_name + build_id_nhdr.n_namesz;
    memcpy(core_data->build_id.data, build_id_data, build_id_nhdr.n_descsz);

    MSG("Build ID: ");
    for(size_t i= 0; i < core_data->build_id.length; i++)
        printf("%02x", build_id_data[i]);
    printf("\n");

    struct multiboot_info *mb=
        (struct multiboot_info *)core_data->multiboot_header;

    /* On the BSP core, initialise our core_data command line. */
    if(is_bsp) {
        const char *mb_cmdline=
            (const char *)local_phys_to_mem((lpaddr_t)mb->cmdline);
        memcpy(core_data->cmdline_buf, mb_cmdline,
               min(MAXCMDLINE-1, strlen(mb_cmdline)));
        core_data->cmdline_buf[MAXCMDLINE-1]= '\0';
    }
    
    MSG("Multiboot info:\n");
    MSG(" info header at 0x%"PRIxLVADDR"\n",       (lvaddr_t)mb);
    MSG(" mods_addr is P:0x%"PRIxLPADDR"\n",       (lpaddr_t)mb->mods_addr);
    MSG(" mods_count is 0x%"PRIu32"\n",                      mb->mods_count);
    MSG(" cmdline is at P:0x%"PRIxLPADDR"\n",      (lpaddr_t)mb->cmdline);
    MSG(" cmdline reads '%s'\n", local_phys_to_mem((lpaddr_t)mb->cmdline));
    MSG(" mmap_length is 0x%"PRIu32"\n",                     mb->mmap_length);
    MSG(" mmap_addr is P:0x%"PRIxLPADDR"\n",       (lpaddr_t)mb->mmap_addr);
    MSG(" multiboot_flags is 0x%"PRIu32"\n",                 mb->flags);

#if 0
    if(cpu_is_bsp()) bsp_init( NULL );
    else nonbsp_init(NULL);
#endif

    MSG("Initializing exceptions.\n");

    if(is_bsp) {
        /* Map the exception vectors. */
        paging_map_vectors();
    }

    /* Initialise the exception stack pointers. */
    exceptions_load_stacks();

    /* Relocate the KCB into our new address space. */
    kcb_current= (struct kcb *)(lpaddr_t)core_data->kcb;
    MSG("KCB at %p\n", kcb_current);

    MSG("Parsing command line\n");
    init_cmdargs();
    parse_commandline((const char *)core_data->cmdline, cmdargs);
    kernel_timeslice = min(max(kernel_timeslice, 20), 1);

    errval = serial_debug_init();
    if (err_is_fail(errval)) {
        MSG("Failed to initialize debug port: %d", serial_debug_port);
    }
    MSG("Debug port initialized.\n");

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

#if 0
/**
 * \brief Initialization for the BSP (the first core to be booted). 
 * \param pointer address of \c multiboot_info
 */
static void bsp_init( void *pointer ) 
{
    struct multiboot_info *mb = pointer;
    
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
#endif
