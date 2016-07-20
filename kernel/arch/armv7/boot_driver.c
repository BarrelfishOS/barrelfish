/*
 * Copyright (c) 2009-2016, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.  If
 * you do not find this file, copies can be found by writing to: ETH Zurich
 * D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich, Attn: Systems Group.
 */

/**
 * \file
 * \brief CPU driver boot code for ARMv7A cores.
 */
#include <kernel.h>

#include <barrelfish_kpi/arm_core_data.h>
#include <cp15.h>
#include <dev/cpuid_arm_dev.h>
#include <getopt/getopt.h>
#include <global.h>
#include <kcb.h>
#include <multiboot.h>
#include <paging_kernel_arch.h>
#include <serial.h>
#include <stdio.h>

#define MSG(format, ...) printk( LOG_NOTE, "ARMv7-A: "format, ## __VA_ARGS__ )

void boot(void *pointer, void *cpu_driver_entry, void *cpu_driver_base);

extern char boot_start;

/* There is only one copy of the global locks, which is allocated alongside
 * the BSP kernel.  All kernels have their pointers set to the BSP copy. */
static struct global bsp_global __attribute__((section(".boot")));
struct global *global= &bsp_global;

/* The BSP core's KCB is allocated here.  Application cores will have theirs
 * allocated at user level. */
struct kcb bsp_kcb __attribute__((section(".boot")));

struct arm_core_data boot_core_data __attribute__((section(".boot")));

/* Print a little information about the processor, and check that it supports
 * the features we require. */
static bool check_cpuid(void) __attribute__((noinline));
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

extern char kernel_stack_top;

/* On many platforms, we have no way to set the argument register values when
 * starting the boot driver.  In that case, the static loader will place those
 * values here, which we'll detect by seeing that the multiboot pointer isn't
 * 0xdeadbeef. */
struct boot_arguments {
    void *pointer;
    void *cpu_driver_entry;
    void *cpu_driver_base;
} boot_arguments= { (void *)0xdeadbeef, NULL, NULL };

/* The boot driver needs the index of the console port, but nothing else.  The
 * argument list is left untouched, for the CPU driver. */
static struct cmdarg bootargs[] = {
    { "consolePort", ArgType_UInt, { .uinteger = (void *)0 } },
    { NULL, 0, { NULL } }
};

static void
init_bootargs(void) {
    bootargs[0].var.uinteger= &serial_console_port;
}

void switch_and_jump(lpaddr_t ram_base, size_t ram_size,
                     struct arm_core_data *boot_core_data,
                     void *cpu_driver_entry, void *cpu_driver_base,
                     lvaddr_t boot_pointer)
    __attribute__((noreturn));

/**
 * \brief Entry point called from boot.S for the kernel. 
 *
 * \param pointer address of \c multiboot_info on the BSP; or the
 * global structure if we're on an AP. 
 */
__attribute__((noreturn))
void boot(void *pointer, void *cpu_driver_entry, void *cpu_driver_base)
{
    /* If this pointer has been modified by the loader, it means we're got a
     * statically-allocated multiboot info structure, as we're executing from
     * ROM, in a simulator, or otherwise unable to use a full bootloader. */
    if(boot_arguments.pointer != (void *)0xdeadbeef) {
        pointer= boot_arguments.pointer;
        cpu_driver_entry= boot_arguments.cpu_driver_entry;
        cpu_driver_base= boot_arguments.cpu_driver_base;
    }

    /* Grab the multiboot header, so we can find our command line.  Note that
     * we're still executing with physical addresses, to we need to convert
     * the pointer back from the kernel-virtual address that the CPU driver
     * will use. */
    struct multiboot_info *mbi=
        (struct multiboot_info *)mem_to_local_phys((lvaddr_t)pointer);

    /* If there's no commandline passed, panic on port 0. */
    if(!(mbi->flags & MULTIBOOT_INFO_FLAG_HAS_CMDLINE)) {
        serial_early_init(0);
        panic("No commandline arguments.\n");
    }

    /* Parse the commandline, to find which console port to connect to. */
    init_bootargs();
    const char *cmdline= (const char *)mbi->cmdline;
    parse_commandline(cmdline, bootargs);

    /* Initialise the serial port driver using the physical address of the
     * port, so that we can start printing before we enable the MMU. */
    serial_early_init(serial_console_port);

    /* The spinlocks are in the BSS, and thus already zeroed, but it's polite
     * to explicitly initialize them here... */
    spinlock_init(&global->locks.print);

    MSG("Boot driver invoked as: %s\n", cmdline);

    /* These, likewise, use physical addresses directly. */
    check_cpuid();
    //platform_print_id();

    /* Print kernel address for debugging with gdb. */
    MSG("First byte of boot driver at 0x%"PRIxLVADDR"\n",
            local_phys_to_mem((uint32_t)&boot_start));
    MSG("First byte of CPU driver at %p\n", cpu_driver_base);

    /* Get the memory map. */
    if(!(mbi->flags & MULTIBOOT_INFO_FLAG_HAS_MMAP))
        panic("No memory map.\n");
    struct multiboot_mmap *mmap= (struct multiboot_mmap *)mbi->mmap_addr;
    if(mbi->mmap_length == 0) panic("Memory map is empty.\n");

    /* Find the first RAM region. */
    size_t region;
    for(region= 0;
        region < mbi->mmap_length &&
        mmap[region].type != MULTIBOOT_MEM_TYPE_RAM;
        region++);
    if(region == mbi->mmap_length) panic("No RAM regions.\n");

    /* Make sure there's some RAM we can use. */
    if(mmap[region].base_addr > (uint64_t)UINT32_MAX)
        panic("First RAM region is >4GB - I can't address it.\n");
    lpaddr_t ram_base= (uint32_t)mmap[region].base_addr;

    /* Truncate the region if necessary. */
    size_t ram_size;
    if(mmap[region].base_addr + (mmap[region].length - 1) >
       (uint64_t)UINT32_MAX) {
        ram_size=
            (uint32_t)((uint64_t)UINT32_MAX - mmap[region].base_addr + 1);
        printf("Truncated first RAM region to fit in 4GB.\n");
    }
    else ram_size= (uint32_t)mmap[region].length;

    if(ram_size > RAM_WINDOW_SIZE) {
        panic("Reduce the first MMAP entry to <1GB, otherwise everything\n"
              "breaks.  This is really dumb, and must be fixed.\n");
    }

    MSG("CPU driver entry point is %p\n", cpu_driver_entry);

    /* Fill in the boot data structure for the CPU driver. */
    /* We need to pass in anything we've allocated. */
    boot_core_data.multiboot_header= local_phys_to_mem((lpaddr_t)mbi);
    boot_core_data.global=           local_phys_to_mem((lpaddr_t)&bsp_global);
    boot_core_data.kcb=              local_phys_to_mem((lpaddr_t)&bsp_kcb);
    /* We're starting the BSP core, so its commandline etc. is that given in
     * the multiboot header. */
    assert(mbi->mods_count > 0);
    memcpy(&boot_core_data.kernel_module,
           (void *)mbi->mods_addr,
           sizeof(struct multiboot_modinfo));
    boot_core_data.cmdline= local_phys_to_mem(mbi->cmdline);

    /* Relocate the boot data pointer for the CPU driver. */
    lvaddr_t boot_pointer= local_phys_to_mem((lpaddr_t)&boot_core_data);
    MSG("Boot data structure at kernel VA %08x\n", boot_pointer);

    MSG("Switching page tables and jumping - see you in arch_init().\n");
    switch_and_jump(ram_base, ram_size, &boot_core_data,
                    cpu_driver_entry, cpu_driver_base, boot_pointer);
}

void
switch_and_jump(lpaddr_t ram_base, size_t ram_size,
                struct arm_core_data *core_data,
                void *cpu_driver_entry, void *cpu_driver_base,
                lvaddr_t boot_pointer) {
    /* Create the kernel page tables. */
    paging_init(ram_base, ram_size, core_data);

    /* We're now executing with the kernel window mapped.  If we're on a
     * platform that doesn't have RAM at 0x80000000, then we're still using
     * physical addresses through the uncached device mappings in TTBR0.
     * Otherwise our physical addresses were mapped 1-1 by the kernel window.
     *
     * In either case, we can't safely use the UART, as there's no guarantee
     * that its device registers weren't in what is now the kernel window
     * (this is the case on the Zynq). */

    /* This is important.  We need to clean and invalidate the data caches, to
     * ensure that anything we've modified since enabling them is visible
     * after we make the jump. */
    invalidate_data_caches_pouu(true);

    /* Long jump to the CPU driver entry point, passing the kernel-virtual
     * address of the boot_core_data structure. */
    __asm("mov r0, %[pointer]\n"
          "mov pc, %[jump_target]\n"
          : /* No outputs */
          : [jump_target] "r"(cpu_driver_entry),
            [pointer]     "r"(boot_pointer)
          : "r0");

    panic("Shut up GCC, I'm not returning.\n");
}
