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

#include <cp15.h>
#include <dev/cpuid_arm_dev.h>
#include <getopt/getopt.h>
#include <global.h>
#include <multiboot.h>
#include <serial.h>
#include <stdio.h>

#define MSG(format, ...) printk( LOG_NOTE, "ARMv7-A: "format, ## __VA_ARGS__ )

void paging_init(void);
void boot(void *pointer);

/* There is only one copy of the global locks, which is allocated alongside
 * the BSP kernel.  All kernels have their pointers set to the BSP copy, which
 * means we waste a little space (4 bytes) on each additional core. */
static struct global bsp_global;

struct global *global= &bsp_global;

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

/* We need to update the GOT base pointer if arch_init_2() isn't at its
 * physical address. */
static inline uint32_t
get_got_base(void) {
    uint32_t got_base;
    __asm("mov %0, "XTR(PIC_REGISTER) : "=r"(got_base));
    return got_base;
}

static inline void
set_got_base(uint32_t got_base) {
    __asm("mov "XTR(PIC_REGISTER)", %0" : : "r"(got_base));
}

extern char kernel_stack_top;

void *static_multiboot= (void *)0xdeadbeef;

#if 0
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
#endif

/**
 * \brief Entry point called from boot.S for the kernel. 
 *
 * \param pointer address of \c multiboot_info on the BSP; or the
 * global structure if we're on an AP. 
 */
void boot(void *pointer)
{
    /* If this pointer has been modified by the loader, it means we're got a
     * statically-allocated multiboot info structure, as we're executing from
     * ROM, in a simulator, or otherwise unable to use a proper bootloader. */
    if(static_multiboot != (void *)0xdeadbeef)
        pointer= static_multiboot;

    /* Initialise the serial port driver using the physical address of the
     * port, so that we can start printing before we enable the MMU. */
    //serial_early_init(serial_console_port); XXX
    serial_early_init(0);

    struct multiboot_info *mbi=
        (struct multiboot_info *)mem_to_local_phys((lvaddr_t)pointer);
    printf("%s\n", (const char *)mbi->cmdline);

    /* These, likewise, use physical addresses directly. */
    check_cpuid();
    //platform_print_id();

    /* Print kernel address for debugging with gdb. */
    MSG("First byte of kernel at 0x%"PRIxLVADDR"\n",
            local_phys_to_mem((uint32_t)&kernel_first_byte));

    MSG("Initialising paging...\n");
    paging_init();

    while(1);

#if 0
    /* We can't safely use the UART until we're inside arch_init_2(). */

    /* If we're on a platform that doesn't have RAM at 0x80000000, then we
     * need to jump into the kernel window (we'll be currently executing in
     * physical addresses through the uncached device mappings in TTBR0).
     * Therefore, the call to arch_init_2() needs to be a long jump, and we
     * need to relocate all references to physical addresses into the virtual
     * kernel window.  These are the references that exist at this point, and
     * will be visible within arch_init_2():
     *      - pointer: must be relocated.
     *      - GOT (r9): the global offset table must be relocated.
     *      - LR: discarded as we don't return.
     *      - SP: we'll reset the stack, as this frame will become
     *            meaningless.
     */

    /* Relocate the boot info pointer. */
    pointer= (void *)local_phys_to_mem((lpaddr_t)pointer);
    /* Relocate the GOT. */
    set_got_base(local_phys_to_mem(get_got_base()));
    /* Calculate the jump target (relocate arch_init_2). */
    lvaddr_t jump_target= local_phys_to_mem((lpaddr_t)&arch_init_2);
    /* Relocate the kernel stack. */
    lvaddr_t stack_top= local_phys_to_mem((lpaddr_t)&kernel_stack_top);

    /* Note that the above relocations are NOPs if phys_memory_start=2GB. */

    /* This is important.  We need to clean and invalidate the data caches, to
     * ensure that anything we've modified since enabling them is visible
     * after we make the jump. */
    invalidate_data_caches_pouu(true);

    /* Perform the long jump, resetting the stack pointer. */
    __asm("mov r0, %[pointer]\n"
          "mov sp, %[stack_top]\n"
          "mov pc, %[jump_target]\n"
          : /* No outputs */
          : [jump_target] "r"(jump_target),
            [pointer]     "r"(pointer),
            [stack_top]   "r"(stack_top)
          : "r0", "r1");

    panic("Shut up GCC, I'm not returning.\n");
#endif
}
