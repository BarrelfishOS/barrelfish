/*
 * Copyright (c) 2016, ETH Zurich.
 * Copyright (c) 2016, Hewlett Packard Enterprise Development LP.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <serial.h>
#include <offsets.h>
#include <stdio.h>
#include <stddef.h>
#include <errno.h>

#include <multiboot2.h>

// parsing commandline arguments
#include <getopt/getopt.h>

#include <barrelfish_kpi/arm_core_data.h>

#include <arch/arm/gic.h>
#include <arch/armv8/arm_hal.h>
#include <arch/armv8/init.h>
#include <arch/armv8/exceptions.h>
#include <arch/armv8/global.h>
#include <arch/armv8/startup_arch.h>
#include <efi.h>
#include <sysreg.h>
#include <arch/armv8/kernel_multiboot2.h>
#include <arch/armv8/paging_kernel_arch.h>
#include <arch/armv8/platform.h>
#include <systime.h>
#include <coreboot.h>

static struct global global_temp;

/*
 * Need to be initialized during kernel loading.
 */
struct armv8_core_data *armv8_glbl_core_data = NULL;

lpaddr_t kernel_stack = 0;
lpaddr_t kernel_stack_top = 0;

#define MSG(format, ...) printk( LOG_NOTE, "ARMv8-A: "format, ## __VA_ARGS__ )

/*
 * parsing of command line arguments
 */
static struct cmdarg cmdargs[] = {
    {"loglevel", ArgType_Int, { .integer = &kernel_loglevel }},
    {"logmask", ArgType_Int, { .integer = &kernel_log_subsystem_mask }},
    {"ticks", ArgType_Bool, { .boolean = &kernel_ticks_enabled }},
    {"timeslice", ArgType_UInt, { .uinteger = &config_timeslice }},
    {"serial", ArgType_ULong, { .ulonginteger = &uart_base[0] }},
    {"gic", ArgType_ULong, { .ulonginteger = &platform_gic_cpu_base }},
    {"gicdist", ArgType_ULong, { .ulonginteger = &platform_gic_dist_base }},
    {NULL, 0, {NULL}}
};

static void mmap_find_memory(struct multiboot_tag_efi_mmap *mmap)
{
    lpaddr_t physical_mem = 0;
    uint64_t pages = ARMV8_CORE_DATA_PAGES;

    for (size_t i = 0; i < mmap->size; i += mmap->descr_size) {
        efi_memory_descriptor *desc = (efi_memory_descriptor *)(mmap->efi_mmap + i);
        if (desc->Type == EfiConventionalMemory && desc->NumberOfPages > pages) {
            physical_mem = ROUND_UP(desc->PhysicalStart, BASE_PAGE_SIZE);
            pages = desc->NumberOfPages;
        }
    }

    if (!physical_mem) {
        panic("No free memory found!\n");
    }

    armv8_glbl_core_data = (void*) local_phys_to_mem(physical_mem);
    armv8_glbl_core_data->start_kernel_ram = physical_mem;
    armv8_glbl_core_data->start_free_ram = physical_mem + sizeof(*armv8_glbl_core_data);

    global = (void*) local_phys_to_mem(armv8_glbl_core_data->start_free_ram);

    // Construct the global structure
    memset(&global->locks, 0, sizeof(global->locks));

    armv8_glbl_core_data->start_free_ram += sizeof(*global);
    armv8_glbl_core_data->start_free_ram = ROUND_UP(armv8_glbl_core_data->start_free_ram, BASE_PAGE_SIZE);

}

bool cpu_is_bsp(void)
{
    /* xxx: assumes the coreid to be set */
    return (my_core_id == 0);
}

bool arch_core_is_bsp(void)
{
    return cpu_is_bsp();
}

/**
 * @param Entry point to architecture specific initialization
 *
 * @param magic     Magic value to tell the kernel it was started by multiboot
 * @param pointer   Pointer to the multiboot structure
 * @param stack     Pointer to the stack
 *
 * ASSUMPTIONS:
 *   - the execution starts in HIGH addresses (e.g. > KERNEL_OFFSET)
 *   - Pointers to stack and multiboot structures point to HIGH memory
 *   - ARM exception level is EL1 (privileged)
 */
void
arch_init(uint32_t magic, void *pointer, uintptr_t stack) {
    global = &global_temp;
    memset(&global->locks, 0, sizeof(global->locks));

    switch (magic) {
    case MULTIBOOT2_BOOTLOADER_MAGIC:
        {
        my_core_id = 0;

        struct multiboot_header *mbhdr = pointer;
        uint32_t size = mbhdr->header_length;

        // sanity checks
        assert(mbhdr->architecture == MULTIBOOT_ARCHITECTURE_AARCH64);
        assert((mbhdr->architecture + mbhdr->checksum + mbhdr->header_length
                 + mbhdr->magic) == 0);

        struct multiboot_header_tag *mb;
        struct multiboot_tag_string *kernel_cmd;

        // get the first header tag
        mb = (struct multiboot_header_tag *)(mbhdr + 1);

        // get the kernel cmdline. this may contain address which UART/GIC to use
        kernel_cmd = multiboot2_find_cmdline(mb, size);
        if (kernel_cmd == NULL) {
            panic("Multiboot did not contain an kernel CMD line\n");
        }

        // parse the cmdline
        parse_commandline(kernel_cmd->string, cmdargs);

        // initialize the serial console.
        serial_init(serial_console_port, false);
//        serial_console_init(false);

        struct multiboot_tag_efi_mmap *mmap = (struct multiboot_tag_efi_mmap *)
                multiboot2_find_header(mb, size, MULTIBOOT_TAG_TYPE_EFI_MMAP);
        if (!mmap) {
            panic("Multiboot image does not have EFI mmap!");
        } else {
            printf("Found EFI mmap: %p\n", mmap);
        }

        mmap_find_memory(mmap);

        armv8_glbl_core_data->multiboot_image.base  = mem_to_local_phys((lvaddr_t) mb);
        armv8_glbl_core_data->multiboot_image.length = size;
        armv8_glbl_core_data->efi_mmap = mem_to_local_phys((lvaddr_t) mmap);

        armv8_glbl_core_data->cpu_driver_stack = stack;

        kernel_stack = stack;
        kernel_stack_top = stack + 16 - KERNEL_STACK_SIZE;
        break;
    }
    case ARMV8_BOOTMAGIC_PSCI :
        //serial_init(serial_console_port, false);

        serial_init(serial_console_port, false);

        struct armv8_core_data *core_data = (struct armv8_core_data*)pointer;
        armv8_glbl_core_data = core_data;
        global = (struct global *)core_data->cpu_driver_globals_pointer;

        kernel_stack = stack;
        kernel_stack_top = local_phys_to_mem(core_data->cpu_driver_stack_limit);

        my_core_id = core_data->dst_core_id;

        MSG("ARMv8 Core magic...\n");

        break;
    default: {
        serial_init(serial_console_port, false);

        serial_console_putchar('x');
        serial_console_putchar('x');
        serial_console_putchar('\n');

        panic("Implement AP booting!");
        __asm volatile ("wfi":::);
        break;
    }
    }


    MSG("Barrelfish CPU driver starting on ARMv8\n");
    MSG("Global data at %p\n", global);
    MSG("Multiboot record at %p\n", pointer);
    MSG("Kernel stack at 0x%016" PRIxPTR ".. 0x%016" PRIxPTR "\n",
        kernel_stack_top, kernel_stack);
    MSG("Kernel first byte at 0x%" PRIxPTR "\n", &kernel_first_byte);

    MSG("Exception vectors (VBAR_EL1): %p\n", &vectors);
    sysreg_write_vbar_el1((uint64_t)&vectors);

    platform_gic_init();

    MSG("Setting coreboot spawn handler\n");
    coreboot_set_spawn_handler(CPU_ARM8, platform_boot_core);

    arm_kernel_startup(pointer);
    while (1) {
        __asm volatile ("wfi":::);
    }
}

