/*
 * Copyright (c) 2016, ETH Zurich.
 * Copyright (c) 2016, Hewlett Packard Enterprise Development LP.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
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

#include <arch/armv8/arm_hal.h>
#include <arch/armv8/init.h>
#include <arch/armv8/exceptions.h>
#include <arch/armv8/global.h>
#include <arch/armv8/startup_arch.h>
#include <efi.h>
#include <sysreg.h>
#include <arch/armv8/kernel_multiboot2.h>
#include <arch/armv8/paging_kernel_arch.h>
#include <arch/arm/platform.h>
#include <systime.h>
#include <coreboot.h>
#include <dev/armv8_dev.h>

static struct global global_temp;

/*
 * Need to be initialized during kernel loading.
 */
struct armv8_core_data *armv8_glbl_core_data = NULL;

lpaddr_t kernel_stack = 0;
lpaddr_t kernel_stack_top = 0;

const char *kernel_command_line;

#define MSG(format, ...) printk( LOG_NOTE, "ARMv8-A: "format, ## __VA_ARGS__ )

/*
 * parsing of command line arguments
 */
static struct cmdarg cmdargs[] = {
    {"loglevel", ArgType_Int, { .integer = &kernel_loglevel }},
    {"logmask", ArgType_Int, { .integer = &kernel_log_subsystem_mask }},
    {"ticks", ArgType_Bool, { .boolean = &kernel_ticks_enabled }},
    {"timeslice", ArgType_UInt, { .uinteger = &config_timeslice }},
    {"serial", ArgType_ULong, { .ulonginteger = &platform_uart_base[0] }},
    {NULL, 0, {NULL}}
};

static void mmap_find_memory(struct multiboot_tag_efi_mmap *mmap)
{
    lpaddr_t physical_mem = 0;

    for (size_t i = 0; i < mmap->size; i += mmap->descr_size) {
        efi_memory_descriptor *desc = (efi_memory_descriptor *)(mmap->efi_mmap + i);
        if (desc->Type == EfiConventionalMemory && desc->NumberOfPages > ARMV8_CORE_DATA_PAGES) {
            physical_mem = ROUND_UP(desc->PhysicalStart, BASE_PAGE_SIZE);
            break;
        }
    }

    if (!physical_mem) {
        panic("No free memory found!\n");
    }

    armv8_glbl_core_data->start_kernel_ram = physical_mem;
    armv8_glbl_core_data->start_free_ram = physical_mem;

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
 * @param pointer   Pointer to the ARMv8 core data
 *
 * ASSUMPTIONS:
 *   - the execution starts in HIGH addresses (e.g. > KERNEL_OFFSET)
 *   - Pointers to stack and multiboot structures point to HIGH memory
 *   - ARM exception level is EL1 (privileged)
 */
void
arch_init(struct armv8_core_data *core_data) {
    global = &global_temp;
    memset(&global->locks, 0, sizeof(global->locks));

    armv8_glbl_core_data = core_data;

    my_core_id = armv8_glbl_core_data->dst_core_id;

    /* parse the cmdline */
    kernel_command_line = (const char *)armv8_glbl_core_data->cpu_driver_cmdline;
    parse_commandline(kernel_command_line, cmdargs);

    /* initialize the serial console */
    serial_console_init(false);

    /* store the stack pointers */
    kernel_stack = local_phys_to_mem(core_data->cpu_driver_stack);
    kernel_stack_top = local_phys_to_mem(core_data->cpu_driver_stack_limit);

    uint8_t mpidr_aff0 = armv8_MPIDR_EL1_Aff0_rdf(NULL);
    uint8_t mpidr_aff1 = armv8_MPIDR_EL1_Aff1_rdf(NULL);
    uint8_t mpidr_aff2 = armv8_MPIDR_EL1_Aff2_rdf(NULL);
    uint8_t mpidr_aff3 = armv8_MPIDR_EL1_Aff3_rdf(NULL);

    switch (armv8_glbl_core_data->boot_magic) {
        case ARMV8_BOOTMAGIC_BSP:
            assert(my_core_id == 0);
            printf("Barrelfish CPU driver starting on ARMv8 (BSP)\n");

            struct multiboot_info *multiboot = (struct multiboot_info *)
                local_phys_to_mem(armv8_glbl_core_data->multiboot_image.base);
            struct multiboot_tag_efi_mmap *mmap = (struct multiboot_tag_efi_mmap *)
                multiboot2_find_tag(multiboot->tags, multiboot->total_size - 8, MULTIBOOT_TAG_TYPE_EFI_MMAP);

            mmap_find_memory(mmap);
            break;
        case ARMV8_BOOTMAGIC_PSCI :
        case ARMV8_BOOTMAGIC_PARKING :
            assert(my_core_id != 0);

            global = (struct global *)core_data->cpu_driver_globals_pointer;

            printf("Barrelfish CPU driver starting on ARMv8 (APP) "
                    "mpid=%"PRIu8":%"PRIu8":%"PRIu8":%"PRIu8"\n",
                    mpidr_aff3, mpidr_aff2, mpidr_aff1, mpidr_aff0);

            break;
        default: {
            serial_console_putchar('x');
            serial_console_putchar('x');
            serial_console_putchar('\n');

            panic("Implement AP booting!");
            __asm volatile ("wfi":::);
            break;
        }
    }

    MSG("Global data at %p\n", global);

    MSG("Kernel stack at 0x%016" PRIxPTR ".. 0x%016" PRIxPTR "\n",
        kernel_stack_top, kernel_stack);
    MSG("Kernel first byte at 0x%" PRIxPTR "\n", &kernel_first_byte);

    MSG("Exception vectors (VBAR_EL1): %p\n", &vectors);
    sysreg_write_vbar_el1((uint64_t)&vectors);

    if (cpu_is_bsp()) {
        platform_revision_init();
	    MSG("%"PRIu32" cores in system\n", platform_get_core_count());
        MSG("Initializing the interrupt controller\n");
        platform_init_ic_bsp();
    } else {
        platform_init_ic_app();
    }

    MSG("Enabling timers\n");
    platform_timer_init(config_timeslice);

    MSG("Setting coreboot spawn handler\n");
    coreboot_set_spawn_handler(CPU_ARM8, platform_boot_core);

    MSG("Calling arm_kernel_startup\n");
    arm_kernel_startup();
    while (1) {
        __asm volatile ("wfi":::);
    }
}
