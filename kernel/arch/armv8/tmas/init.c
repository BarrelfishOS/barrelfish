/*
 * Copyright (c) 2016, ETH Zurich.
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

static struct global global_temp;

static inline int islower(int c) {
    return 'a' <= c && c <= 'z';
}

static void parse_cmd_line(char* cmd_line) {
    char *p = cmd_line;
    while (*p) {
        if (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r') {
            p++;
            continue;
        }
        char *e = p;
        while (*e && (islower((int) *e) || *e == '=')) {
            e++;
        }

        if (strncmp("earlycon=", p, 9) == 0) {
            lpaddr_t base = strtoul(e, &p, 16);
            assert(base);
            platform_set_uart_address(base);
        } else if (strncmp("gicv2=", p, 6) == 0) {
            lpaddr_t dist_base = strtoul(e, &p, 16);
            assert(dist_base);
            assert(*e);
            assert(*e == ',');
            e++;
            lpaddr_t cpu_base = strtoul(e, &p, 16);
            assert(cpu_base);
            platform_set_gic_cpu_address(cpu_base);
            platform_set_distributor_address(dist_base);
        } else {
            while (*e  && (*e != ' ')) {
                e++;
            }
        }
        p = e;
    }
}

static void
mmap_find_memory(struct multiboot_tag_efi_mmap *mmap) {
    lpaddr_t physical_mem = 0;
    uint64_t pages = 512;
    for (size_t i = 0; i < (mmap->size - sizeof(struct multiboot_tag_efi_mmap)) / mmap->descr_size; i++) {
        efi_memory_descriptor *desc = (efi_memory_descriptor *)(mmap->efi_mmap + mmap->descr_size * i);
        if (desc->Type == EfiConventionalMemory && desc->NumberOfPages > pages) {
            physical_mem = desc->PhysicalStart;
            pages = desc->NumberOfPages;
        }
    }
    if (!physical_mem) {
        panic("No free memory found!\n");
    } else {
        glbl_core_data = (void*) local_phys_to_mem(physical_mem);
        glbl_core_data->start_kernel_ram = physical_mem;
        glbl_core_data->start_free_ram = physical_mem + sizeof(*glbl_core_data);

        global = (void*) local_phys_to_mem(glbl_core_data->start_free_ram);
        // Construct the global structure
        memset(&global->locks, 0, sizeof(global->locks));

        glbl_core_data->start_free_ram += sizeof(*global);
        glbl_core_data->start_free_ram = ROUND_UP(glbl_core_data->start_free_ram, BASE_PAGE_SIZE);
    }
    printf("%s:%d glbl_core_data=%p\n", __FUNCTION__, __LINE__, glbl_core_data);
}

void
arch_init(uint32_t magic, void *pointer, uintptr_t stack) {
    global = &global_temp;
    memset(&global->locks, 0, sizeof(global->locks));

    switch (magic) {
    case MULTIBOOT2_BOOTLOADER_MAGIC: {
        // pointer contains multiboot 2 image
        uint32_t size = *(uint32_t *) pointer;
        // skip size and reserved fields
        struct multiboot_header_tag *mb = pointer + 2*sizeof(uint32_t);

        struct multiboot_tag_string *kernel_cmd = (struct multiboot_tag_string *)
                multiboot2_find_header(mb, size, MULTIBOOT_TAG_TYPE_CMDLINE);

        parse_cmd_line(kernel_cmd->string);

        serial_early_init(0);
        serial_console_init(false);
        printf("Serial initialised.\n");

        struct multiboot_tag_efi_mmap *mmap = (struct multiboot_tag_efi_mmap *)
                multiboot2_find_header(mb, size, MULTIBOOT_TAG_TYPE_EFI_MMAP);
        if (!mmap) {
            panic("Multiboot image does not have EFI mmap!");
        } else {
            printf("Found EFI mmap: %p\n", mmap);
        }

        mmap_find_memory(mmap);
        glbl_core_data->multiboot2 = mem_to_local_phys((lvaddr_t) mb);
        glbl_core_data->multiboot2_size = size;
        glbl_core_data->efi_mmap = mem_to_local_phys((lvaddr_t) mmap);
        kernel_stack = stack;

        break;
    }
    default: {
        panic("Implement AP booting!");
        break;
    }
    }

    gic_init();

    printf("Exception vectors (VBAR_EL1): %p\n", &vectors);
    sysreg_write_vbar_el1((uint64_t)&vectors);


    arm_kernel_startup();
    while (1);
}
