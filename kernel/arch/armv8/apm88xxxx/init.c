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
/* XXX - shouldn't be here. */
bool is_bsp;

//__attribute__((noreturn))
//void arch_init(uint32_t magic, void *pointer, uintptr_t stack);

static struct global global_temp;

static inline int islower(int c) {
    return 'a' <= c && c <= 'z';
}

static inline int isnumber(int c) {
    return '0' <= c && c <= '9';
}

#define stop(x) { if (!(x)) { while (1) { } } }

#if 0
static void parse_cmd_line(char* cmd_line) {
    char *p = cmd_line;
    while (*p) {
        if (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r') {
            p++;
            continue;
        }
        char *e = p;
        while (*e && (islower((int) *e) || isnumber((int) *e))) {
            e++;
        }
        if (*e && *e == '=') {
            e++;
            if (strncmp("earlycon=", p, 9) == 0) {
                lpaddr_t base = strtoul(e, &p, 16);
                stop(base);
                platform_set_uart_address(base);
            } else if (strncmp("gicv2=", p, 6) == 0) {
                lpaddr_t dist_base = strtoul(e, &p, 16);
                stop(dist_base);
                stop(*p);
                stop(*p == ',');
                p++;
                lpaddr_t cpu_base = strtoul(p, &p, 16);
                stop(cpu_base);
                platform_set_gic_cpu_address(cpu_base);
                platform_set_distributor_address(dist_base);
                e = p;
            }
        }
        while (*e && (*e != ' ')) {
            e++;
        }
        p = e;
    }
}
#endif


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


/* Hagfish guarantees us the following on calling plat_init():
    * Single core running (not guaranteed to be core 0)
    * CPU is in highest non-secure privilege mode: EL2 or EL1
    * MMU enabled, 4k translation granule, 1:1 mapping of all RAM, using TTBR0.
    * Little-endian mode
    * Core caches (L1&L2) and TLB enabled
    * Non-architectural caches disabled
    * Interrupts enabled
    * Generic timer initialized and enabled
    * >= 128KiB stack
    * ACPI tables available
    * Register x0 contains handle to ACPI root table
    * Register x1 contains a pointer to the UEFI memory map
 */

void arch_init(uint32_t magic, void *pointer, uintptr_t stack) {

    assert(get_current_el() ==1);

    global = &global_temp;
    memset(&global->locks, 0, sizeof(global->locks));


    /* Uncomment the line below to wait here for GDB. */
    /* __asm volatile ("wfi":::); */

    /* Set both console ports: UART0 is the one that's connected to the DB9
       connector on the back of the Mustang boxes. */
    /* XXX - we should get EFI to tell us this. */
    serial_console_port= 0;
    serial_debug_port=   0;

    /* Initialise the serial console. Skip hardware initialisation, as the
       port is guaranteed to have been initialized by UEFI. */
    serial_console_init(false);

    switch(magic) {
    case MULTIBOOT2_BOOTLOADER_MAGIC:
        is_bsp= true;

        // pointer contains multiboot 2 image
        uint32_t size = *(uint32_t *) pointer;
        // skip size and reserved fields
        struct multiboot_header_tag *mb = pointer + 2*sizeof(uint32_t);

        //struct multiboot_tag_string *kernel_cmd = (struct multiboot_tag_string *)
        //        multiboot2_find_header(mb, size, MULTIBOOT_TAG_TYPE_CMDLINE);

        //parse_cmd_line(kernel_cmd->string);

        struct multiboot_tag_efi_mmap *mmap = (struct multiboot_tag_efi_mmap *)
                multiboot2_find_header(mb, size, MULTIBOOT_TAG_TYPE_EFI_MMAP);

        if (!mmap) {
            panic("Multiboot image does not have EFI mmap!");
        } else {
            printf("Found EFI mmap: %p\n", mmap);
        }

        printf("%s:%u\n", __FUNCTION__, __LINE__);

        mmap_find_memory(mmap);

        glbl_core_data->multiboot2 = mem_to_local_phys((lvaddr_t) mb);
        glbl_core_data->multiboot2_size = size;
        glbl_core_data->efi_mmap = mem_to_local_phys((lvaddr_t) mmap);

        kernel_stack = stack;
        break;
    default:
        is_bsp= false;
        panic("Implement AP booting!");
        break;
    }


    if (is_bsp) {
        // TODO: finish BSP core init
    } else {
        // TODO: AP core init
        panic("AP init");
    }

    printf("%s:%u\n", __FUNCTION__, __LINE__);


    printf("Barrelfish APM88xxxx CPU driver starting at addr 0x%"
            PRIxLVADDR" on core %"PRIuCOREID"\n",
            //local_phys_to_mem((lpaddr_t)&kernel_first_byte), my_core_id);
            ((lpaddr_t)&kernel_first_byte), my_core_id);

    printf("Exception vectors (VBAR_EL1): %p\n", &vectors);
    sysreg_write_vbar_el1((uint64_t)&vectors);


    gic_init();


    arm_kernel_startup();

    while(1) {
        __asm volatile ("wfi":::);
    }
}
