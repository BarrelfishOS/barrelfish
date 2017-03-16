/**
 * \file
 * \brief Data sent to a newly booted kernel
 */

/*
 * Copyright (c) 2012, 2017 ETH Zurich.
 * Copyright (c) 2015, 2016 Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _AARCH64_COREDATA_H
#define _AARCH64_COREDATA_H


struct armv8_coredata_elf {
    uint32_t    num;
    uint32_t    size;
    uint32_t    addr;
    uint32_t    shndx;
};

#define ARMV8_BOOTMAGIC_PSCI 0xb001b001
#define ARMV8_BOOTMAGIC_PARKING 0xb001b002

struct armv8_coredata_memreg
{
    genpaddr_t base;
    gensize_t length;
};

/**
 * \brief Data sent to a newly booted kernel
 *
 */
struct armv8_core_data {

    /**
     * ARMv8 Boot magic field. Contains the value ARMV8_BOOTMAGIC_*
     */
    uint64_t boot_magic;

    /**
     * Physical address of the kernel stack
     */
    genpaddr_t cpu_driver_stack;

    /**
     * Physical address of the kernel stack
     */
    genpaddr_t cpu_driver_stack_limit;

    /**
     * Physical address of the global data structure shared by all
     */
    genpaddr_t cpu_driver_globals_pointer;

    /**
     * CPU Driver entry point
     */
    genvaddr_t cpu_driver_entry;

    /**
     * CPU driver command line arguments
     */
    char cpu_driver_cmdline[128];

    /**
     * Physical address of the L0 page table in memory
     */
    genpaddr_t page_table_root;

    /**
     * Memory region to be used for the new CPU driver's allocations
     */
    struct armv8_coredata_memreg memory;

    /**
     * Memory region to be used for the new CPU driver's allocations
     */
    struct armv8_coredata_memreg urpc_frame;

    /**
     * Memory region to be used for the new CPU driver's allocations
     */
    struct armv8_coredata_memreg monitor_binary;

    /**
     * memory region of the multiboot image
     */
    struct armv8_coredata_memreg multiboot_image;

    lpaddr_t efi_mmap;

    uint64_t    start_kernel_ram; ///< The physical start of allocated kernel memory
    uint64_t    start_free_ram; ///< The physical start of free ram for the bsp allocator

    uint32_t    chan_id;

    genpaddr_t kcb; ///< The kernel control block


    coreid_t src_core_id;
    coreid_t dst_core_id;
    hwid_t src_arch_id;
    hwid_t dst_arch_id;


};

#include <barrelfish_kpi/paging_arch.h>

STATIC_ASSERT(sizeof(struct armv8_core_data) < BASE_PAGE_SIZE,
        "Core Data structure must not exceed page size");



#define ARMV8_CORE_DATA_PAGES 700


#endif
