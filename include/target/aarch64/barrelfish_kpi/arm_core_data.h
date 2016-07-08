/**
 * \file
 * \brief Data sent to a newly booted kernel
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * Copyright (c) 2015, 2016 Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef COREDATA_H
#define COREDATA_H

struct arm_coredata_modinfo {
    uint32_t    mod_start;
    uint32_t    mod_end;
    uint32_t    string;
    uint32_t    reserved;
};

struct arm_coredata_mmap {
    uint32_t    size;
	//uint32_t    res;
    uint64_t    base_addr;
    uint64_t    length;
    uint32_t    type;
} __attribute__ ((packed));

struct arm_coredata_elf {
    uint32_t    num;
    uint32_t    size;
    uint32_t    addr;
    uint32_t    shndx;
};

/**
 * \brief Data sent to a newly booted kernel
 *
 */
struct arm_core_data {
    lpaddr_t multiboot2; ///< The physical multiboot2 location
    uint64_t multiboot2_size;
    lpaddr_t efi_mmap;
    uint32_t module_start;  ///< The start of the cpu module
    uint32_t module_end;    ///< The end of the cpu module
    uint32_t urpc_frame_base;
    uint8_t urpc_frame_bits;
    uint32_t monitor_binary;
    uint32_t monitor_binary_size;
    uint32_t memory_base_start;
    uint8_t memory_bits;
    coreid_t src_core_id;
    uint8_t src_arch_id;
    coreid_t dst_core_id;
    char kernel_cmdline[128];

    uint32_t    initrd_start;
    uint32_t	initrd_size;


    uint64_t    start_kernel_ram; ///< The physical start of allocated kernel memory
    uint64_t    start_free_ram; ///< The physical start of free ram for the bsp allocator

    uint32_t    chan_id;

    genpaddr_t kcb; ///< The kernel control block
}; //__attribute__ ((packed));

#define ARM_CORE_DATA_PAGES 	1100

#endif
