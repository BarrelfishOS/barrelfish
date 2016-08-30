/**
 * \file
 * \brief Data sent to a newly booted ARM kernel
 */

/*
 * Copyright (c) 2012,2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.  If
 * you do not find this file, copies can be found by writing to: ETH Zurich
 * D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich, Attn: Systems Group.
 */

#ifndef COREDATA_H
#define COREDATA_H

#include <multiboot.h>

#define MAXCMDLINE 128

#define MAX_BUILD_ID 32

struct gnu_build_id {
    char data[MAX_BUILD_ID];
    size_t length;
};

/**
 * \brief Data sent to a newly booted kernel
 *
 */
struct arm_core_data {
    /* The physical address of the multiboot header. */
    lvaddr_t multiboot_header;

    /* The kernel page tables. */
    /* XXX - we can drop L1_low. */
    lpaddr_t kernel_l1_low, kernel_l1_high, kernel_l2_vec;

    /* The CPU driver entry point. */
    lvaddr_t entry_point;

    /* The CPU driver's stack bounds. */
    lpaddr_t stack_base, stack_top;

    /* The module information and ELF section headers for the image used to
     * boot this kernel. n.b. this may not be one of the modules in the
     * initial multiboot image. */
    struct multiboot_modinfo kernel_module;
    struct multiboot_elf kernel_elf;

    /* The preallocated kernel control block for the new core. */
    lvaddr_t kcb;

    /* The kernel command line. Again, this may differ from that passed to the
     * BSP kernel. */
    char cmdline_buf[MAXCMDLINE];

    /* This may point to the preceeding buffer, or into the multiboot image,
     * if the commandline hasn't been modified. */
    lvaddr_t cmdline;

    /* Preallocated monitor channel. */
    uint32_t urpc_frame_base;
    uint32_t urpc_frame_size;
    uint32_t chan_id;

    /* Monitor to start. */
    struct multiboot_modinfo monitor_module;

    /* The contiguous memory region into which this kernel should allocate the
     * initial process' structures. */
    uint32_t memory_base_start;
    uint32_t memory_bytes;

    /* The core that booted us. */
    coreid_t src_core_id;

    /* Our core ID, as assigned by the booting core. */
    coreid_t dst_core_id;

    /* The architecture of the core that booted us. */
    uint8_t src_arch_id;

    /* The address of the global locks. */
    lvaddr_t global;

    /* The address of the mailboxes within the boot driver. */
    lvaddr_t target_bootrecs;

    /* The GNU build ID (SHA-1 hash), identifying the CPU driver binary. */
    struct gnu_build_id build_id;

    /* The load address of the non-relocatable portion of the image. */
    lvaddr_t kernel_load_base;

    /* The kernel-virtual address of the global offset table. */
    lvaddr_t got_base;
};

#define ARM_CORE_DATA_PAGES 	1100

#endif
