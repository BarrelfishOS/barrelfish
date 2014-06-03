/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_XEON_PHI_H_
#define XEON_PHI_XEON_PHI_H_

/// The maximum number of coprocessor cards in a system
#define XEON_PHI_NUM_MAX 8

struct xeon_phi_boot_params
{
    uint8_t reserved[0x54];
    uint64_t tboot_addr; /* 0x058 */
    uint8_t _pad3[128]; /* 0x070 */
    uint8_t dummy[256];
    uint8_t _pad1[4];
    uint32_t scratch; /* Scratch field! *//* 0x1e4 */
    uint8_t _pad6[13]; /* 0x1eb */
    uint8_t setup_sects;    /// must be at this very location !!!
    uint16_t root_flags;
    uint32_t syssize;       /// must be at this very location !!!
    uint16_t _pad2[5];
    uint32_t header;        /// must be at this very location !!!
    uint16_t version;
    uint32_t realmode_swtch;
    uint16_t start_sys;
    uint16_t kernel_version;
    uint8_t type_of_loader;
    uint8_t loadflags;
    uint16_t setup_move_size;
    uint32_t code32_start;
    uint32_t ramdisk_image; /// pointer to the multiboot image
    uint32_t ramdisk_size;  /// multiboot image size
    uint32_t bootsect_kludge;
    uint16_t heap_end_ptr;
    uint8_t ext_loader_ver;
    uint8_t ext_loader_type;
    uint32_t payload_offset;
    uint32_t initrd_addr_max;
    uint32_t kernel_alignment;
    uint8_t _pad4[4];
    uint32_t cmdline_size;  /// size of the command line
    uint32_t hardware_subarch;
    uint64_t hardware_subarch_data;
    uint32_t cmdline_ptr;   /// pointer to the command line
    uint32_t payload_length;
    uint64_t msg_base;      /// pointer to the host message base address
    uint64_t multiboot;     /// pointer to the multiboot information
}__attribute__((packed));

#endif // XEON_PHI_XEON_PHI_H_
