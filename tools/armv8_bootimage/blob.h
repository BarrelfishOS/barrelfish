/*
 * EFI Blob structure for the ARMv8 platforms
 *
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BLOB_H
#define BLOB_H

#define BASE_PAGE_SIZE (1<<12)

// Blob header
struct Blob {                   // offsets
    union {
        struct {
            uint64_t magic;
            uint64_t multiboot; // offset of the Multiboot2 boot info
            uint64_t multiboot_size;
            uint64_t modules;
            uint64_t modules_size;
            uint64_t boot_driver_segment;       // offset of the boot driver image
            uint64_t boot_driver_segment_size;
            uint64_t boot_driver_relocations;
            uint64_t boot_driver_relocations_count;
            uint64_t boot_driver_entry;
            uint64_t cpu_driver_segment;    // offset of the cpu kernel image
            uint64_t cpu_driver_segment_size;
            uint64_t cpu_driver_relocations;
            uint64_t cpu_driver_relocations_count;
            uint64_t cpu_driver_entry;
        };
        unsigned char data[BASE_PAGE_SIZE];
    };
};

struct Blob_relocation {
    uint64_t offset, addend;
};

#endif
