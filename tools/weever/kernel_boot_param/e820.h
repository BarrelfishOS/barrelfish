/**
 * \file
 * \brief Struct definition for the boot param struct supplied by the
 *        K1OM boot loader
 */

/*
 * Copyright (c) 2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 *
 * This is adapted from the Linux kernel (kernel.org)
 *
 */

#ifndef KERNEL_E820_H
#define KERNEL_E820_H


#define E820MAP 0x2d0       /* our map */
#define E820MAX 128     /* number of entries in E820MAP */

/*
 * Legacy E820 BIOS limits us to 128 (E820MAX) nodes due to the
 * constrained space in the zeropage.  If we have more nodes than
 * that, and if we've booted off EFI firmware, then the EFI tables
 * passed us from the EFI firmware can list more nodes.  Size our
 * internal memory map tables to have room for these additional
 * nodes, based on up to three entries per node for which the
 * kernel was built: MAX_NUMNODES == (1 << CONFIG_NODES_SHIFT),
 * plus E820MAX, allowing space for the possible duplicate E820
 * entries that might need room in the same arrays, prior to the
 * call to sanitize_e820_map() to remove duplicates.  The allowance
 * of three memory map entries per node is "enough" entries for
 * the initial hardware platform motivating this mechanism to make
 * use of additional EFI map entries.  Future platforms may want
 * to allow more than three entries per node or otherwise refine
 * this size.
 */

/*
 * Odd: 'make headers_check' complains about numa.h if I try
 * to collapse the next two #ifdef lines to a single line:
 *  #if defined(__KERNEL__) && defined(CONFIG_EFI)
 */
#define E820_X_MAX E820MAX

#define E820NR  0x1e8       /* # entries in E820MAP */

#define E820_RAM    1
#define E820_RESERVED   2
#define E820_ACPI   3
#define E820_NVS    4
#define E820_UNUSABLE   5


/*
 * reserved RAM used by kernel itself
 * if CONFIG_INTEL_TXT is enabled, memory of this type will be
 * included in the S3 integrity calculation and so should not include
 * any memory that BIOS might alter over the S3 transition
 */
#define E820_RESERVED_KERN        128

#ifndef __ASSEMBLY__
struct e820entry {
    uint64_t addr; /* start of memory segment */
    uint64_t size; /* size of memory segment */
    uint32_t type; /* type of memory segment */
} __attribute__((packed));

struct e820map {
    uint32_t nr_map;
    struct e820entry map[E820_X_MAX];
};

#define ISA_START_ADDRESS   0xa0000
#define ISA_END_ADDRESS     0x100000

#define BIOS_BEGIN      0x000a0000
#define BIOS_END        0x00100000

#define BIOS_ROM_BASE       0xffe00000
#define BIOS_ROM_END        0xffffffff

#endif /* __ASSEMBLY__ */


#endif /* KERNEL_E820_H */

