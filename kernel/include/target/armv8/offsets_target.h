/**
 * \file
 * \brief ARMv8 address-space sizes and offsets
 *
 */

/*
 * Copyright (c) 2007,2008,2009,2012,2015,2016 ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OFFSETS_TARGET_H
#define OFFSETS_TARGET_H

#define GEN_ADDR(bits)          (((genpaddr_t)1) << bits)

/**
 * Absolute size of virtual address space. This is 48-bit on AArch64.
 * TODO: might be implementation-specific
 */
#define ARMv8_VADDR_SPACE_SIZE_BITS   48
#define ARMv8_VADDR_SPACE_SIZE        GEN_ADDR(VADDR_SPACE_SIZE_BITS);

/**
 * Absolute size of physical address space.
 * Depends on value in ID_AA64MMFR0_EL1 (ARMv8-A TRM, D4-1733)
 * current options are 4G, 64G, 1T, 4T, 16T, 256T
 * set to 256T for now
 */
#define ARMv8_PADDR_SPACE_SIZE_BITS   48
#define ARMv8_PADDR_SPACE_SIZE        GEN_ADDR(PADDR_SPACE_SIZE_BITS)

/**
 * Start address of kernel image in physical memory.  Most ARM platforms have
 * the first physical window starting at 2GB.
 */
#define ARMv8_START_KERNEL_PHYS       0x80000000

/**
 * Kernel offset - virtual base of the kernel's address space: the region
 * mapped by TTBR1.
 */
#define ARMv8_KERNEL_OFFSET           0xffff000000000000ULL

/**
 * Maximum physical address space mappable by the kernel.  Adjust this
 * for a bigger physical address space.  
 */
#define ARMv8_PADDR_SPACE_LIMIT       (GEN_ADDR(49) - 1) // 2GB

/**
 * Static address space limit for the init user-space domain. The
 * static space is used to map in code and static data of the init
 * module, as well as all loaded multiboot modules. init can freely
 * allocate dynamic memory as soon as it is running. This is 32 MBytes
 * right now.
 *
 * You should make this constant a multiple of #BASE_PAGE_SIZE *
 * #PTABLE_SIZE or you'll restrict init's static address space
 * unneccessarily. init's lowest segment should also be based at these
 * multiples or it restricts itself.
 *
 */
#define ARMV8_INIT_SPACE_LIMIT        (32 * 1024 * 1024)

/**
 * Base address of init address space in virtual memory. init should
 * start at 4 MByte. The kernel maps in important structures at 2
 * MByte. This address should be page-table size aligned (i.e. with 4
 * KByte pages, a page table maps 2 MBytes. Thus, align it to
 * multiples of 2 MBytes).
 */
#define ARMV8_INIT_VBASE              (2 * 1024 * 1024)

/**
 * The absolute base address of mapped physical memory, within the kernel's
 * virtual address space.  
 *
 */
#define ARMv8_MEMORY_OFFSET           (KERNEL_OFFSET)

/**
 * Absolute start of RAM in physical memory.  XXX - this isn't statically
 * known.
 */
#define ARMv8_PHYS_MEMORY_START       0x0

/*
 * The top of the region, within kernel memory, in which devices are mapped.
 */
#define ARMv8_DEVICE_OFFSET           (KERNEL_OFFSET + GEN_ADDR(30))

/**
 * Kernel stack size -- 16KB
 */
#define ARMv8_KERNEL_STACK_SIZE       0x4000

/*
 * Bytes per kernel copy for each core (1 Section)
 */
#define ARMv8_KERNEL_SECTION_SIZE     0x100000

#define ARMv8_KERNEL_STACK_ADDR       (lpaddr_t)kernel_stack

#endif  // OFFSETS_TARGET_H
