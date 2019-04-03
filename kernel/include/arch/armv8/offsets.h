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

#ifndef OFFSETS_H
#define OFFSETS_H

#include <target/armv8/offsets_target.h>

/**
 * Absolute size of virtual address space. This is 48-bit on AArch64.
 * TODO: might be implementation-specific
 */
#define VADDR_SPACE_SIZE_BITS   ARMv8_VADDR_SPACE_SIZE_BITSA
#define VADDR_SPACE_SIZE        GEN_ADDR(VADDR_SPACE_SIZE_BITS);

/**
 * Absolute size of physical address space.
 * Depends on value in ID_AA64MMFR0_EL1 (ARMv8-A TRM, D4-1733)
 * current options are 4G, 64G, 1T, 4T, 16T, 256T
 * set to 256T for now
 */
#define PADDR_SPACE_SIZE_BITS   ARMv8_PADDR_SPACE_SIZE_BITS
#define PADDR_SPACE_SIZE        GEN_ADDR(PADDR_SPACE_SIZE_BITS)

/**
 * Start address of kernel image in physical memory.  Most ARM platforms have
 * the first physical window starting at 2GB.
 */
#define START_KERNEL_PHYS       ARMv8_START_KERNEL_PHYS

/**
 * Kernel offset - virtual base of the kernel's address space: the region
 * mapped by TTBR1.
 */
#define KERNEL_OFFSET           ARMv8_KERNEL_OFFSET

/**
 * Maximum physical address space mappable by the kernel.  Adjust this
 * for a bigger physical address space.  
 */
#define PADDR_SPACE_LIMIT       ARMv8_PADDR_SPACE_LIMIT // 2GB

/**
 * The absolute base address of mapped physical memory, within the kernel's
 * virtual address space.  
 *
 */
#define MEMORY_OFFSET           (KERNEL_OFFSET)

/**
 * Absolute start of RAM in physical memory.  XXX - this isn't statically
 * known.
 */
#define PHYS_MEMORY_START       ARMv8_PHYS_MEMORY_START

/*
 * The top of the region, within kernel memory, in which devices are mapped.
 */
#define DEVICE_OFFSET           (KERNEL_OFFSET + GEN_ADDR(30))

/**
 * Kernel stack size -- 16KB
 */
#define KERNEL_STACK_SIZE       ARMv8_KERNEL_STACK_SIZE

/**
 * The size of the whole kernel image.
 */
#define KERNEL_IMAGE_SIZE       (size_t)(&kernel_final_byte - \
                                         &kernel_first_byte)

/*
 * Bytes per kernel copy for each core (1 Section)
 */
#define KERNEL_SECTION_SIZE     0x100000

#define KERNEL_STACK_ADDR       (lpaddr_t)kernel_stack

#ifndef __ASSEMBLER__

#include <assert.h>
#include <stdbool.h>

static inline lvaddr_t local_phys_to_mem(lpaddr_t addr)
{
    assert(addr < PHYS_MEMORY_START + PADDR_SPACE_LIMIT);
    return (lvaddr_t)(addr + ((lpaddr_t)MEMORY_OFFSET -
                              (lpaddr_t)PHYS_MEMORY_START));
}

/**
 * Checks whether absolute local physical address `addr` is valid.
 * \param addr Absolute local physical address
 * \return True iff addr is a valid local physical address
 */
static inline bool local_phys_is_valid(lpaddr_t addr)
{
    return addr < PHYS_MEMORY_START + PADDR_SPACE_LIMIT;
}

static inline lpaddr_t mem_to_local_phys(lvaddr_t addr)
{
    assert(addr >= MEMORY_OFFSET);
    return (lpaddr_t)(addr - ((lvaddr_t)MEMORY_OFFSET -
                              (lvaddr_t)PHYS_MEMORY_START));
}

static inline lpaddr_t gen_phys_to_local_phys(genpaddr_t addr)
{
    //assert(addr < PADDR_SPACE_SIZE);
    return (lpaddr_t)addr;
}

/* XXX - what is this? */
static inline genpaddr_t local_phys_to_gen_phys(lpaddr_t addr)
{
    return (genpaddr_t)addr;
}

/**
 * Symbol: Start of kernel image. This symbol points to the start
 * address of the kernel image.
 */
extern uint8_t kernel_first_byte;

/**
 * Symbol: End of kernel image. This symbol points to the end address
 * of the kernel image.
 */
extern uint8_t kernel_text_final_byte;

/**
 * Symbol: End of kernel image. This symbol points to the end address
 * of the kernel image.
 */
extern uint8_t kernel_final_byte;

extern uint8_t kernel_elf_header;

/**
 * \brief The kernel stack.
 *
 * Declared in boot.S.
 */
extern uintptr_t kernel_stack, kernel_stack_top;

#endif  // __ASSEMBLER__

#endif  // OFFSETS_H
