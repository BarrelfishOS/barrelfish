/**
 * \file
 * \brief ARMv7-A address space sizes and offsets
 *
 * The layout of the ARM virtual address space can be summarized as
 * follows:
 *
 *
 * User-space maps user-space programs. Physical memory maps all
 * available physical memory (up to PADDR_SPACE_LIMIT). Kernel-space
 * maps only the kernel text and data.
 *
 * This partition is static and can only be changed at compile-time.
 *
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2012, 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OFFSETS_H
#define OFFSETS_H

/*
 * Some general naming conventions:
 *    *_SIZE is given in bytes.
 *    *_SPACE_* refers to address space, physical or virtual
 *    *_PHYS is a physical address.
 *    *_LIMIT is a highest-possible address, physical or virtual
 *    *_OFFSET is ....
 */

/**
 * GEN_ADDR(bits) gives the size of address space possible with <bits>
 * bits. 
 */
#define GEN_ADDR(bits)          (((genpaddr_t)1) << bits)

/**
 * Absolute size of virtual address space. This is 32-bit on ARM.
 */
#define VADDR_SPACE_SIZE        GEN_ADDR(32);

/**
 * Absolute size of physical address space.
 */
#define PADDR_SPACE_SIZE        GEN_ADDR(32)

/**
 * Maximum physical address space mappable by the kernel.  Adjust this
 * for a bigger physical address space.  
 */
#define PADDR_SPACE_LIMIT       (PADDR_SPACE_SIZE - 1)

/**
 * The size of the kernel's RAM window.
 */
#define RAM_WINDOW_SIZE         GEN_ADDR(30)

#ifndef KERNEL_LINK_BASE
#define KERNEL_LINK_BASE        0
#endif

/**
 * Kernel offset - the kernel window is mapped by TTBR1, from 2GB.
 */
#define KERNEL_OFFSET           0x80000000

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
 *
 * NB 32MB is size of the fast context switch extension
 * per-process address space.
 */
#define INIT_SPACE_LIMIT        (32 * 1024 * 1024)

/**
 * Base address of init address space in virtual memory. init should
 * start at 4 MByte. The kernel maps in important structures at 2
 * MByte. This address should be page-table size aligned (i.e. with 4
 * KByte pages, a page table maps 2 MBytes. Thus, align it to
 * multiples of 2 MBytes).
 */
#define INIT_VBASE              (2 * 1024 * 1024)

/**
 * Absolute offset of mapped physical memory within virtual address 
 * space.   Just to clarify, this means that RAM will be mapped into kernel 
 * virtual address space at this address (i.e. 2GB.).   
 */
#define MEMORY_OFFSET           GEN_ADDR(31)
// 2G (2 ** 31)

/*
 * Device offset to map devices in high memory.
 */
#define DEVICE_OFFSET			0xff000000

/**
 * The high exception vector address
 */
#define VECTORS_BASE            0xffff0000

/**
 * Kernel stack size -- 16KB
 */
#define KERNEL_STACK_SIZE       0x4000

#ifndef __ASSEMBLER__

/**
 * Absolute start of RAM in physical memory.
 */
extern lpaddr_t phys_memory_start;

static inline lvaddr_t local_phys_to_mem(lpaddr_t addr)
{
    // On the PandaBoard, this is a nop, because the physical memory is mapped
    // at the same address in virtual memory
    // i.e., MEMORY_OFFSET == phys_memory_start
    if(PADDR_SPACE_LIMIT - phys_memory_start > 0) {
        assert(addr < phys_memory_start + PADDR_SPACE_LIMIT);
    }
    return (lvaddr_t)(addr + ((lpaddr_t)MEMORY_OFFSET -
                              (lpaddr_t)phys_memory_start));
}

/**
 * Checks whether absolute local physical address `addr` is valid.
 * \param addr Absolute local physical address
 * \return True iff addr is a valid local physical address
 */
static inline bool local_phys_is_valid(lpaddr_t addr)
{
    return addr < phys_memory_start + PADDR_SPACE_LIMIT;
}

static inline lpaddr_t mem_to_local_phys(lvaddr_t addr)
{
    assert(addr >= MEMORY_OFFSET);
    return (lpaddr_t)(addr - ((lvaddr_t)MEMORY_OFFSET -
                              (lvaddr_t)phys_memory_start));
}

static inline lpaddr_t gen_phys_to_local_phys(genpaddr_t addr)
{
    //assert(addr < PADDR_SPACE_SIZE);
    return (lpaddr_t)addr;
}

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
extern uintptr_t kernel_stack[KERNEL_STACK_SIZE/sizeof(uintptr_t)];

#endif  // __ASSEMBLER__

/**
 * Kernel interrupt jump table
 */
#define INT_HANDLER_TABLE	0xFFFF0100

#endif  // OFFSETS_H
