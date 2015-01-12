/**
 * \file
 * \brief K1OM address space sizes and offsets
 *
 * The layout of the K1OM virtual address space can be summarized as
 * follows:
 *
 *<pre>
 * +----------------------------------------------------+-----------------+
 * | User-space                                         | Physical memory |
 * | PML4 entries: 0 1 2 3 4 ... 510                    | 511             |
 * +----------------------------------------------------+-----------------+</pre>
 *
 * User-space maps user-space programs. Physical memory maps all
 * available physical memory (up to PADDR_SPACE_LIMIT).
 *
 * This partition is static and can only be changed at compile-time.
 *
 * Physical memory can grow downwards, towards user-space, although it
 * is expected to stay within PML4 entry 511 for quite some time (one
 * PML4 entry can map 512 GBytes). The rest of the address space can
 * be freely mapped by (possibly multiple) user-space programs.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_TARGET_K1OM_OFFSETS_H
#define KERNEL_TARGET_K1OM_OFFSETS_H



/**
 * Absolute size of virtual address space. This is 48-bit on x86-64
 * currently, which equals 256 TBytes and allows for 512 PML4 slots,
 * each of which can map 512 GBytes.
 */
#define K1OM_VADDR_SPACE_BITS 48
#define K1OM_VADDR_SPACE_SIZE        ((genpaddr_t)1 << K1OM_VADDR_SPACE_BITS)

/**
 * Absolute size of physical address space. This is also 40-bit.
 *
 * Intel Xeon Phi Systems Software Developers Guide,  2.1.4:
 *
 * The Intel® Xeon Phi™ coprocessor supports 40-bit physical address in 64-bit.
 */
#define X1OM_PADDR_SPACE_BITS 40
#define K1OM_PADDR_SPACE_SIZE        ((genpaddr_t)1 << X1OM_PADDR_SPACE_BITS)

/**
 * Start address of kernel image in physical memory. This is passed to
 * the linker also. The bootloader will load us there.
 */
#define K1OM_START_KERNEL_PHYS          0x100000

/**
 * Kernel stack size -- 16KB
 */
#define K1OM_KERNEL_STACK_SIZE       0x4000



/**
 * Maximum physical address space mappable by the kernel.  Adjust this
 * for a bigger physical address space.  We set this to 40-bit,
 * i.e. 1024 GBytes.
 *
 * Xeon Phi Systems Software Developers Guide,  2.1.4:
 *
 * The Intel® Xeon Phi™ coprocessor supports 40-bit physical address in 64-bit.
 *
 */
#define K1OM_PADDR_SPACE_LIMIT       ((genpaddr_t)1 << 40)

/**
 * Maximum physical address space mappable by the kernel.  Adjust this
 * for a bigger physical address space.  We set this to 40-bit,
 * i.e. 1024 GBytes.
 *
 * Xeon Phi Systems Software Developers Guide,  2.1.4:
 *
 * The Intel® Xeon Phi™ coprocessor supports 40-bit physical address in 64-bit.
 *
 */
#ifdef __XEON_PHI_7120__
#define K1OM_PHYSICAL_MEMORY_SIZE       ((genpaddr_t)(16ULL*1024*1024*1024))
#else
#ifdef __XEON_PHI_5120__
#define K1OM_PHYSICAL_MEMORY_SIZE       ((genpaddr_t)(8ULL*1024*1024*1024))
#else
/* __XEON_PHI_3120__  */
#define K1OM_PHYSICAL_MEMORY_SIZE       ((genpaddr_t)(6ULL*1024*1024*1024))
#endif
#endif



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
 */
#define K1OM_INIT_SPACE_LIMIT        (32 * 1024 * 1024)

/**
 * Base address of init address space in virtual memory. init should
 * start at 4 MByte. The kernel maps in important structures at 2
 * MByte. This address should be page-table size aligned (i.e. with 4
 * KByte pages, a page table maps 2 MBytes. Thus, align it to
 * multiples of 2 MBytes).
 */
#define K1OM_INIT_VBASE              0x200000

/**
 * Initial amount of physical memory to map during bootup. The low
 * 1MByte of memory is always expected to be there and has to be
 * specified here at minimum. If you need more during bootup, increase
 * this value. This value is also the amount of memory you _expect_ to
 * be in the system during bootup, or the kernel will crash!
 */
#define K1OM_KERNEL_INIT_MEMORY      (1 * 1024 * 1024)

/**
 * Aligns an address to the nearest PML4 entry by masking out lower 39
 * bits.
 */
#define K1OM_PML4_ALIGN(addr)        ((addr) & ((genpaddr_t)0x1ffffffUL << 39))

/**
 * Absolute offset of mapped physical memory within virtual address
 * space.  This occupies one or more (usually one) PML4 slots directly
 * before the kernel. This needs to be aligned to PADDR_SPACE_LIMIT.
 *
 * Change VSPACE_END in lib/barrelfish if you change this.
 */
#define K1OM_MEMORY_OFFSET        K1OM_PML4_ALIGN(-K1OM_PADDR_SPACE_LIMIT)

/**
 * The real-mode addresses
 */

#define K1OM_REAL_MODE_SEGMENT 0x0600 /**< The real-mode segment */
#define K1OM_REAL_MODE_OFFSET  0x0000 /**< The real-mode offset _has to be_ 0000!! */

#define K1OM_REAL_MODE_LINEAR_OFFSET \
    (K1OM_REAL_MODE_SEGMENT << 4) /**< The linear offset
                                       of the real-mode
                                       segment */

#define K1OM_REAL_MODE_SEGMENT_TO_REAL_MODE_PAGE(seg) ((uint8_t)(seg >> 8))
#define K1OM_REAL_MODE_ADDR_TO_REAL_MODE_VECTOR(seg,off) ((uint32_t)(seg << 16) | off)

#define X86_64_REAL_MODE_SEGMENT 0x0600 /**< The real-mode segment */
#define X86_64_REAL_MODE_OFFSET  0x0000 /**< The real-mode offset _has to be_ 0000!! */

#define X86_64_REAL_MODE_LINEAR_OFFSET \
    (X86_64_REAL_MODE_SEGMENT << 4) /**< The linear offset
                                       of the real-mode
                                       segment */

#define X86_64_REAL_MODE_SEGMENT_TO_REAL_MODE_PAGE(seg) ((uint8_t)(seg >> 8))
#define X86_64_REAL_MODE_ADDR_TO_REAL_MODE_VECTOR(seg,off) ((uint32_t)(seg << 16) | off)

/*
 * TODO: Add K1OM offsets
 */

#ifndef __ASSEMBLER__

/**
 * \brief The kernel stack.
 */
extern uintptr_t k1om_kernel_stack[K1OM_KERNEL_STACK_SIZE/sizeof(uintptr_t)];

#endif

#endif // KERNEL_TARGET_K1OM_OFFSETS_H
