/**
 * \file
 * \brief x86-64 kernel page-table setup
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <paging_kernel_arch.h>

#ifdef __k1om__
#include <xeon_phi.h>
#define PADDR_SPACE_LIMIT K1OM_PADDR_SPACE_LIMIT
#define PTABLE_GLOBAL_PAGE_BIT 0
#define MEMORY_OFFSET K1OM_MEMORY_OFFSET
#define KERNEL_INIT_MEMORY K1OM_KERNEL_INIT_MEMORY
#else
#define PADDR_SPACE_LIMIT X86_64_PADDR_SPACE_LIMIT
#define PTABLE_GLOBAL_PAGE_BIT X86_64_PTABLE_GLOBAL_PAGE
#define MEMORY_OFFSET X86_64_MEMORY_OFFSET
#define KERNEL_INIT_MEMORY X86_64_KERNEL_INIT_MEMORY
#endif

/*
 * Table requirements for various address spaces.
 */
#define MEM_PDPT_SIZE           X86_64_PDPT_ENTRIES(PADDR_SPACE_LIMIT)
#define MEM_PDIR_SIZE           X86_64_PDIR_ENTRIES(PADDR_SPACE_LIMIT)

/*
 * Page attribute bitmaps for various address spaces.
 */
#define MEM_PAGE_BITMAP                                 \
    (X86_64_PTABLE_PRESENT | X86_64_PTABLE_READ_WRITE | \
     PTABLE_GLOBAL_PAGE_BIT)
#define DEVICE_PAGE_BITMAP                                      \
    (X86_64_PTABLE_PRESENT | X86_64_PTABLE_READ_WRITE |         \
     X86_64_PTABLE_CACHE_DISABLED | PTABLE_GLOBAL_PAGE_BIT)

/**
 * Kernel page map level 4 table.
 */
static union x86_64_pdir_entry pml4[X86_64_PTABLE_SIZE]
__attribute__((aligned(X86_64_BASE_PAGE_SIZE)));

/**
 * Page directory pointer table for physical memory address space.
 */
static union x86_64_pdir_entry mem_pdpt[MEM_PDPT_SIZE][X86_64_PTABLE_SIZE]
__attribute__((aligned(X86_64_BASE_PAGE_SIZE)));

/**
 * Page directory for physical memory address space.
 */
static union x86_64_ptable_entry mem_pdir[MEM_PDPT_SIZE][MEM_PDIR_SIZE][X86_64_PTABLE_SIZE]
__attribute__((aligned(X86_64_BASE_PAGE_SIZE)));

static inline void mapit(union x86_64_pdir_entry *pml4_base,
                         union x86_64_pdir_entry *pdpt_base,
                         union x86_64_ptable_entry *pdir_base, lpaddr_t addr,
                         uint64_t bitmap)
{
    if(!X86_64_IS_PRESENT(pml4_base)) {
        paging_x86_64_map_table(pml4_base,
                                mem_to_local_phys((lvaddr_t)pdpt_base));
    }

    if(!X86_64_IS_PRESENT(pdpt_base)) {
        paging_x86_64_map_table(pdpt_base,
                                mem_to_local_phys((lvaddr_t)pdir_base));
    }

    if(!X86_64_IS_PRESENT(pdir_base)) {
        debug(SUBSYS_PAGING, "mapped!\n");
        paging_x86_64_map_large(pdir_base, addr, bitmap);
    } else {
//remap the page anyway, this is important for the memory latency benchmark
        debug(SUBSYS_PAGING, "already existing! remapping it\n");
        paging_x86_64_map_large(pdir_base, addr, bitmap);
    }
}

/**
 * \brief Map a region of physical memory into physical memory address space.
 *
 * Maps the region of physical memory, based at base and sized size bytes
 * to the same-sized virtual memory region. All pages are flagged according to
 * bitmap. This function automatically fills the needed page directory entries
 * in the page hierarchy rooted at pml4. base and size will be made
 * page-aligned by this function.
 *
 * \param base          Physical base address of memory region
 * \param size          Size in bytes of memory region
 * \param bitmap        Bitmap of flags for page tables/directories
 *
 * \return 0 on success, -1 on error (out of range)
 */
static int paging_map_mem(lpaddr_t base, size_t size, uint64_t bitmap)
{
    lvaddr_t vaddr, vbase = local_phys_to_mem(base);
    lpaddr_t addr;

    // Align given physical base address
    if(base & X86_64_MEM_PAGE_MASK) {
        base -= base & X86_64_MEM_PAGE_MASK;
    }

    paging_align(&vbase, &base, &size, X86_64_MEM_PAGE_SIZE);

    // Is mapped region out of range?
    assert(base + size <= (lpaddr_t)PADDR_SPACE_LIMIT);
    if(base + size > (lpaddr_t)PADDR_SPACE_LIMIT) {
        return -1;
    }

    // Map pages, tables and directories
    for(vaddr = vbase, addr = base; vaddr < vbase + size;
        vaddr += X86_64_MEM_PAGE_SIZE, addr += X86_64_MEM_PAGE_SIZE) {
        union x86_64_pdir_entry *pml4_base =
            &pml4[X86_64_PML4_BASE(vaddr)],
            *pdpt_base = &mem_pdpt[X86_64_PML4_BASE(addr)][X86_64_PDPT_BASE(vaddr)];
        union x86_64_ptable_entry *pdir_base =
            &mem_pdir[X86_64_PML4_BASE(addr)][X86_64_PDPT_BASE(addr)][X86_64_PDIR_BASE(vaddr)];

        debug(SUBSYS_PAGING, "Mapping 2M page: vaddr = 0x%"PRIxLVADDR"x, addr = 0x%lx, "
              "PML4_BASE = %lu, PDPT_BASE = %lu, PDIR_BASE = %lu -- ", vaddr,
              addr, X86_64_PML4_BASE(vaddr), X86_64_PDPT_BASE(vaddr),
              X86_64_PDIR_BASE(vaddr));

        mapit(pml4_base, pdpt_base, pdir_base, addr, bitmap);
    }
    // XXX FIXME: get rid of this TBL flush code, or move it elsewhere
    // uint64_t cr3;
    // __asm__ __volatile__("mov %%cr3,%0" : "=a" (cr3) : );
    // __asm__ __volatile__("mov %0,%%cr3" :  : "a" (cr3));

    return 0;
}

lvaddr_t paging_x86_64_map_device(lpaddr_t base, size_t size)
{
    if(paging_map_mem(base, size, DEVICE_PAGE_BITMAP) == 0) {
        return local_phys_to_mem(base);
    } else {
        return 0;
    }
}

int paging_x86_64_map_memory(lpaddr_t base, size_t size)
{
    return paging_map_mem(base, size, MEM_PAGE_BITMAP);
}

/**
 * \brief Reset kernel paging.
 *
 * This function resets the page maps for kernel and memory-space. It clears out
 * all other mappings. Use this only at system bootup!
 */
void paging_x86_64_reset(void)
{
    // Map kernel image so we don't lose ground
    if(paging_x86_64_map_memory(mem_to_local_phys((lvaddr_t)&_start_kernel),
                                SIZE_KERNEL_IMAGE) != 0) {
        panic("error while mapping physical memory!");
    }

    // Map an initial amount of memory
    if(paging_x86_64_map_memory(0, KERNEL_INIT_MEMORY) != 0) {
        panic("error while mapping physical memory!");
    }
    
#ifdef __k1om__
    /* mapping the Xeon Phi SBOX registers to provide serial input */
    if (paging_x86_64_map_memory(XEON_PHI_SBOX_BASE, XEON_PHI_SBOX_SIZE) != 0) {
        panic("error while mapping physical memory!");
    }
#endif

    // Switch to new page layout
    paging_x86_64_context_switch(mem_to_local_phys((lvaddr_t)pml4));
}

/**
 * \brief Make a "good" PML4 table out of a page table.
 *
 * A "good" PML4 table is one that has all physical address space and
 * the kernel mapped in. This function modifies the passed PML4, based
 * at physical address 'base' accordingly. It does this by taking out
 * the corresponding entries of the kernel's pristine PML4 table.
 *
 * \param base  Physical base address of PML4 table to make "good".
 */
void paging_x86_64_make_good_pml4(lpaddr_t base)
{
    union x86_64_pdir_entry *newpml4 =
        (union x86_64_pdir_entry *)local_phys_to_mem(base);
    int                 i;

        // XXX: Disabled till vaddr_t is figured out
    debug(SUBSYS_PAGING, "Is now a PML4: table = 0x%"PRIxLPADDR"\n", base);

    // Map memory
    for(i = X86_64_PML4_BASE(MEMORY_OFFSET); i < X86_64_PTABLE_SIZE; i++) {
        newpml4[i] = pml4[i];
    }
}

// setup 1:1 mappings for EPT pml4
void paging_x86_64_make_good_ept_pml4(lpaddr_t base)
{
    union x86_64_pdir_entry *newpml4 =
        (union x86_64_pdir_entry *)local_phys_to_mem(base);
    uint64_t i;

        // XXX: Disabled till vaddr_t is figured out
    debug(SUBSYS_PAGING, "Is now an EPT PML4: table = 0x%"PRIxLPADDR"\n", base);

    // Map memory 1:1 for EPT
    for(i = X86_64_PML4_BASE(MEMORY_OFFSET); i < X86_64_PTABLE_SIZE; i++) {
        paging_x86_64_map_table(&newpml4[i], i << 39);
    }
}
