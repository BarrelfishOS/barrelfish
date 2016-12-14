/**
 * \file
 * \brief Kernel debugging functions
 */

/*
 * Copyright (c) 2008, 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdio.h>
#include <x86.h>
#include <arch/x86/debug.h>
#include <paging_kernel_arch.h>

union lin_addr {
    uint64_t raw;
    struct {
        uint64_t  offset       :12;
        uint64_t  ptable       :9;
        uint64_t  pdir         :9;
        uint64_t  pdpt         :9;
        uint64_t  pml4         :9;
        uint64_t  sign_extend  :16;
    } d;
};

void debug_vaddr_identify(lvaddr_t debug_pml4, lvaddr_t vaddr)
{
    int i;
    printf("cr3 register      %lx\n", debug_pml4);
    printf("identifying vaddr %lx\n", vaddr);

    volatile uint64_t *temp = (uint64_t*)vaddr;

    for(i = 0; i < 512; i++) {
        printf("at addr %lx content is %lx\n", (uint64_t)(temp + i), *(temp + i));
    }
    printf("\n");

    union lin_addr lin_addr;
    lin_addr.raw = (uint64_t)vaddr;

    printf("vaddr broken down\n");
    printf("sign_extend = %x\n", lin_addr.d.sign_extend);
    printf("pml4        = %x\n", lin_addr.d.pml4);
    printf("pdpt        = %x\n", lin_addr.d.pdpt);
    printf("pdir        = %x\n", lin_addr.d.pdir);
    printf("ptable      = %x\n", lin_addr.d.ptable);
    printf("offset      = %x\n", lin_addr.d.offset);

    uint64_t *pml4et;
    pml4et = (uint64_t*)(debug_pml4 +
                         (lin_addr.d.pml4 * sizeof(union x86_64_pdir_entry)));
    printf("PML4e addr = %016lx ", (uint64_t)pml4et);
    printf("content = %016lx\n", *pml4et);

    lvaddr_t pdpt_addr;
    pdpt_addr = local_phys_to_mem(((union x86_64_pdir_entry*)pml4et)->d.base_addr << 12);
    uint64_t *pdptet;
    pdptet = (uint64_t*)(pdpt_addr +
                         (lin_addr.d.pdpt * sizeof(union x86_64_pdir_entry)));
    printf("PDPe  addr = %016lx ", (uint64_t)pdptet);
    printf("content = %016lx\n", *pdptet);

    if (x86_64_pdir_entry_leafp ((union x86_64_pdir_entry*) pdptet)) {
	lpaddr_t addr = ((union x86_64_ptable_entry*)pdptet)->huge.base_addr << 30;
	printf("addr = %016lx, 1G page\n", addr);
	return;
    }

    lvaddr_t pdir_addr;
    pdir_addr = local_phys_to_mem(((union x86_64_pdir_entry*)pdptet)->d.base_addr << 12);
    uint64_t *pdiret;
    pdiret = (uint64_t*)(pdir_addr +
                         (lin_addr.d.pdir * sizeof(union x86_64_pdir_entry)));
    printf("PGDe  addr = %016lx ", (uint64_t)pdiret);
    printf("content = %016lx\n", *pdiret);

    if (x86_64_pdir_entry_leafp ((union x86_64_pdir_entry*) pdiret)) {
	lpaddr_t addr = ((union x86_64_ptable_entry*)pdiret)->large.base_addr << 21;
	printf("addr = %016lx, 2M page\n", addr);
	return;
    }

    lvaddr_t ptable_addr;
    ptable_addr = local_phys_to_mem(((union x86_64_pdir_entry*)pdiret)->d.base_addr << 12);
    uint64_t *ptableet;
    ptableet = (uint64_t*)(ptable_addr +
                         (lin_addr.d.ptable * sizeof(union x86_64_pdir_entry)));
    printf("PTe   addr = %016lx ", (uint64_t)ptableet);
    printf("content = %016lx\n", *ptableet);

    lpaddr_t addr = ((union x86_64_ptable_entry*)ptableet)->base.base_addr << 12;
    printf("addr = %016lx, 4K page\n", addr);
}

uintptr_t kernel_virt_to_elf_addr(void *addr)
{
    return (uintptr_t)addr - (uintptr_t)&_start_kernel + START_KERNEL_PHYS;
}
