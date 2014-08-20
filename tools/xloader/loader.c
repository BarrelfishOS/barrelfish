/**
 * \file
 * \brief Elver - Intermediary stage bootloader
 *
 * Elver is used to switch the system into 64-bit long-mode and load
 * the kernel, which is a relocatable ELF64 image. Unfortunately, GRUB
 * is not able to this without a patch. This is purely for
 * backwards-compatibility. As soon as bootloaders support loading
 * relocatable ELF64 images into 64-bit mode, this can be dropped.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <barrelfish_kpi/types.h>
#include <errors/errno.h>
#include <elf/elf.h>

#include "kernel_boot_param.h"

extern struct multiboot_info xeon_phi_mbi;

#include "../../kernel/include/multiboot.h"

#define K1OM_BOOT_MAGIC         0xB001B001

#define SBOX_BASE           0x08007D0000ULL

extern char _end_bootloader;

/// Round up n to the next multiple of size
#define ROUND_UP(n, size)           ((((n) + (size) - 1)) & (~((size) - 1)))
#define BASE_PAGE_SIZE 0x1000

struct multiboot_info multiboot_info = { .flags =
        (MULTIBOOT_HEADER_FLAG_MODS_PGALIGNED | MULTIBOOT_HEADER_FLAG_NEED_MEMINFO) };
static lpaddr_t phys_alloc_start;

int
loader(uint64_t magic,
       struct boot_params *mb);

genvaddr_t kernel_entry;

/*
 * ----------------------------------------------------------------------------
 *  Basic Error Reporting Mechanism
 * ----------------------------------------------------------------------------
 */
union status
{
    uint32_t raw;
    char vals[4];
};

static void
print_status(char a,
             char b)
{

    volatile uint32_t *p = (volatile uint32_t *) ((SBOX_BASE) + 0x0000AB40);
    volatile uint32_t *p2 = (volatile uint32_t *) ((SBOX_BASE) + 0x0000AB5C);

    union status s;

    s.vals[3] = 0x0a;
    s.vals[2] = b;
    s.vals[1] = a;
    s.vals[0] = '>';

    while ((*p))
        ;

    *p2 = s.raw;
    *p = 0x7A7A7A7A;
}

static inline void
eabort(char a,
       char b)
{
    print_status(a, b);
    while (1)
        ;
}

/*
 * ----------------------------------------------------------------------------
 *  ELF Utility Functions
 * ----------------------------------------------------------------------------
 */

static errval_t
linear_alloc(void *s,
             genvaddr_t base,
             size_t size,
             uint32_t flags,
             void **ret)
{
    // round to base page size
    uint32_t npages = (size + BASE_PAGE_SIZE - 1) / BASE_PAGE_SIZE;

    /* *ret = (void *)(uintptr_t)base; */
    *ret = (void *) phys_alloc_start;

    phys_alloc_start += npages * BASE_PAGE_SIZE;
    return SYS_ERR_OK;
}

#if 0
static struct multiboot_modinfo *multiboot_find_module(const char *basename)
{
    struct multiboot_modinfo *mod = (struct multiboot_modinfo *)
    multiboot_info->mods_addr;

    for(size_t i = 0; i < multiboot_info->mods_count; i++) {
        const char *modname = strrchr((char *)mod[i].string, '/');

        if(modname == NULL) {
            modname = (char *)mod[i].string;
        } else {
            modname++;
        }

        if(!strncmp(modname, basename, strlen(basename))) {
            return &mod[i];
        }
    }

    return NULL;
}

static uintptr_t multiboot_end_addr(void)
{
    lpaddr_t end = ((lpaddr_t)multiboot_info) + sizeof(struct multiboot_info);
    struct multiboot_info *mi = multiboot_info;

#define CHECK(pa)           { lpaddr_t tmp = pa; if (tmp > end) { end = tmp; } }
#define CHECK_STR(pstr)     CHECK(pstr + strlen((char *)pstr) + 1)

    if (mi->flags & MULTIBOOT_INFO_FLAG_HAS_CMDLINE) {
        CHECK_STR(mi->cmdline)
    }

    if (mi->flags & MULTIBOOT_INFO_FLAG_HAS_MODS) {
        struct multiboot_modinfo *mod = (void *)mi->mods_addr;

        for(int i = 0; i < mi->mods_count; i++) {
            CHECK(mod[i].mod_end)
            CHECK_STR(mod[i].string)
        }
    }

    if (mi->flags & MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS) {
        CHECK(mi->syms.elf.addr + mi->syms.elf.num * mi->syms.elf.size)
        /* FIXME: does this include mi_elfshdr_shndx?? */
    }

    if (mi->flags & MULTIBOOT_INFO_FLAG_HAS_MMAP) {
        CHECK(mi->mmap_addr + mi->mmap_length)
    }

    if (mi->flags & MULTIBOOT_INFO_FLAG_HAS_DRIVES) {
        CHECK(mi->drives_addr + mi->drives_length)
    }

    if (mi->flags & MULTIBOOT_INFO_FLAG_HAS_LOADERNAME) {
        CHECK_STR(mi->boot_loader_name)
    }

    /* TODO: config table, APM table, VBE */

#undef CHECK
#undef CHECK_STR

    return end;
}


static void
set_elf_headers(uintptr_t base)
{
    struct Elf64_Ehdr *head = (struct Elf64_Ehdr *) (base);

    multiboot_info->syms.elf.num = head->e_shnum;
    multiboot_info->syms.elf.size = head->e_shentsize;
    multiboot_info->syms.elf.addr = base + head->e_shoff;
    multiboot_info->syms.elf.shndx = head->e_shstrndx;
}
#endif


/**
 * Entry point from boot.S
 *
 * + long mode enabled
 * + procetion mode enabled
 * + paging enabled
 *
 * Memory Layout
 *
 * 0x0000000 boot.S
 *
 * 0x0000000
 *
 * 0x0000000    _end_bootloader
 *
 * start of CPU.elf
 *
 *
 * \param magic         magic value
 * \param bootparam     pointer to struct boot param
 *
 */
int
loader(uint64_t magic,
       struct boot_params *bootparam)
{
    errval_t err;

    print_status('S', '0');

    if (magic != K1OM_BOOT_MAGIC) {
        /* wrong value */
        eabort('E', '0');
    }

    struct boot_params *bp = (struct boot_params *) bootparam;
    struct setup_header *boot_hdr = (struct setup_header *) &bp->hdr;



    /*
     * Copy the multi boot image closer to the kernel
     */
    lpaddr_t mb_img_start = ROUND_UP((lpaddr_t)&_end_bootloader,
                                  1<<21 );
    lpaddr_t mb_img_orig = boot_hdr->ramdisk_image;

    if (mb_img_start > mb_img_orig) {
        eabort('E', 'C');
    }
    memcpy((void *)mb_img_start, (void *)mb_img_orig, boot_hdr->ramdisk_size);



    /* set the start address where we can allocate ram */
    phys_alloc_start = ROUND_UP(mb_img_start+boot_hdr->ramdisk_size, BASE_PAGE_SIZE);

    boot_hdr->ramdisk_image = (uint32_t)mb_img_start;

    lpaddr_t kernel_start = phys_alloc_start;

    /* fillin multiboot structure */
    multiboot_info.cmdline = boot_hdr->cmd_line_ptr;
    multiboot_info.mods_addr = boot_hdr->ramdisk_image;


    err = elf64_load(EM_K1OM, linear_alloc, NULL, boot_hdr->ramdisk_image,
                     boot_hdr->ramdisk_size, &kernel_entry, NULL, NULL,
                     NULL);

    if (err_is_fail(err)) {
        eabort('E', '1');
    }

    struct Elf64_Ehdr *cpu_head = (struct Elf64_Ehdr *) (uint64_t) boot_hdr
            ->ramdisk_image;
    struct Elf64_Shdr *rela, *symtab, *symhead =
            (struct Elf64_Shdr *) (boot_hdr->ramdisk_image
                    + (uintptr_t) cpu_head->e_shoff);

    genvaddr_t elfbase = elf_virtual_base64(cpu_head);

    rela = elf64_find_section_header_type(symhead, cpu_head->e_shnum, SHT_RELA);

    symtab = elf64_find_section_header_type(symhead, cpu_head->e_shnum, SHT_DYNSYM);

    elf64_relocate(
            kernel_start,
            elfbase,
            (struct Elf64_Rela *) (uintptr_t) (boot_hdr->ramdisk_image
                    + rela->sh_offset),
            rela->sh_size,
            (struct Elf64_Sym *) (uintptr_t) (boot_hdr->ramdisk_image
                    + symtab->sh_offset),
            symtab->sh_size, elfbase, (void *) kernel_start);

    kernel_entry = kernel_entry - elfbase + kernel_start;

    //set_elf_headers();
    print_status('S', '2');

    // void (*arch_init)(uint64_t magic, void *pointer);
    // arch_init= (void(*)(uint64_t, void *))kernel_entry;

    multiboot_info.flags = 123;
    multiboot_info.vbe_interface_len = 456;

    //arch_init(K1OM_BOOT_MAGIC, &multiboot_info);

    return kernel_entry;

#if 0
    // Store important registers
    multiboot_info = mb;
    eax = magic;

    // Look for the kernel to boot, which may have several names
    struct multiboot_modinfo *kernel;
    kernel = multiboot_find_module("cpu");
    if (kernel == NULL) {
        kernel = multiboot_find_module("kernel");
    }

    // Reserve a page before kernel start
    phys_alloc_start = ROUND_UP(multiboot_end_addr(), BASE_PAGE_SIZE) +
    BASE_PAGE_SIZE;
    lpaddr_t kernel_start = phys_alloc_start;

    err = elf64_load(EM_X86_64, linear_alloc, NULL, kernel->mod_start,
            MULTIBOOT_MODULE_SIZE(*kernel), &kernel_entry, NULL, NULL, NULL);
    if (err_is_fail(err)) {
        printf("Elver ELF loading failed!\n");
        return -1;
    }

    // Relocate kernel image
    struct Elf64_Ehdr *cpu_head = (struct Elf64_Ehdr *)kernel->mod_start;
    struct Elf64_Shdr *rela, *symtab, *symhead =
    (struct Elf64_Shdr *)(kernel->mod_start + (uintptr_t)cpu_head->e_shoff);
    genvaddr_t elfbase = elf_virtual_base64(cpu_head);
    rela = elf64_find_section_header_type(symhead, cpu_head->e_shnum, SHT_RELA);
    symtab = elf64_find_section_header_type(symhead, cpu_head->e_shnum, SHT_DYNSYM);
    elf64_relocate(kernel_start, elfbase,
            (struct Elf64_Rela *)(uintptr_t)(kernel->mod_start + rela->sh_offset),
            rela->sh_size,
            (struct Elf64_Sym *)(uintptr_t)(kernel->mod_start + symtab->sh_offset),
            symtab->sh_size,
            elfbase, (void *)kernel_start);
    kernel_entry = kernel_entry - elfbase + kernel_start;

    // Identity map the first 1 GByte of physical memory in long mode
    paging_map_table(&boot_pml4[PML4_BASE(0)], (uint64_t)(uint32_t)pdpt);
    paging_map_table(&pdpt[PDPT_BASE(0)], (uint64_t)(uint32_t)pdir);
    for(uint32_t i = 0; i < 0xf000000; i += 0x200000) {
        paging_map_large(&pdir[PDIR_BASE(i)], i, PTABLE_PRESENT
                | PTABLE_READ_WRITE | PTABLE_USER_SUPERVISOR);
    }

    // Put real kernel's ELF symbols into multiboot
    set_elf_headers(kernel->mod_start);
#endif
    return 0;
}

