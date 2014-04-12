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

#include "mbi.h"
#include "kernel_boot_param.h"

#include "../../kernel/include/multiboot.h"

/* the boot magic */
#define K1OM_BOOT_MAGIC         0xB001B001

/* the address of the Xeon Phi SBOX registers used for status prints*/
#define SBOX_BASE           0x08007D0000ULL

/* reference to the end of bootloader */
extern char _end_bootloader;

/// Round up n to the next multiple of size
#define ROUND_UP(n, size)           ((((n) + (size) - 1)) & (~((size) - 1)))
#define BASE_PAGE_SIZE 0x1000

/* Pointer to the multiboot struct we use */
struct multiboot_info *multiboot_info;

/* Address where we can safely allocate memory */
static lpaddr_t phys_alloc_start;

/* the entry address of the loaded kernel */
genvaddr_t kernel_entry;

/**
 * C level entry point for the boot loader
 *
 * \param magic this field must hold the value K1OM_BOOT_MAGIC
 * \param mb    pointer to the boot_params struct setup by the boot loader
 */
int
loader(uint64_t magic,
       struct boot_params *mb);

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

    *ret = (void *) phys_alloc_start;

    phys_alloc_start += npages * BASE_PAGE_SIZE;
    return SYS_ERR_OK;
}


static struct multiboot_modinfo *
multiboot_find_module(const char *basename)
{
    struct multiboot_modinfo *mod;
    mod = (struct multiboot_modinfo *) (uintptr_t) multiboot_info->mods_addr;

    for (size_t i = 0; i < multiboot_info->mods_count; i++) {
        const char *modname = strrchr((char *) (uintptr_t) mod[i].string, '/');

        if (modname == NULL) {
            modname = (char *) (uintptr_t) mod[i].string;
        } else {
            modname++;
        }

        if (!strncmp(modname, basename, strlen(basename))) {
            return &mod[i];
        }
    }

    return NULL;
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

/*
 * ----------------------------------------------------------------------------
 *  Loader
 * ----------------------------------------------------------------------------
 */

/**
 * Entry point from boot.S
 * Long mode, paging and protected mode enabled
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

    multiboot_info = get_multiboot();

    /*
     * Copy the multi boot image closer to the kernel
     */
    lpaddr_t mb_img_start = ROUND_UP((lpaddr_t )&_end_bootloader, 1 << 21);
    lpaddr_t mb_img_orig = boot_hdr->ramdisk_image;

    /* sanity check for the locations */
    if (mb_img_start > mb_img_orig) {
        eabort('E', 'C');
    }
    memcpy((void *) mb_img_start, (void *) mb_img_orig, boot_hdr->ramdisk_size);

    /*
     * the multiboot does only stores the offsets within the multiboot image
     * thus we have to adjust the addresses in the multiboot info struct
     */
    struct multiboot_modinfo *mod;
    mod = (struct multiboot_modinfo *) (uintptr_t) multiboot_info->mods_addr;

    for (size_t i = 0; i < multiboot_info->mods_count; i++) {
        mod->mod_start += mb_img_start;
        mod->mod_end += mb_img_start;
        mod++;
    }

    /* look up the kernel module */
    struct multiboot_modinfo *kernel;
    kernel = multiboot_find_module("cpu");
    if (kernel == NULL) {
        kernel = multiboot_find_module("kernel");
    }

    /* set the start address where we can allocate ram */
    phys_alloc_start = ROUND_UP(mb_img_start + boot_hdr->ramdisk_size,
                                BASE_PAGE_SIZE);

    boot_hdr->ramdisk_image = (uint32_t) mb_img_start;

    lpaddr_t kernel_start = phys_alloc_start;

    /* overwrite the cmd line with the one supplied by the host */
    multiboot_info->cmdline = boot_hdr->cmd_line_ptr;

    /* we use the mem_lower and mem_upper for the mulitboot image location */
    multiboot_info->mem_lower = boot_hdr->ramdisk_image;
    multiboot_info->mem_upper = boot_hdr->ramdisk_image+boot_hdr->ramdisk_size;

    /* we use the config table to store the pointer to struct boot param */
    multiboot_info->config_table = (uint32_t)(uintptr_t)bootparam;

    err = elf64_load(EM_K1OM, linear_alloc, NULL, kernel->mod_start,
                     MULTIBOOT_MODULE_SIZE(*kernel), &kernel_entry, NULL, NULL,
                     NULL);

    if (err_is_fail(err)) {
        eabort('E', '1');
    }

    struct Elf64_Ehdr *cpu_head = (struct Elf64_Ehdr *) (uint64_t) kernel->mod_start;
    struct Elf64_Shdr *rela, *symtab, *symhead;

    symhead = (struct Elf64_Shdr *) (kernel->mod_start
            + (uintptr_t) cpu_head->e_shoff);

    genvaddr_t elfbase = elf_virtual_base64(cpu_head);

    rela = elf64_find_section_header_type(symhead, cpu_head->e_shnum, SHT_RELA);

    symtab = elf64_find_section_header_type(symhead, cpu_head->e_shnum, SHT_DYNSYM);

    elf64_relocate(
            kernel_start, elfbase,
            (struct Elf64_Rela *) (uintptr_t) (kernel->mod_start + rela->sh_offset),
            rela->sh_size,
            (struct Elf64_Sym *) (uintptr_t) (kernel->mod_start + symtab->sh_offset),
            symtab->sh_size, elfbase, (void *) kernel_start);

    kernel_entry = kernel_entry - elfbase + kernel_start;

    set_elf_headers(kernel->mod_start);
    print_status('S', '2');

    return kernel_entry;

}

