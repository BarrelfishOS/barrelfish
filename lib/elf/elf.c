/**
 * \file
 * \brief Rudimentary ELF loader and handling routines.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

/* Restricted includes, because this file is used in three environments:
 * in the preboot loader tool "elver", in 32-bit mode
 * in-kernel, 64-bit mode
 * userspace, 64-bit mode
 */
#include <stddef.h>
#include <stdint.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/types.h>
#include <errors/errno.h>
#include <elf/elf.h>

/**
 * \brief Load ELF binary image into memory
 *
 * This function loads an ELF binary image, based at 'base' and of size
 * 'size' into the memory provided by 'allocate'
 *
 * \param em_machine    ELF machine type.
 * \param allocate      Memory allocation function.
 * \param state         Pointer to state for allocation function.
 * \param base          Base address of ELF binary image in memory.
 * \param size          Size of ELF binary image in bytes.
 * \param retentry      Used to return entry point address
 * \param ret_tlsbase   Used to return TLS block base address
 * \param ret_tlssize   Used to return TLS block size
 */
errval_t elf_load_tls(uint16_t em_machine, elf_allocator_fn allocate_func,
                      void *state, lvaddr_t base,
                      size_t size, genvaddr_t *retentry,
                      genvaddr_t *ret_tlsbase, size_t *ret_tlsinitlen,
                      size_t *ret_tlstotallen)
{
    struct Elf64_Ehdr *head = (struct Elf64_Ehdr *)base;

    if(!IS_ELF(*head)) {
        return ELF_ERR_HEADER;
    }

    switch(head->e_ident[EI_CLASS]) {
    case ELFCLASS32:
        return elf32_load(em_machine, allocate_func, state, base, size,
                          retentry, ret_tlsbase, ret_tlsinitlen, ret_tlstotallen);

    case ELFCLASS64:
        return elf64_load(em_machine, allocate_func, state, base, size,
                          retentry, ret_tlsbase, ret_tlsinitlen, ret_tlstotallen);
    }

    return ELF_ERR_HEADER;
}

errval_t elf_load(uint16_t em_machine, elf_allocator_fn allocate_func,
                  void *state, lvaddr_t base,
                  size_t size, genvaddr_t *retentry)
{
    return elf_load_tls(em_machine, allocate_func, state, base, size,
                        retentry, NULL, NULL, NULL);
}

static size_t elf_virtual_size64(struct Elf64_Ehdr *ehead)
{
    struct Elf64_Phdr *phead =
        (struct Elf64_Phdr *)((uintptr_t)ehead + (uintptr_t)ehead->e_phoff);

    size_t retval = 0;
    int i;

    for (i = 0; i < ehead->e_phnum; i++) {
        struct Elf64_Phdr *p = &phead[i];
        if (p->p_type == PT_LOAD) {
            retval = p->p_vaddr + p->p_memsz;
        }
    }

    return retval - elf_virtual_base64(ehead);
}

static size_t elf_virtual_size32(struct Elf32_Ehdr *ehead)
{
    struct Elf32_Phdr *phead =
        (struct Elf32_Phdr *)((uintptr_t)ehead + (uintptr_t)ehead->e_phoff);

    size_t retval = 0;
    int i;

    for (i = 0; i < ehead->e_phnum; i++) {
        struct Elf32_Phdr *p = &phead[i];
        if (p->p_type == PT_LOAD) {
            retval = p->p_vaddr + p->p_memsz;
        }
    }

    return retval - elf_virtual_base32(ehead);
}

/**
 * \brief Calculates the size of the loadable portion of the elf image in
 * virtual memory. This is the amount of virtual memory required to load an
 * image.
 */
size_t elf_virtual_size(lvaddr_t base)
{
    size_t elfsize = 0;

    struct Elf64_Ehdr *ehead = (struct Elf64_Ehdr *)base;
    if (IS_ELF(*ehead)) {
        switch (ehead->e_ident[EI_CLASS]) {
          case ELFCLASS64:
            elfsize = elf_virtual_size64(ehead);
            break;

          case ELFCLASS32:
            elfsize = elf_virtual_size32((struct Elf32_Ehdr*)ehead);
            break;
        }
    }

    return elfsize;
}

/**
 * \brief Calculates the base of the loadable portion of the elf image in
 * virtual memory.
 */
genvaddr_t elf_virtual_base(lvaddr_t base)
{
    genvaddr_t elfbase = 0;

    struct Elf64_Ehdr *ehead = (struct Elf64_Ehdr *)base;
    if (IS_ELF(*ehead)) {
        switch (ehead->e_ident[EI_CLASS]) {
          case ELFCLASS64:
            elfbase = elf_virtual_base64(ehead);
            break;

          case ELFCLASS32:
            elfbase = elf_virtual_base32((struct Elf32_Ehdr*)ehead);
            break;
        }
    }

    return elfbase;
}

static errval_t elf32_get_eh_info(lvaddr_t elfbase, size_t elfsize,
                                  lvaddr_t *eh_frame, size_t *eh_frame_size,
                                  lvaddr_t *eh_frame_hdr, size_t *eh_frame_hdr_size)
{
    struct Elf32_Shdr *shdr;

    lvaddr_t addr = 0;
    size_t size = 0;

    shdr= elf32_find_section_header_name(elfbase, elfsize, ".eh_frame");
    if (shdr != NULL) {
        size = shdr->sh_size;
        addr = shdr->sh_addr;
    }

    if (eh_frame) {
        *eh_frame = addr;
    }
    if (eh_frame_size) {
        *eh_frame_size = size;
    }

    size = 0;
    addr = 0;

    shdr= elf32_find_section_header_name(elfbase, elfsize, ".eh_frame_hdr");
    if (shdr != NULL) {
        size = shdr->sh_size;
        addr = shdr->sh_addr;
    }

    if (eh_frame_hdr) {
        *eh_frame_hdr = addr;
    }
    if (eh_frame_hdr_size) {
        *eh_frame_hdr_size = size;
    }

    return SYS_ERR_OK;
}

static errval_t elf64_get_eh_info(lvaddr_t elfbase, size_t elfsize,
                                  lvaddr_t *eh_frame, size_t *eh_frame_size,
                                  lvaddr_t *eh_frame_hdr, size_t *eh_frame_hdr_size)
{
    struct Elf64_Shdr *shdr;

    lvaddr_t addr = 0;
    size_t size = 0;

    shdr= elf64_find_section_header_name(elfbase, elfsize, ".eh_frame");
    if (shdr != NULL) {
        size = shdr->sh_size;
        addr = shdr->sh_addr;
    }

    if (eh_frame) {
        *eh_frame = addr;
    }
    if (eh_frame_size) {
        *eh_frame_size = size;
    }

    size = 0;
    addr = 0;

    shdr= elf64_find_section_header_name(elfbase, elfsize, ".eh_frame_hdr");
    if (shdr != NULL) {
        size = shdr->sh_size;
        addr = shdr->sh_addr;
    }

    if (eh_frame_hdr) {
        *eh_frame_hdr = addr;
    }
    if (eh_frame_hdr_size) {
        *eh_frame_hdr_size = size;
    }

    return SYS_ERR_OK;
}

/**
 * \brief obtains the error handling frame information form the elf image
 *
 * \param elfbase            virtual base address of the mapped elf
 * \param elfsize            size of the elf in bytes
 * \param eh_frame           returns the virtual address of the eh_frame
 * \param eh_frame_size      returns the size of the eh_frame
 * \param eh_frame_hdr       returns the virtual address of the eh_frame_hdr
 * \param eh_frame_hdr_size  returns the size of the eh_frame_hdr
 *
 * \returns SYS_ERR_OK on success
 */
errval_t elf_get_eh_info(lvaddr_t elfbase, size_t elfsize,
                         lvaddr_t *eh_frame, size_t *eh_frame_size,
                         lvaddr_t *eh_frame_hdr, size_t *eh_frame_hdr_size)
{
    struct Elf64_Ehdr *ehead = (struct Elf64_Ehdr *)elfbase;
    if (IS_ELF(*ehead)) {
        switch (ehead->e_ident[EI_CLASS]) {
          case ELFCLASS64:
            return elf64_get_eh_info(elfbase, elfsize, eh_frame, eh_frame_size,
                                     eh_frame_hdr, eh_frame_hdr_size);
            break;
          case ELFCLASS32:
            return  elf32_get_eh_info(elfbase, elfsize, eh_frame, eh_frame_size,
                                      eh_frame_hdr, eh_frame_hdr_size);
            break;
        }
    }
    return ELF_ERR_HEADER;
}
