/**
 * \file
 * \brief Rudimentary ELF64 loader and handling routines.
 *
 * Note that on 32-bit platforms, this loader is only able to load
 * ELF64 files that it can address (ie. those that are not bigger than
 * what fits into a 32-bit address space).
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

/* Restricted includes, because this file is used in three environments:
 * in the preboot loader tool "elver", in 32-bit mode
 * in-kernel, 64-bit mode
 * userspace, 64-bit mode
 */
#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/types.h>
#include <errors/errno.h>
#include <elf/elf.h>

#define XEON_PHI_SBOX_BASE           0x08007D0000ULL     /* PCIE Box Registers */
union status
{
    uint32_t raw;
    char vals[4];
};

static void
print_status(char a,
             char b)
{

    volatile uint32_t *p = (volatile uint32_t *) ((XEON_PHI_SBOX_BASE) + 0x0000AB40);
    volatile uint32_t *p2 = (volatile uint32_t *) ((XEON_PHI_SBOX_BASE) + 0x0000AB5C);

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



/**
 * \brief Calculates the base of the loadable portion of the elf image in
 * virtual memory.
 */
genvaddr_t elf_virtual_base64(struct Elf64_Ehdr *ehead)
{
    struct Elf64_Phdr *phead =
        (struct Elf64_Phdr *)((uintptr_t)ehead + (uintptr_t)ehead->e_phoff);

    genvaddr_t retval = 0;
    int i;

    for (i = 0; i < ehead->e_phnum; i++) {
        struct Elf64_Phdr *p = &phead[i];
        if (p->p_type == PT_LOAD) {
            if(retval == 0) {
                retval = p->p_vaddr;
            }
            retval = p->p_vaddr < retval ? p->p_vaddr : retval;
        }
    }

    return retval;
}

/**
 * \brief Return pointer to relocation section ELF header.
 *
 * This function finds and returns a pointer to the first ELF section
 * header of type 'type'.
 *
 * \param shdr          Pointer to head of ELF section header table.
 * \param entries       Number of entries in the ELF section header table.
 * \param type          ELF section header type to look for.
 *
 * \return Pointer to first ELF section header of type 'type', or NULL.
 */
struct Elf64_Shdr *
elf64_find_section_header_type(struct Elf64_Shdr * shdr,
                               uint32_t entries, uint32_t type)
{
    int i;

    for(i = 0; i < entries; i++) {
        struct Elf64_Shdr *s = &shdr[i];

        if(s->sh_type == type) {
            return s;
        }
    }

    return NULL;
}

/**
 * \brief Return pointer to section header with given name.
 *
 * @param elf_base      Address of ELF header
 * @param elf_bytes     Size of ELF file.
 * @param section_name  Named section to look for.
 *
 * @return Pointer to ELF section header with name, or NULL.
 */
struct Elf64_Shdr *
elf64_find_section_header_name(genvaddr_t  elf_base,
                               size_t      elf_bytes,
                               const char* section_name)
{
    lvaddr_t elf_lbase = (lvaddr_t)elf_base;
    struct Elf64_Ehdr *head = (struct Elf64_Ehdr *)elf_lbase;

    if (elf_bytes < sizeof(struct Elf64_Ehdr) || !IS_ELF(*head) ||
        head->e_ident[EI_CLASS] != ELFCLASS64) {
        return NULL;
    }

    struct Elf64_Shdr *shead =
        (struct Elf64_Shdr *)(elf_lbase + (uintptr_t)head->e_shoff);

    struct Elf64_Shdr *strtab =
        elf64_find_section_header_type(shead, head->e_shnum, SHT_STRTAB);

    if (strtab == NULL)
    {
        return NULL;
    }

    for (uint32_t i = 0; i < head->e_shnum; i++)
    {
        const char* strings = (const char*)(elf_lbase +
                                            (size_t)strtab->sh_offset);
        if (!strcmp(section_name, strings + shead[i].sh_name)) {
            return &shead[i];
        }
    }
    return NULL;
}

/**
 * \brief Return pointer to relocation section ELF header.
 *
 * This function finds and returns a pointer to the first ELF section
 * header at virtual address 'addr'.
 *
 * \param shdr          Pointer to head of ELF section header table.
 * \param entries       Number of entries in the ELF section header table.
 * \param addr          Virtual address to look for
 *
 * \return Pointer to first ELF section header loaded at 'addr', or NULL.
 */
static struct Elf64_Shdr *
elf64_find_section_header_vaddr(struct Elf64_Shdr * shdr,
                                uint32_t entries, genvaddr_t addr)
{
    int i;

    for(i = 0; i < entries; i++) {
        struct Elf64_Shdr *s = &shdr[i];

        if(s->sh_addr == addr) {
            return s;
        }
    }

    return NULL;
}

/**
 * \brief Relocates the ELF image from src to dst.
 *
 * This function processes the ELF relocation section 'rela' of size 'size' of
 * the ELF image, formerly located at 'src', to the new location 'dst'.
 * Relocation is necessary for certain variables that cannot be coded as
 * position-independent code.
 *
 * \param dst           Address to relocate to.
 * \param src           Former base address of the ELF image.
 * \param rela          Pointer to relocation section of the ELF image.
 * \param size          Size in bytes of the ELF relocation section.
 * \param symtab        Pointer to ELF symbol table.
 * \param symsize       Size in bytes of the ELF symbol table.
 * \param start         Original base address of the ELF image (needed for
 * symbol-table-based relocations -- we don't touch the symbol table).
 * \param vbase         Pointer to ELF image in virtual memory.
 */
void elf64_relocate(genvaddr_t dst, genvaddr_t src,
                    struct Elf64_Rela * rela, size_t size,
                    struct Elf64_Sym * symtab, size_t symsize,
                    genvaddr_t start, void *vbase)
{
    genvaddr_t base = dst - src, abase = dst - start;

    for(int i = 0; i < size / sizeof(struct Elf64_Rela); i++) {
        struct Elf64_Rela *r = &rela[i];
        uint32_t type = ELF64_R_TYPE(r->r_info);
        uint64_t *addr = (uint64_t *)((char *)vbase + r->r_offset - start);

        switch(type) {
        case R_X86_64_NONE:
            // Do nothing
            break;

        case R_X86_64_64:{
            uint32_t sym = ELF64_R_SYM(r->r_info);
            assert(sym < symsize / sizeof(struct Elf64_Sym));
#if 0 // XXX: symbols should be 0 but this fires sometimes
            assert(symtab[sym].st_value != 0);
#endif
            *addr = abase + symtab[sym].st_value + r->r_addend;
            break;}

        case R_X86_64_RELATIVE:
            // FIXME: why doesn't the following work? -AB
            // I don't think this makes sense. It is a relative
            // relocation from the old position. Thus, base and not
            // abase should be used. Further, since r->r_addend is not
            // updated between relocations, this will fail as soon as
            // a binary is relocated more than once. -SP
            //*addr = abase + r->r_addend;
            *addr += base;
            break;

        default:
            eabort('E','x');
            printf("elf_relocate: relocation %d type %"PRIu32"\n", i, type);
            assert(!"Unimplemented: Cannot handle relocation type");
            break;
        }
    }
}

/**
 * \brief Load ELF64 binary image into memory
 *
 * This function loads an ELF64 binary image, based at 'base' and of size
 * 'size' into the memory provided by 'allocate'
 *
 * \param em_machine    ELF machine type.
 * \param allocate      Memory allocation function.
 * \param state         Pointer to state for allocation function.
 * \param base          Base address of ELF64 binary image in memory.
 * \param size          Size of ELF64 binary image in bytes.
 * \param retentry      Used to return entry point address
 * \param ret_tlsbase   Used to return TLS block base address
 * \param ret_tlsinitlen Used to return length of initialised TLS data block
 * \param ret_tlstotallen Used to return total length of TLS data
 */
errval_t elf64_load(uint16_t em_machine, elf_allocator_fn allocate_func,
                    void *state, lvaddr_t base, size_t size,
                    genvaddr_t *retentry,
                    genvaddr_t *ret_tlsbase, size_t *ret_tlsinitlen,
                    size_t *ret_tlstotallen)
{
    struct Elf64_Ehdr   *head = (struct Elf64_Ehdr *)base;
    errval_t err;
    int i;

    // Check for valid file size
    if (size < sizeof(struct Elf64_Ehdr)) {
        return ELF_ERR_FILESZ;
    }

    // Check for compatible ELF64 header
    if (!IS_ELF(*head)
        || head->e_ident[EI_CLASS] != ELFCLASS64
        || head->e_ident[EI_DATA] != ELFDATA2LSB
        || head->e_ident[EI_VERSION] != EV_CURRENT
        || head->e_ident[EI_OSABI] != ELFOSABI_SYSV
        || head->e_ident[EI_ABIVERSION] != 0
        || (head->e_type != ET_EXEC && head->e_type != ET_DYN)
        || head->e_machine != em_machine
        || head->e_version != EV_CURRENT) {
        return ELF_ERR_HEADER;
    }

    // More sanity checks
    if (head->e_phoff + head->e_phentsize * head->e_phnum > size
        || head->e_phentsize != sizeof(struct Elf64_Phdr)) {
        return ELF_ERR_PROGHDR;
    }

    struct Elf64_Shdr *shead =
        (struct Elf64_Shdr *)(base + (uintptr_t)head->e_shoff);
    struct Elf64_Shdr *rela =
        elf64_find_section_header_type(shead, head->e_shnum, SHT_RELA);
    struct Elf64_Shdr *symtab =
        elf64_find_section_header_type(shead, head->e_shnum, SHT_SYMTAB);

    size_t rela_size = rela ? rela->sh_size : 0, new_rela_size = 0;
    struct Elf64_Shdr *new_rela = NULL;

    // Find dynamic program header, if any
    struct Elf64_Phdr *phead =
        (struct Elf64_Phdr *)(base + (uintptr_t)head->e_phoff);
    for (i = 0; i < head->e_phnum; i++) {
        struct Elf64_Phdr *p = &phead[i];

        if (p->p_type == PT_DYNAMIC) {
            struct Elf64_Dyn *dynamic = (void *)(base + (uintptr_t)p->p_offset);
            int n_dynamic = p->p_filesz / sizeof(struct Elf64_Dyn);
            for (int j = 0; j < n_dynamic; j++) {
                switch (dynamic[j].d_tag) {
                case DT_RELA:
                    // virtual address of relocations, look for matching section
                    new_rela =
                        elf64_find_section_header_vaddr(shead, head->e_shnum,
                                                        dynamic[j].d_un.d_val);
                    break;

                case DT_RELASZ:
                    // store size of relocations, as they may cover more than
                    // one section
                    new_rela_size = dynamic[j].d_un.d_val;
                    break;

                case DT_SYMTAB:
                    // virtual address of symtab, look for matching section
                    symtab =
                        elf64_find_section_header_vaddr(shead, head->e_shnum,
                                                        dynamic[j].d_un.d_val);
                    break;

                case DT_SYMENT:
                    assert(dynamic[j].d_un.d_val == sizeof(struct Elf64_Sym));
                    break;
                }
            }

            if (new_rela != NULL) {
                assert(new_rela_size != 0);
                rela = new_rela;
                rela_size = new_rela_size;
            }
            break;
        }
    }


    genvaddr_t tls_base = 0;
    size_t tls_init_len = 0, tls_total_len = 0;

    // Process program headers to load file
    for (i = 0; i < head->e_phnum; i++) {
        struct Elf64_Phdr *p = &phead[i];
        if (p->p_type == PT_LOAD) {

            // Map segment in user-space memory
            void *dest = NULL;
            err = allocate_func(state, p->p_vaddr, p->p_memsz, p->p_flags, &dest);

            if (err_is_fail(err)) {
                return err_push(err, ELF_ERR_ALLOCATE);
            }
            assert(dest != NULL);
            // Copy file segment into memory
            memcpy(dest, (void *)(base + (uintptr_t)p->p_offset), p->p_filesz);

            // Initialize rest of memory segment (ie. BSS) with all zeroes
            memset((char *)dest + p->p_filesz, 0, p->p_memsz - p->p_filesz);

            // Apply relocations
            if (rela != NULL && symtab != NULL) {
                elf64_relocate(p->p_vaddr, p->p_vaddr,
                               (struct Elf64_Rela *)
                               (base + (uintptr_t)rela->sh_offset),
                               rela_size,
                               (struct Elf64_Sym *)
                               (base + (uintptr_t)symtab->sh_offset),
                               symtab->sh_size, p->p_vaddr, dest);
            }
        } else if (p->p_type == PT_TLS) {
            assert(p->p_vaddr != 0);
            assert(tls_base == 0); // if not we have multiple TLS sections!
            tls_base = p->p_vaddr;
            tls_init_len = p->p_filesz;
            tls_total_len = p->p_memsz;
        }
    }

    if (retentry != NULL) {
        *retentry = head->e_entry;
    }

    if (ret_tlsbase != NULL) {
        *ret_tlsbase = tls_base;
    }

    if (ret_tlsinitlen != NULL) {
        *ret_tlsinitlen = tls_init_len;
    }

    if (ret_tlstotallen != NULL) {
        *ret_tlstotallen = tls_total_len;
    }

    return SYS_ERR_OK;
}
