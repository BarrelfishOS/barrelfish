/**
 * \file
 * \brief functionality to spawn domains
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <spawndomain/spawndomain.h>
#include <barrelfish/dispatcher_arch.h>
#include "spawn.h"
#include <elf/elf.h>
#include "../../arch.h"

#if defined(__i386__)
#define EM_HOST EM_386
#elif defined(__k1om__)
#define EM_HOST EM_K1OM
#elif defined(__x86_64__)
#define EM_HOST EM_X86_64
#else
#error "Unexpected architecture."
#endif

/**
 * \brief Convert elf flags to vregion flags
 */
static vregion_flags_t elf_to_vregion_flags(uint32_t flags)
{
    vregion_flags_t vregion_flags = 0;

    if (flags & PF_R) {
        vregion_flags |= VREGION_FLAGS_READ;
    }
    if (flags & PF_W) {
        vregion_flags |= VREGION_FLAGS_WRITE;
    }
    if (flags & PF_X) {
        vregion_flags |= VREGION_FLAGS_EXECUTE;
    }

    return vregion_flags;
}

static errval_t elf_allocate(void *state, genvaddr_t base, size_t size,
                             uint32_t flags, void **retbase)
{
    errval_t err;

    struct spawninfo *si = state;

    // Increase size by space wasted on first page due to page-alignment
    size_t base_offset = BASE_PAGE_OFFSET(base);
    size += base_offset;
    base -= base_offset;
    // Page-align
    size = ROUND_UP(size, BASE_PAGE_SIZE);

    cslot_t vspace_slot = si->elfload_slot;

    // Allocate the frames
    size_t sz = 0;
    for (lpaddr_t offset = 0; offset < size; offset += sz) {
        sz = 1UL << log2floor(size - offset);
        struct capref frame = {
            .cnode = si->segcn,
            .slot  = si->elfload_slot++,
        };
        err = frame_create(frame, sz, NULL);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_FRAME_CREATE);
        }
    }

    cslot_t spawn_vspace_slot = si->elfload_slot;
    cslot_t new_slot_count = si->elfload_slot - vspace_slot;

    // create copies of the frame capabilities for spawn vspace
    for (int copy_idx = 0; copy_idx < new_slot_count; copy_idx++) {
        struct capref frame = {
            .cnode = si->segcn,
            .slot = vspace_slot + copy_idx,
        };
        struct capref spawn_frame = {
            .cnode = si->segcn,
            .slot = si->elfload_slot++,
        };
        err = cap_copy(spawn_frame, frame);
        if (err_is_fail(err)) {
            // TODO: make debug printf
            printf("cap_copy failed for src_slot = %"PRIuCSLOT", dest_slot = %"PRIuCSLOT"\n", frame.slot, spawn_frame.slot);
            return err_push(err, LIB_ERR_CAP_COPY);
        }
    }

    /* Map into my vspace */
    struct memobj *memobj = malloc(sizeof(struct memobj_anon));
    if (!memobj) {
        return LIB_ERR_MALLOC_FAIL;
    }
    struct vregion *vregion = malloc(sizeof(struct vregion));
    if (!vregion) {
        return LIB_ERR_MALLOC_FAIL;
    }
    // Create the objects
    err = memobj_create_anon((struct memobj_anon*)memobj, size, 0);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_MEMOBJ_CREATE_ANON);
    }
    err = vregion_map(vregion, get_current_vspace(), memobj, 0, size,
                      VREGION_FLAGS_READ_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }
    for (lvaddr_t offset = 0; offset < size; offset += sz) {
        sz = 1UL << log2floor(size - offset);
        struct capref frame = {
            .cnode = si->segcn,
            .slot  = vspace_slot++,
        };
        genvaddr_t genvaddr = vspace_lvaddr_to_genvaddr(offset);
        err = memobj->f.fill(memobj, genvaddr, frame, sz);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_MEMOBJ_FILL);
        }
        err = memobj->f.pagefault(memobj, vregion, offset, 0);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "lib_err_memobj_pagefault_handler");
            return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
        }
    }

    /* Map into spawn vspace */
    struct memobj *spawn_memobj = NULL;
    struct vregion *spawn_vregion = NULL;
    err = spawn_vspace_map_anon_fixed_attr(si, base, size, &spawn_vregion,
                                           &spawn_memobj,
                                           elf_to_vregion_flags(flags));
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_VSPACE_MAP);
    }
    for (lvaddr_t offset = 0; offset < size; offset += sz) {
        sz = 1UL << log2floor(size - offset);
        struct capref spawn_frame = {
            .cnode = si->segcn,
            .slot = spawn_vspace_slot++,
        };
        genvaddr_t genvaddr = vspace_lvaddr_to_genvaddr(offset);
        err = memobj->f.fill(spawn_memobj, genvaddr, spawn_frame, sz);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_MEMOBJ_FILL);
        }
        err = spawn_memobj->f.pagefault(spawn_memobj, spawn_vregion, offset, 0);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "lib_err_memobj_pagefault_handler");
            return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
        }
    }

    si->vregion[si->vregions] = vregion;
    si->base[si->vregions++] = base;

    genvaddr_t genvaddr = vregion_get_base_addr(vregion) + base_offset;
    *retbase = (void*)vspace_genvaddr_to_lvaddr(genvaddr);
    return SYS_ERR_OK;
}

static errval_t spawn_parse_omp_functions(const char *name,
                                          lvaddr_t binary, size_t binary_size)
{
    errval_t err;
    genvaddr_t value;
    err = spawn_symval_lookup(name, 0, NULL, &value);
    if (err_is_ok(err)) {
        return SYS_ERR_OK;
    }

    uint32_t count = 0;

    struct Elf64_Sym *sym = NULL;
    struct Elf64_Shdr *shead;
    struct Elf64_Shdr *symtab;
    const char *symname = NULL;

    lvaddr_t elfbase = (lvaddr_t)binary;
    struct Elf64_Ehdr *head = (struct Elf64_Ehdr *)elfbase;

    // just a sanity check
    if (!IS_ELF(*head) || head->e_ident[EI_CLASS] != ELFCLASS64) {
        return ELF_ERR_HEADER;
    }

    shead = (struct Elf64_Shdr *)(elfbase + (uintptr_t)head->e_shoff);

    symtab = elf64_find_section_header_type(shead, head->e_shnum, SHT_SYMTAB);

    uintptr_t symbase = elfbase + (uintptr_t)symtab->sh_offset;

    uint32_t symindex = 1;
    for (uintptr_t i = 0; i < symtab->sh_size; i += sizeof(struct Elf64_Sym)) {
        // getting the symbol
        sym = (struct Elf64_Sym *)(symbase + i);

        // check for matching type
        if ((sym->st_info & 0x0F) != STT_FUNC) {
            continue;
        }

        // find the section of the associated string table
        struct Elf64_Shdr *strtab = shead+symtab->sh_link;

        // get the pointer to the symbol name from string table + string index
        symname = (const char *)elfbase + strtab->sh_offset + sym->st_name;

        if (strstr(symname, "_omp_fn") != NULL) {
            count++;
            err = spawn_symval_register(name, symindex++,  symname, sym->st_value);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "could not register symbol. %s\n", symname);
                return err;
            }
        }
    }

    err = spawn_symval_register(name, 0, "binary", count);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not register symbol: %s.binary", name);
        return err;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Load the elf image
 */
errval_t spawn_arch_load(struct spawninfo *si,
                         lvaddr_t binary, size_t binary_size,
                         genvaddr_t *entry, void** arch_load_info)
{
    errval_t err;

    // Reset the elfloader_slot
    si->elfload_slot = 0;
    si->vregions = 0;

    struct capref cnode_cap = {
        .cnode = si->rootcn,
        .slot  = ROOTCN_SLOT_SEGCN,
    };
    // XXX: this code assumes that elf_load never needs more than 32 slots for
    // text frame capabilities.
    err = cnode_create_raw(cnode_cap, &si->segcn, DEFAULT_CNODE_SLOTS, NULL);
    if (err_is_fail(err)) {
        return err_push(err, SPAWN_ERR_CREATE_SEGCN);
    }

    // Load the binary
    si->tls_init_base = 0;
    si->tls_init_len = si->tls_total_len = 0;
    err = elf_load_tls(EM_HOST, elf_allocate, si, binary, binary_size, entry,
                       &si->tls_init_base, &si->tls_init_len, &si->tls_total_len);
    if (err_is_fail(err)) {
        return err;
    }

    lvaddr_t tmp, tmp2;
    err = elf_get_eh_info(binary, binary_size, &tmp, &si->eh_frame_size,
                          &tmp2, &si->eh_frame_hdr_size);
    si->eh_frame = vspace_lvaddr_to_genvaddr(tmp);
    si->eh_frame_hdr = vspace_lvaddr_to_genvaddr(tmp2);

    if (err_is_fail(err)) {
        return err;
    }

    if ((strcmp("spawnd", disp_name())==0) && (si->flags & SPAWN_FLAGS_OMP)) {
        return spawn_parse_omp_functions(si->name, binary, binary_size);
    }

    return SYS_ERR_OK;
}

void spawn_arch_set_registers(void *arch_load_info,
                              dispatcher_handle_t handle,
                              arch_registers_state_t *enabled_area,
                              arch_registers_state_t *disabled_area)
{
#if defined(__x86_64__) || defined(__k1om__)
    /* XXX: 1st argument to _start is the dispatcher pointer
     * see lib/crt/arch/x86_64/crt0.s */
    disabled_area->rdi = get_dispatcher_shared_generic(handle)->udisp;
#elif defined(__i386__)
    /* XXX: 1st argument to _start is the dispatcher pointer
     * see lib/crt/arch/x86_32/crt0.s */
    disabled_area->edi = get_dispatcher_shared_generic(handle)->udisp;
#endif
}
