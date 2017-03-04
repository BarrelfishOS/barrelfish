/**
 * \file
 * \brief Boot driver arch specific parts for ARM CPUs
 */
/*
 * Copyright (c) 2014,2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include "../../coreboot.h"

#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/platform.h>
#include <barrelfish/syscall_arch.h>
#include <target/arm/barrelfish_kpi/arm_core_data.h>

#include <skb/skb.h>

/// Round up n to the next multiple of size
#define ROUND_UP(n, size)           ((((n) + (size) - 1)) & (~((size) - 1)))

struct monitor_allocate_state {
    void          *vbase;
    genvaddr_t     elfbase;
};

struct xcore_bind_handler {
    coreid_t                    coreid;
    enum cpu_type               cputype;
    struct monitor_binding      *binding;
};

extern coreid_t my_arch_id;
extern struct capref ipi_cap;

errval_t get_core_info(coreid_t core_id, 
                       hwid_t* hw_id,
                       enum cpu_type* cpu_type) {
    char* record = NULL;
    errval_t err = oct_get(&record, "hw.processor.%"PRIuCOREID"", core_id);
    if (err_is_fail(err)) return err;

    int enabled, type;
    err = oct_read(record, "_ { hw_id: %d, enabled: %d, type: %d}",
                   hw_id, &enabled, &type);
    assert (enabled);
    if (err_is_fail(err)) return err;

    *cpu_type = (enum cpu_type) type;
    return SYS_ERR_OK;
}

#if 0
static errval_t monitor_elfload_allocate(void *state, genvaddr_t base,
                                         size_t size, uint32_t flags,
                                         void **retbase)
{
    struct monitor_allocate_state *s = state;

    *retbase = (char *)s->vbase + base - s->elfbase;
    return SYS_ERR_OK;
}
#endif

struct module_blob {
    size_t             size;
    lvaddr_t           vaddr;
    genpaddr_t         paddr;
    struct mem_region *mem_region;
};

static errval_t
module_blob_map(const char *name, struct module_blob *blob)
{
    errval_t err;

    err = lookup_module(name, &blob->vaddr,
                        &blob->paddr, &blob->size);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not lookup module");
        return err_push(err, SPAWN_ERR_FIND_MODULE);
    }

    return SYS_ERR_OK;
}

static errval_t
cpu_memory_prepare(size_t *size,
                   struct capref *cap_ret, void **buf_ret,
                   struct frame_identity *frameid)
{
    errval_t err;
    struct capref cap;
    void *buf;

     err = frame_alloc(&cap, *size, size);
     if (err_is_fail(err)) {
         USER_PANIC("Failed to allocate %zd memory\n", *size);
     }

#ifdef __gem5__
    // XXX: We map the frame for the new kernel as uncacheable. Gem5 has a
    // problem when one core has cacheing on and writes to a location where an
    // other core reads from without caches enabled. On real hardware one could
    // clean/flush the cache, but Gem5 doesn't support cache maintenance
    // operations for ARM
    err = vspace_map_one_frame_attr(&buf, *size, cap,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
#else
    err = vspace_map_one_frame(&buf, *size, cap, NULL, NULL);
#endif
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    // Mark memory as remote
    err = cap_mark_remote(cap);
    if (err_is_fail(err)) {
        return err;
    }

    err = invoke_frame_identify(cap, frameid);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_IDENTIFY);
    }

    *cap_ret = cap;
    *buf_ret = buf;
    return SYS_ERR_OK;
}

static errval_t
cpu_memory_cleanup(struct capref cap, void *buf)
{
    errval_t err;

    err = vspace_unmap(buf);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vspace unmap CPU driver memory failed");
    }

    // XXX: Should not delete the remote cap
    err = cap_destroy(cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }

    return SYS_ERR_OK;
}

static errval_t
spawn_memory_prepare(size_t size, struct capref *cap_ret,
                     struct frame_identity *frameid)
{
    errval_t err;
    struct capref cap;

    err = frame_alloc(&cap, size, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }

    // Mark memory as remote
    err = cap_mark_remote(cap);
    if (err_is_fail(err)) {
        return err;
    }

    err = invoke_frame_identify(cap, frameid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "frame_identify failed");
    }

    *cap_ret = cap;
    return SYS_ERR_OK;
}

static errval_t
spawn_memory_cleanup(struct capref cap)
{

    errval_t err;
    err = cap_destroy(cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }

    return SYS_ERR_OK;
}

#if 0
static errval_t
elf_load_and_relocate(lvaddr_t blob_start, size_t blob_size,
                      void *to, lvaddr_t reloc_dest,
                      uintptr_t *reloc_entry)
{
    genvaddr_t entry; // entry poing of the loaded elf image
    struct Elf32_Ehdr *head = (struct Elf32_Ehdr *)blob_start;
    struct Elf32_Shdr *symhead, *rel, *symtab;
    errval_t err;

    //state.vbase = (void *)ROUND_UP(to, ARM_L1_ALIGN);
    struct monitor_allocate_state state;
    state.vbase   = to;
    state.elfbase = elf_virtual_base(blob_start);

    err = elf_load(head->e_machine,
                   monitor_elfload_allocate,
                   &state,
                   blob_start, blob_size,
                   &entry);
    if (err_is_fail(err)) {
        return err;
    }

    // Relocate to new physical base address
    symhead = (struct Elf32_Shdr *)(blob_start + (uintptr_t)head->e_shoff);
    rel = elf32_find_section_header_type(symhead, head->e_shnum, SHT_REL);
    symtab = elf32_find_section_header_type(symhead, head->e_shnum, SHT_DYNSYM);
    assert(rel != NULL && symtab != NULL);

    elf32_relocate(reloc_dest, state.elfbase,
                   (struct Elf32_Rel *)(blob_start + rel->sh_offset),
                   rel->sh_size,
                   (struct Elf32_Sym *)(blob_start + symtab->sh_offset),
                   symtab->sh_size,
                   state.elfbase, state.vbase);

    *reloc_entry = entry - state.elfbase + reloc_dest;
    return SYS_ERR_OK;
}
#endif

/**
 * \brief Spawn a new core.
 *
 * \param cur_kern   Cap of the current kernel
 * \param core_id    APIC ID of the core to try booting
 * \param sp_mem     Cap to Ram type memory to relocate the new kernel
 * \param dcb        Cap to the dcb of the user program to run on the new kernel
 * \param root_vbits Number of valid bits in root_cptr
 * \param root_cptr  Cap to the root of cspace of the new user program
 * \param vtree      Cap to the vtree root of the new user program
 * \param dispatcher Cap to the dispatcher of the new user program
 * \param entry      Kernel entry point in physical memory
 */
static inline errval_t
invoke_monitor_spawn_core(coreid_t core_id, enum cpu_type cpu_type,
                          forvaddr_t entry)
{
    return cap_invoke5(ipi_cap, IPICmd_Send_Start, core_id, cpu_type,
            (uintptr_t)(entry >> 32), (uintptr_t) entry).error;
}

static void
print_build_id(const char *data, size_t length) {
    for(size_t i= 0; i < length; i++) printf("%02x", data[i]);
}

static int
compare_build_ids(const char *data1, size_t length1,
                  const char *data2, size_t length2) {
    if(length1 != length2) return 0;

    for(size_t i= 0; i < length1; i++) {
        if(data1[i] != data2[i]) return 0;
    }

    return 1;
}

/* Return the first program header of type 'type'. */
static struct Elf32_Phdr *
elf32_find_segment_type(void *elfdata, uint32_t type) {
    struct Elf32_Ehdr *ehdr= (struct Elf32_Ehdr *)elfdata;

    if(!IS_ELF(*ehdr) ||
       ehdr->e_ident[EI_CLASS] != ELFCLASS32 ||
       ehdr->e_machine != EM_ARM) {
        return NULL;
    }

    void *phdrs_base= (void *)(elfdata + ehdr->e_phoff);

    for(size_t i= 0; i < ehdr->e_phnum; i++) {
        struct Elf32_Phdr *phdr= phdrs_base + i * ehdr->e_phentsize;

        if(phdr->p_type == type) return phdr;
    }

    return NULL;
}

static errval_t
load_cpu_relocatable_segment(void *elfdata, void *out, lvaddr_t vbase,
                             lvaddr_t text_base, lvaddr_t *got_base) {
    /* Find the full loadable segment, as it contains the dynamic table. */
    struct Elf32_Phdr *phdr_full= elf32_find_segment_type(elfdata, PT_LOAD);
    if(!phdr_full) return ELF_ERR_HEADER;
    void *full_segment_data= elfdata + phdr_full->p_offset;

    printf("Loadable segment at V:%08"PRIx32"\n", phdr_full->p_vaddr);

    /* Find the relocatable segment to load. */
    struct Elf32_Phdr *phdr= elf32_find_segment_type(elfdata, PT_BF_RELOC);
    if(!phdr) return ELF_ERR_HEADER;

    printf("Relocatable segment at V:%08"PRIx32"\n", phdr->p_vaddr);

    /* Copy the raw segment data. */
    void *in= elfdata + phdr->p_offset;
    assert(phdr->p_filesz <= phdr->p_memsz);
    memcpy(out, in, phdr->p_filesz);

    /* Find the dynamic segment. */
    struct Elf32_Phdr *phdr_dyn= elf32_find_segment_type(elfdata, PT_DYNAMIC);
    if(!phdr_dyn) return ELF_ERR_HEADER;

    printf("Dynamic segment at V:%08"PRIx32"\n", phdr_dyn->p_vaddr);

    /* The location of the dynamic segment is specified by its *virtual
     * address* (vaddr), relative to the loadable segment, and *not* by its
     * p_offset, as for every other segment. */
    struct Elf32_Dyn *dyn=
        full_segment_data + (phdr_dyn->p_vaddr - phdr_full->p_vaddr);

    /* There is no *entsize field for dynamic entries. */
    size_t n_dyn= phdr_dyn->p_filesz / sizeof(struct Elf32_Dyn);

    /* Find the relocations (REL only). */
    void *rel_base= NULL;
    size_t relsz= 0, relent= 0;
    void *dynsym_base= NULL;
    const char *dynstr_base= NULL;
    size_t syment= 0, strsz= 0;
    for(size_t i= 0; i < n_dyn; i++) {
        switch(dyn[i].d_tag) {
            /* There shouldn't be any RELA relocations. */
            case DT_RELA:
                return ELF_ERR_HEADER;

            case DT_REL:
                if(rel_base != NULL) return ELF_ERR_HEADER;

                /* Pointers in the DYNAMIC table are *virtual* addresses,
                 * relative to the vaddr base of the segment that contains
                 * them. */
                rel_base= full_segment_data +
                    (dyn[i].d_un.d_ptr - phdr_full->p_vaddr);
                break;

            case DT_RELSZ:
                relsz= dyn[i].d_un.d_val;
                break;

            case DT_RELENT:
                relent= dyn[i].d_un.d_val;
                break;

            case DT_SYMTAB:
                dynsym_base= full_segment_data +
                    (dyn[i].d_un.d_ptr - phdr_full->p_vaddr);
                break;

            case DT_SYMENT:
                syment= dyn[i].d_un.d_val;
                break;

            case DT_STRTAB:
                dynstr_base= full_segment_data +
                    (dyn[i].d_un.d_ptr - phdr_full->p_vaddr);
                break;

            case DT_STRSZ:
                strsz= dyn[i].d_un.d_val;
        }
    }
    if(rel_base == NULL || relsz == 0 || relent == 0 ||
       dynsym_base == NULL || syment == 0 ||
       dynstr_base == NULL || strsz == 0)
        return ELF_ERR_HEADER;

    /* XXX - The dynamic segment doesn't actually tell us the size of the
     * dynamic symbol table, which is very annoying.  We should fix this by
     * defining and implementing a standard format for dynamic executables on
     * Barrelfish, using DT_PLTGOT.  Currently, GNU ld refuses to generate
     * that for the CPU driver binary. */
    assert((size_t)dynstr_base > (size_t)dynsym_base);
    size_t dynsym_len= (size_t)dynstr_base - (size_t)dynsym_base;

    /* Walk the symbol table to find got_base. */
    size_t dynsym_offset= 0;
    struct Elf32_Sym *got_sym= NULL;
    while(dynsym_offset < dynsym_len) {
        got_sym= dynsym_base + dynsym_offset;
        if(!strcmp(dynstr_base + got_sym->st_name, "got_base")) break;

        dynsym_offset+= syment;
    }
    if(dynsym_offset >= dynsym_len) {
        printf("got_base not found.\n");
        return ELF_ERR_HEADER;
    }

    /* Addresses in the relocatable segment are relocated to the
     * newly-allocated region, relative to their addresses in the relocatable
     * segment.  Addresses outside the relocatable segment are relocated to
     * the shared text segment, relative to their position in the
     * originally-loaded segment. */
    uint32_t relocatable_offset= vbase - phdr->p_vaddr;
    uint32_t text_offset= text_base - phdr_full->p_vaddr;

    /* Relocate the got_base within the relocatable segment. */
    *got_base= vbase + (got_sym->st_value - phdr->p_vaddr);

    /* Process the relocations. */
    size_t n_rel= relsz / relent;
    printf("Have %zu relocations of size %zu\n", n_rel, relent);
    for(size_t i= 0; i < n_rel; i++) {
        struct Elf32_Rel *rel= rel_base + i * relent;

        size_t sym=  ELF32_R_SYM(rel->r_info);
        size_t type= ELF32_R_TYPE(rel->r_info);

        /* We should only see relative relocations (R_ARM_RELATIVE) against
         * sections (symbol 0). */
        if(sym != 0 || type != R_ARM_RELATIVE) return ELF_ERR_HEADER;

        uint32_t offset_in_seg= rel->r_offset - phdr->p_vaddr;
        uint32_t *value= out + offset_in_seg;

        uint32_t offset;
        if(*value >= phdr->p_vaddr &&
           (*value - phdr->p_vaddr) < phdr->p_memsz) {
            /* We have a relocation to an address *inside* the relocatable
             * segment. */
            offset= relocatable_offset;
            //printf("r ");
        }
        else {
            /* We have a relocation to an address in the shared text segment.
             * */
            offset= text_offset;
            //printf("t ");
        }

        //printf("REL@%08"PRIx32" %08"PRIx32" -> %08"PRIx32"\n",
               //rel->r_offset, *value, *value + offset);
        *value+= offset;
    }

    return SYS_ERR_OK;
}

/* XXX - this currently only clones the running kernel. */
errval_t spawn_xcore_monitor(coreid_t coreid, hwid_t hwid,
                             enum cpu_type cpu_type,
                             const char *cmdline,
                             struct frame_identity urpc_frame_id,
                             struct capref kcb)
{
    char cpuname[256], monitorname[256];
    genpaddr_t arch_page_size;
    errval_t err;

    if(cpu_type != CPU_ARM7)
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;

    /* XXX - ignore command line passed in.  Fixing this requires
     * cross-architecture changes. */
    cmdline= NULL;

    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Connect to SKB.");
    }

    arch_page_size= BASE_PAGE_SIZE;

    /* Query the SKB for the CPU driver to use. */
    err= skb_execute_query("arm_core(%d,T), cpu_driver(T,S), write(res(S)).",
                           hwid);
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "skb_execute_query");
        return err;
    }
    err= skb_read_output("res(%255[^)])", cpuname);
    if (err_is_fail(err)) return err;

    /* Query the SKB for the monitor binary to use. */
    err= skb_execute_query("arm_core(%d,T), monitor(T,S), write(res(S)).",
                           hwid);
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "skb_execute_query");
        return err;
    }
    err= skb_read_output("res(%255[^)])", monitorname);
    if (err_is_fail(err)) return err;

    // map cpu and monitor module
    // XXX: caching these for now, until we have unmap
    static struct module_blob cpu_blob, monitor_blob;
    err = module_blob_map(cpuname, &cpu_blob);
    if (err_is_fail(err)) return err;
    err = module_blob_map(monitorname, &monitor_blob);
    if (err_is_fail(err)) return err;

    // Find the CPU driver's relocatable segment.
    struct Elf32_Phdr *rel_phdr=
        elf32_find_segment_type((void *)cpu_blob.vaddr, PT_BF_RELOC);
    if(!rel_phdr) return ELF_ERR_HEADER;

    // Allocate memory for the new core_data struct, and the relocated kernel
    // data segment.
    assert(sizeof(struct arm_core_data) <= arch_page_size);
    struct {
        size_t                size;
        struct capref         cap;
        void                  *buf;
        struct frame_identity frameid;
    } coredata_mem = {
        .size = arch_page_size + rel_phdr->p_memsz,
    };
    err = cpu_memory_prepare(&coredata_mem.size,
                             &coredata_mem.cap,
                             &coredata_mem.buf,
                             &coredata_mem.frameid);
    if (err_is_fail(err)) return err;

    /* Zero the memory. */
    memset(coredata_mem.buf, 0, coredata_mem.size);

    /* The relocated kernel segment will sit one page in. */
    void *rel_seg_buf= coredata_mem.buf + arch_page_size;
    lpaddr_t rel_seg_kvaddr=
        (lpaddr_t)coredata_mem.frameid.base + arch_page_size;

    printf("Allocated %"PRIu64"B for core_data at KV:0x%08"PRIx32"\n",
            arch_page_size, (lpaddr_t)coredata_mem.frameid.base);
    printf("Allocated %"PRIu64"B for CPU driver BSS at KV:0x%08"PRIx32"\n",
            coredata_mem.frameid.bytes - arch_page_size, rel_seg_kvaddr);

    /* Setup the core_data struct in the new kernel */
    struct arm_core_data *core_data = (struct arm_core_data *)coredata_mem.buf;

    // Initialise the KCB and core data, using that of the running kernel.
    err= invoke_kcb_clone(kcb, coredata_mem.cap);
    if(err_is_fail(err)) return err;

    printf("Reusing text segment at KV:0x%08"PRIx32"\n",
           core_data->kernel_load_base);

    // Check that the build ID matches our binary.
    struct Elf32_Shdr *build_id_shdr=
        elf32_find_section_header_name(cpu_blob.vaddr, cpu_blob.size,
                ".note.gnu.build-id");
    if(!build_id_shdr) return ELF_ERR_HEADER;

    // Find the GNU build ID note section
    struct Elf32_Nhdr *build_id_nhdr=
        (struct Elf32_Nhdr *)(cpu_blob.vaddr + build_id_shdr->sh_offset);
    assert(build_id_nhdr->n_type == NT_GNU_BUILD_ID);
    size_t build_id_len= build_id_nhdr->n_descsz;
    const char *build_id_data=
        ((const char *)build_id_nhdr) +
        sizeof(struct Elf32_Nhdr) +
        build_id_nhdr->n_namesz;

    // Check that the binary we're loading matches the kernel we're cloning.
    assert(build_id_len <= MAX_BUILD_ID);
    if(!compare_build_ids(build_id_data,
                          build_id_len,
                          core_data->build_id.data,
                          core_data->build_id.length)) {
        printf("Build ID mismatch: ");
        print_build_id(build_id_data, build_id_len);
        printf(" != ");
        print_build_id(core_data->build_id.data, core_data->build_id.length);
        printf("\n");
        return ELF_ERR_HEADER;
    }

    // Load and relocate the new kernel's relocatable segment
    err= load_cpu_relocatable_segment(
            (void *)cpu_blob.vaddr, rel_seg_buf, rel_seg_kvaddr,
            core_data->kernel_load_base, &core_data->got_base);
    if(err_is_fail(err)) return err;

    /* Chunk of memory to load monitor on the app core */
    struct capref spawn_mem_cap;
    struct frame_identity spawn_mem_frameid;
    err = spawn_memory_prepare(ARM_CORE_DATA_PAGES*arch_page_size,
                               &spawn_mem_cap,
                               &spawn_mem_frameid);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "spawn_memory_prepare");
        return err;
    }

    struct Elf32_Ehdr *head32 = (struct Elf32_Ehdr *)cpu_blob.vaddr;
    core_data->kernel_elf.size = sizeof(struct Elf32_Shdr);
    core_data->kernel_elf.addr = cpu_blob.paddr + (uintptr_t)head32->e_shoff;
    core_data->kernel_elf.num  = head32->e_shnum;

    core_data->kernel_module.mod_start = cpu_blob.paddr;
    core_data->kernel_module.mod_end   = cpu_blob.paddr + cpu_blob.size;

    core_data->urpc_frame_base     = urpc_frame_id.base;
    assert((1UL << log2ceil(urpc_frame_id.bytes)) == urpc_frame_id.bytes);
    core_data->urpc_frame_size     = urpc_frame_id.bytes;

    core_data->monitor_module.mod_start = monitor_blob.paddr;
    core_data->monitor_module.mod_end = monitor_blob.paddr + monitor_blob.size;

    core_data->memory_base_start   = spawn_mem_frameid.base;
    assert((1UL << log2ceil(spawn_mem_frameid.bytes)) == spawn_mem_frameid.bytes);
    core_data->memory_bytes        = spawn_mem_frameid.bytes;
    core_data->src_core_id         = disp_get_core_id();
    core_data->src_arch_id         = my_arch_id;
    core_data->dst_core_id         = coreid;
#ifdef CONFIG_FLOUNDER_BACKEND_UMP_IPI
    core_data->chan_id             = chanid;
#endif
    struct frame_identity fid;
    err = invoke_frame_identify(kcb, &fid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Invoke frame identity for KCB failed. "
                            "Did you add the syscall handler for that architecture?");
    }
    core_data->kcb = (genpaddr_t) fid.base;

    if (cmdline != NULL) {
        // copy as much of command line as will fit
        strncpy(core_data->cmdline_buf, cmdline,
                sizeof(core_data->cmdline_buf));
        // ensure termination
        core_data->cmdline_buf[sizeof(core_data->cmdline_buf) - 1] = '\0';
    }
    core_data->cmdline=
        coredata_mem.frameid.base +
        (lvaddr_t)((void *)core_data->cmdline_buf - (void *)core_data);

    /* Ensure that everything we just wrote is cleaned sufficiently that the
     * target core can read it. */
    sys_armv7_cache_clean_poc((void *)(uint32_t)coredata_mem.frameid.base,
                              (void *)((uint32_t)coredata_mem.frameid.base +
                                       (uint32_t)coredata_mem.frameid.bytes - 1));

    /* Invoke kernel capability to boot new core */
    // XXX: Confusion address translation about l/gen/addr
    err = invoke_monitor_spawn_core(hwid, cpu_type, coredata_mem.frameid.base);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SPAWN_CORE);
    }

    err = cpu_memory_cleanup(coredata_mem.cap, coredata_mem.buf);
    if (err_is_fail(err)) {
        return err;
    }

    err = spawn_memory_cleanup(spawn_mem_cap);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}
