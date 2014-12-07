/**
 * \file
 * \brief Boot driver arch specific parts for ARM CPUs
 */
/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "coreboot.h"

#include <barrelfish_kpi/paging_arch.h>
#include <target/arm/barrelfish_kpi/arm_core_data.h>


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
                       archid_t* apic_id, 
                       enum cpu_type* cpu_type) {

    *apic_id = core_id;
    *cpu_type = CPU_ARM7;
    return SYS_ERR_OK;
}

errval_t get_architecture_config(enum cpu_type type,
                                genpaddr_t *arch_page_size,
                                const char **monitor_binary,
                                const char **cpu_binary)
{
    extern char* cmd_monitor_binary;
    switch (type) {

    case CPU_ARM7:
    {
        *arch_page_size = BASE_PAGE_SIZE;
        *monitor_binary = (cmd_monitor_binary == NULL) ?
                          "/" BF_BINARY_PREFIX "armv7/sbin/monitor" :
                          get_binary_path("/" BF_BINARY_PREFIX "armv7/sbin/%s", 
                                          cmd_monitor_binary);
// TODO: That should not be static
#if defined(__gem5__)
        extern char* cmd_kernel_binary;
        *cpu_binary = (cmd_kernel_binary == NULL) ?
                      "/" BF_BINARY_PREFIX "armv7/sbin/cpu_arm_gem5" :
                      get_binary_path("/" BF_BINARY_PREFIX "armv7/sbin/%s", 
                                      cmd_kernel_binary);
#elif defined(__pandaboard__)
        extern char* cmd_kernel_binary;
        *cpu_binary = (cmd_kernel_binary == NULL) ?
                      "/" BF_BINARY_PREFIX "armv7/sbin/cpu_omap44xx" :
                      get_binary_path("/" BF_BINARY_PREFIX "armv7/sbin/%s", 
                                      cmd_kernel_binary);
#else
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
#endif
    }

    default:
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
    }
    DEBUG("Selected CPU binary %s\n", *cpu_binary);

    return SYS_ERR_OK;
}

static errval_t monitor_elfload_allocate(void *state, genvaddr_t base,
                                         size_t size, uint32_t flags,
                                         void **retbase)
{
    struct monitor_allocate_state *s = state;

    *retbase = (char *)s->vbase + base - s->elfbase;
    return SYS_ERR_OK;
}

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
    uint8_t invoke_bits = get_cap_valid_bits(ipi_cap);
    capaddr_t invoke_cptr = get_cap_addr(ipi_cap) >> (CPTR_BITS - invoke_bits);

    return syscall6((invoke_bits << 16) | (KernelCmd_Spawn_core << 8)
                    | SYSCALL_INVOKE, invoke_cptr, core_id, cpu_type,
                    (uintptr_t)(entry >> 32), (uintptr_t) entry).error;
}

errval_t spawn_xcore_monitor(coreid_t coreid, int hwid, 
                             enum cpu_type cpu_type,
                             const char *cmdline,
                             struct frame_identity urpc_frame_id,
                             struct capref kcb)
{
    const char *monitorname = NULL, *cpuname = NULL;
    genpaddr_t arch_page_size;
    errval_t err;

    err = get_architecture_config(cpu_type, &arch_page_size,
                                  &monitorname, &cpuname);

    // map cpu and monitor module
    // XXX: caching these for now, until we have unmap
    static struct module_blob cpu_blob, monitor_blob;
    err = module_blob_map(cpuname, &cpu_blob);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "module_blob_map");
        return err;
    }
    err = module_blob_map(monitorname, &monitor_blob);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "module_blob_map");
        return err;
    }

    // allocate memory for cpu driver: we allocate a page for arm_core_data and
    // the reset for the elf image
    assert(sizeof(struct arm_core_data) <= arch_page_size);
    struct {
        size_t                size;
        struct capref         cap;
        void                  *buf;
        struct frame_identity frameid;
    } cpu_mem = {
        .size = arch_page_size + elf_virtual_size(cpu_blob.vaddr)
    };
    err = cpu_memory_prepare(&cpu_mem.size,
                             &cpu_mem.cap,
                             &cpu_mem.buf,
                             &cpu_mem.frameid);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cpu_memory_prepare");
        return err;
    }

    // Load cpu driver to the allocate space and do relocatation
    uintptr_t reloc_entry;
    err = elf_load_and_relocate(cpu_blob.vaddr,
                                cpu_blob.size,
                                cpu_mem.buf + arch_page_size,
                                cpu_mem.frameid.base + arch_page_size,
                                &reloc_entry);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cpu_memory_prepare");
        return err;
    }

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

    /* Setup the core_data struct in the new kernel */
    struct arm_core_data *core_data = (struct arm_core_data *)cpu_mem.buf;

    struct Elf32_Ehdr *head32 = (struct Elf32_Ehdr *)cpu_blob.vaddr;
    core_data->elf.size = sizeof(struct Elf32_Shdr);
    core_data->elf.addr = cpu_blob.paddr + (uintptr_t)head32->e_shoff;
    core_data->elf.num  = head32->e_shnum;

    core_data->module_start        = cpu_blob.paddr;
    core_data->module_end          = cpu_blob.paddr + cpu_blob.size;
    core_data->urpc_frame_base     = urpc_frame_id.base;
    core_data->urpc_frame_bits     = urpc_frame_id.bits;
    core_data->monitor_binary      = monitor_blob.paddr;
    core_data->monitor_binary_size = monitor_blob.size;
    core_data->memory_base_start   = spawn_mem_frameid.base;
    core_data->memory_bits         = spawn_mem_frameid.bits;
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
        strncpy(core_data->kernel_cmdline, cmdline,
                sizeof(core_data->kernel_cmdline));
        // ensure termination
        core_data->kernel_cmdline[sizeof(core_data->kernel_cmdline) - 1] = '\0';
    }

    /* Invoke kernel capability to boot new core */
    // XXX: Confusion address translation about l/gen/addr
    err = invoke_monitor_spawn_core(hwid, cpu_type, (forvaddr_t)reloc_entry);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SPAWN_CORE);
    }

    err = cpu_memory_cleanup(cpu_mem.cap, cpu_mem.buf);
    if (err_is_fail(err)) {
        return err;
    }

    err = spawn_memory_cleanup(spawn_mem_cap);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}
