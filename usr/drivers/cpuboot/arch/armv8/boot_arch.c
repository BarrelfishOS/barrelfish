/**
 * \file
 * \brief Boot driver arch specific parts for ARM CPUs
 */
/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */



#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/platform.h>
#include <barrelfish/syscall_arch.h>
#include <target/aarch64/barrelfish_kpi/arm_core_data.h>

#include <barrelfish/deferred.h>

#include <hw_records_arch.h>

#include <skb/skb.h>


#include "../../coreboot.h"

#define ARMV8_MONITOR_NAME "/" BF_BINARY_PREFIX "armv8/sbin/monitor"

volatile uint64_t *ap_dispatch;
extern coreid_t my_arch_id;
extern struct capref ipi_cap;
extern uint64_t end;


errval_t get_architecture_config(enum cpu_type type,
                                 genpaddr_t *arch_page_size,
                                 const char **monitor_binary,
                                 const char **cpu_binary)
{
    errval_t err;

    struct monitor_blocking_rpc_client *m = get_monitor_blocking_rpc_client();
    assert(m != NULL);

    uint32_t arch, platform;
    err = m->vtbl.get_platform(m, &arch, &platform);
    if (err_is_fail(err)) {
        return err;
    }
    assert(arch == PI_ARCH_ARMV8A);

    switch(platform) {
    case PI_PLATFORM_FVP:
        *cpu_binary = "/" BF_BINARY_PREFIX "armv8/sbin/cpu_a53v";
        break;
    case PI_PLATFORM_APM88XXXX:
        *cpu_binary = "/" BF_BINARY_PREFIX "armv8/sbin/cpu_apm88xxxx";
        break;
    case PI_PLATFORM_CN88XX:
        *cpu_binary = "/" BF_BINARY_PREFIX "armv8/sbin/cpu_cn88xx";
        break;
    default:
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
    }

    *monitor_binary = ARMV8_MONITOR_NAME;
    *arch_page_size = BASE_PAGE_SIZE;

    return SYS_ERR_OK;
}

static int start_aps_armv8_start(uint8_t core_id, genvaddr_t entry)
{
    return 1;
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

    DEBUG("loading kernel: %s\n", cpuname);
    DEBUG("loading 1st app: %s\n", monitorname);

    DEBUG("%s:%s:%d: urpc_frame_id.base=%"PRIxGENPADDR"\n",
           __FILE__, __FUNCTION__, __LINE__, urpc_frame_id.base);
    DEBUG("%s:%s:%d: urpc_frame_id.size=0x%" PRIuGENSIZE "\n",
           __FILE__, __FUNCTION__, __LINE__, urpc_frame_id.bytes);

    static size_t cpu_binary_size;
    static lvaddr_t cpu_binary = 0;
    static genpaddr_t cpu_binary_phys;
    static const char* cached_cpuname = NULL;
    if (cpu_binary == 0) {
        cached_cpuname = cpuname;
        // XXX: Caching these for now, until we have unmap
        err = lookup_module(cpuname, &cpu_binary, &cpu_binary_phys,
                            &cpu_binary_size);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Can not lookup module");
            return err;
        }
    }
    // Ensure caching actually works and we're
    // always loading same binary. If this starts to fail, get rid of caching.
    assert (strcmp(cached_cpuname, cpuname) == 0);

    static size_t monitor_binary_size;
    static lvaddr_t monitor_binary = 0;
    static genpaddr_t monitor_binary_phys;
    static const char* cached_monitorname = NULL;
    if (monitor_binary == 0) {
        cached_monitorname = monitorname;
        // XXX: Caching these for now, until we have unmap
        err = lookup_module(monitorname, &monitor_binary,
                            &monitor_binary_phys, &monitor_binary_size);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Can not lookup module");
            return err;
        }
    }

    struct capref cpu_memory_cap;
    struct frame_identity frameid;

    lvaddr_t cpu_memory = elf_virtual_size(cpu_binary) + arch_page_size;

    err =  frame_alloc_identify(&cpu_memory_cap, cpu_memory, &cpu_memory, &frameid);
    if (err_is_fail(err)) {
        /* XXX: clean up the modules */
        DEBUG_ERR(err, "Can not allocate space for new app kernel.");
        return err;
    }

    err = cap_mark_remote(cpu_memory_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not mark cap remote.");
        return err;
    }

    void *cpu_buf_memory;
    err = vspace_map_one_frame(&cpu_buf_memory, cpu_memory, cpu_memory_cap,
                               NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    /* Chunk of memory to load monitor on the app core */
    struct capref spawn_memory_cap;
    struct frame_identity spawn_memory_identity;

    err = frame_alloc_identify(&spawn_memory_cap,
                               ARMV8_CORE_DATA_PAGES * arch_page_size,
                               NULL, &spawn_memory_identity);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_FRAME_ALLOC);
    }

    err = cap_mark_remote(spawn_memory_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not mark cap remote.");
        return err;
    }

    struct elf_allocate_state state;
    state.vbase = (char *)cpu_buf_memory + arch_page_size;
    assert(sizeof(struct armv8_core_data) <= arch_page_size);
    state.elfbase = elf_virtual_base(cpu_binary);

    struct Elf64_Ehdr *cpu_head = (struct Elf64_Ehdr *)cpu_binary;
    genvaddr_t cpu_entry;

    err = elf_load(cpu_head->e_machine, elfload_allocate, &state,
                   cpu_binary, cpu_binary_size, &cpu_entry);
    if (err_is_fail(err)) {
        return err;
    }

    struct Elf64_Shdr *rela, *symtab, *symhead =
        (struct Elf64_Shdr *)(cpu_binary + (uintptr_t)cpu_head->e_shoff);

    assert(cpu_head->e_shoff != 0);
    rela = elf64_find_section_header_type(symhead, cpu_head->e_shnum, SHT_RELA);
    assert(rela != NULL);
    symtab = elf64_find_section_header_type(symhead, cpu_head->e_shnum, SHT_DYNSYM);
    assert(symtab != NULL);
    elf64_relocate(frameid.base + arch_page_size, state.elfbase,
                   (struct Elf64_Rela *)(uintptr_t)(cpu_binary + rela->sh_offset),
                   rela->sh_size,
                   (struct Elf64_Sym *)(uintptr_t)(cpu_binary + symtab->sh_offset),
                   symtab->sh_size,
                   state.elfbase, state.vbase);


    genvaddr_t cpu_reloc_entry = cpu_entry - state.elfbase
                                 + frameid.base + arch_page_size;

    /* Compute entry point in the foreign address space */
    forvaddr_t foreign_cpu_reloc_entry = (forvaddr_t)cpu_reloc_entry;

    /* Setup the core_data struct in the new kernel */
    struct armv8_core_data *core_data = (struct armv8_core_data *)cpu_buf_memory;
    switch (cpu_head->e_machine) {
    case EM_AARCH64:
        core_data->elf.size = sizeof(struct Elf64_Shdr);
        core_data->elf.addr = cpu_binary_phys + (uintptr_t)cpu_head->e_shoff;
        core_data->elf.num  = cpu_head->e_shnum;
        break;
    default:
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
    }
    core_data->module_start = cpu_binary_phys;
    core_data->module_end   = cpu_binary_phys + cpu_binary_size;
    core_data->urpc_frame_base = urpc_frame_id.base;
    assert((1UL << log2ceil(urpc_frame_id.bytes)) == urpc_frame_id.bytes);
    core_data->urpc_frame_bits = log2ceil(urpc_frame_id.bytes);
    core_data->monitor_binary   = monitor_binary_phys;
    core_data->monitor_binary_size = monitor_binary_size;
    core_data->memory_base_start = spawn_memory_identity.base;
    assert((1UL << log2ceil(spawn_memory_identity.bytes)) == spawn_memory_identity.bytes);
    core_data->memory_bits       = log2ceil(spawn_memory_identity.bytes);
    core_data->src_core_id       = disp_get_core_id();
    core_data->src_arch_id       = my_arch_id;
    core_data->dst_core_id       = coreid;


    struct frame_identity fid;
    err = invoke_frame_identify(kcb, &fid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Invoke frame identity for KCB failed. "
                            "Did you add the syscall handler for that architecture?");
    }
    DEBUG("%s:%s:%d: fid.base is 0x%"PRIxGENPADDR"\n",
           __FILE__, __FUNCTION__, __LINE__, fid.base);
    core_data->kcb = (genpaddr_t) fid.base;
#ifdef CONFIG_FLOUNDER_BACKEND_UMP_IPI
    core_data->chan_id           = chanid;
#endif

    if (cmdline != NULL) {
        // copy as much of command line as will fit
        snprintf(core_data->kernel_cmdline, sizeof(core_data->kernel_cmdline),
                "%s %s", cpuname, cmdline);
        // ensure termination
        core_data->kernel_cmdline[sizeof(core_data->kernel_cmdline) - 1] = '\0';

        DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, core_data->kernel_cmdline);
    }


    /* Invoke kernel capability to boot new core */
    start_aps_armv8_start(hwid, foreign_cpu_reloc_entry);


    // XXX: Should not delete the remote caps?
    err = cap_destroy(spawn_memory_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }
    err = vspace_unmap(cpu_buf_memory);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vspace unmap CPU driver memory failed");
    }
    err = cap_destroy(cpu_memory_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }

    debug_printf("WARNING: spawn_xcore_monitor currently not implemented!");
    return LIB_ERR_NOT_IMPLEMENTED;
}

errval_t get_core_info(coreid_t core_id, archid_t* hw_id, enum cpu_type* cpu_type)
{
    char* record = NULL;
    errval_t err = oct_get(&record, "hw.processor.%"PRIuCOREID"", core_id);
    if (err_is_fail(err)) {
        goto out;
    }

    /* XXX: figure out which fields are required */
    uint64_t apic, enabled, type;
    err = oct_read(record, "_ { apic_id: %d, enabled: %d, type: %d}",
                   &apic, &enabled, &type);
    assert (enabled);
    if (err_is_fail(err)) {
        goto out;
    }

    *hw_id = (archid_t) apic;
    *cpu_type = (enum cpu_type) type;
out:
    return err;
}
