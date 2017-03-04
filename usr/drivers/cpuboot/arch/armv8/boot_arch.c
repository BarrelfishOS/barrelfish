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

extern coreid_t my_arch_id;


static errval_t get_arch_config(hwid_t hwid,
                                genpaddr_t *arch_page_size,
                                const char *monitor_binary,
                                const char *cpu_binary)
{
    errval_t err;

    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Connect to SKB.");
    }

    /* Query the SKB for the CPU driver to use. */
    err= skb_execute_query("cpu_driver(S), write(res(S)).");
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "skb_execute_query");
        return err;
    }
    err= skb_read_output("res(%255[^)])", cpu_binary);
    if (err_is_fail(err)) return err;

    /* Query the SKB for the monitor binary to use. */
    err= skb_execute_query("monitor(S), write(res(S)).");
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "skb_execute_query");
        return err;
    }
    err= skb_read_output("res(%255[^)])", monitor_binary);
    if (err_is_fail(err)) return err;

    *arch_page_size= BASE_PAGE_SIZE;

    return SYS_ERR_OK;
}


struct mem_info {
    size_t                size;
    struct capref         cap;
    void                  *buf;
    struct frame_identity frameid;
};

static errval_t mem_alloc(size_t size, bool map, struct mem_info *mem_info)
{
    errval_t err;

    DEBUG("mem_alloc=%zu bytes\n", size);

    memset(mem_info, 0, sizeof(*mem_info));

    err = frame_alloc(&mem_info->cap, size, &mem_info->size);
    if (err_is_fail(err)) {
        return err;
    }

    err = invoke_frame_identify(mem_info->cap, &mem_info->frameid);
    if (err_is_fail(err)) {
        err =  err_push(err, LIB_ERR_FRAME_IDENTIFY);
        goto out_err;
    }

    if (map) {
        err = vspace_map_one_frame(&mem_info->buf, mem_info->size, mem_info->cap,
                                   NULL, NULL);
        if (err_is_fail(err)) {
            err =  err_push(err, LIB_ERR_VSPACE_MAP);
            goto out_err;
        }
    }

    // Mark memory as remote
    err = cap_mark_remote(mem_info->cap);
    if (err_is_fail(err)) {
        vspace_unmap(mem_info->buf);
        goto out_err;
    }

    return SYS_ERR_OK;

out_err:
    cap_delete(mem_info->cap);
    memset(mem_info, 0, sizeof(*mem_info));
    return err;
}

static errval_t mem_free(struct mem_info *mem_info)
{
    errval_t err;

    if (mem_info->buf) {
        err = vspace_unmap(mem_info->buf);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to unmap\n");
        }
    }
    if (!capref_is_null(mem_info->cap)) {
        err = cap_destroy(mem_info->cap);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to cap destory");
        }
    }

    return SYS_ERR_OK;
}


static errval_t cpu_memory_alloc(size_t size, struct mem_info *mem_info)
{
    return mem_alloc(size, true, mem_info);
}

static errval_t app_memory_alloc(size_t size, struct mem_info *mem_info)
{
    return mem_alloc(size, false, mem_info);
}


struct module_blob {
    size_t             size;    ///< size of the binary in memory
    lvaddr_t           vaddr;   ///< virtual address of the binary in memory
    genpaddr_t         paddr;   ///< physical address of the memory
    struct capref      frame;
    struct mem_region *mem_region;
};

static errval_t
get_module_info(const char *name, struct module_blob *blob)
{
    errval_t err;

    DEBUG("getting module %s\n", name);

    err = lookup_module(name, &blob->vaddr,
                        &blob->paddr, &blob->size);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not lookup module");
        return err_push(err, SPAWN_ERR_FIND_MODULE);
    }

    return SYS_ERR_OK;
}


#include <barrelfish_kpi/sys_debug.h>

static errval_t sys_debug_invoke_psci(uintptr_t target, lpaddr_t entry, lpaddr_t context)
{
    struct sysret sr = syscall5(SYSCALL_DEBUG, DEBUG_PSCI_CPU_ON, target, entry, context);
    return sr.error;
}

errval_t spawn_xcore_monitor(coreid_t coreid, hwid_t hwid,
                             enum cpu_type cpu_type,
                             const char *cmdline,
                             struct frame_identity urpc_frame_id,
                             struct capref kcb)
{

    DEBUG("Booting: %" PRIuCOREID ", hwid=%" PRIxHWID "\n", coreid, hwid);

    static char cpuname[256], monitorname[256];
    genpaddr_t arch_page_size;
    errval_t err;

    if(cpu_type != CPU_ARM8) {
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
    }

    err = get_arch_config(hwid, &arch_page_size, monitorname, cpuname);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to obtain architecture configuration");
    }

    DEBUG("loading kernel: %s\n", cpuname);
    DEBUG("loading 1st app: %s\n", monitorname);

    // compute size of frame needed and allocate it
    DEBUG("%s:%s:%d: urpc_frame_id.base=%"PRIxGENPADDR"\n",
          __FILE__, __FUNCTION__, __LINE__, urpc_frame_id.base);
    DEBUG("%s:%s:%d: urpc_frame_id.size=0x%" PRIuGENSIZE "\n",
          __FILE__, __FUNCTION__, __LINE__, urpc_frame_id.bytes);


    // XXX: Caching these for now, until we have unmap

    struct module_blob cpu_binary;
    err = get_module_info(cpuname, &cpu_binary);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not lookup module");
        return err;
    }

    struct module_blob monitor_binary;
    err = get_module_info(monitorname, &monitor_binary);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not lookup module");
        return err;
    }

    /* */
    struct mem_info cpu_mem;
    err = cpu_memory_alloc(cpu_binary.size, &cpu_mem);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not allocate space for new app kernel.");
        return err;
    }

    assert(monitor_binary.size < ARMV8_CORE_DATA_PAGES * arch_page_size);

    struct mem_info monitor_mem;
    err = app_memory_alloc(ARMV8_CORE_DATA_PAGES * arch_page_size, &monitor_mem);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not allocate space for new app kernel.");
        return err;
    }

#define ARMV8_KERNEL_STACK_SIZE (16 * 1024)

    struct mem_info stack_mem;
    err = app_memory_alloc(ARMV8_KERNEL_STACK_SIZE, &stack_mem);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not allocate space for new app kernel.");
        return err;
    }

    /* Load cpu */
    struct elf_allocate_state state;
    state.vbase = (char *)cpu_mem.buf + arch_page_size;
    assert(sizeof(struct armv8_core_data) <= arch_page_size);
    state.elfbase = elf_virtual_base(cpu_binary.vaddr);

    struct Elf64_Ehdr *cpu_head = (struct Elf64_Ehdr *)cpu_binary.vaddr;
    genvaddr_t cpu_entry;

    err = elf_load(cpu_head->e_machine, elfload_allocate, &state,
                   cpu_binary.vaddr, cpu_binary.size, &cpu_entry);
    if (err_is_fail(err)) {
        return err;
    }


    struct Elf64_Shdr *rela, *symtab, *symhead;
    symhead = (struct Elf64_Shdr *)(cpu_binary.vaddr + (uintptr_t)cpu_head->e_shoff);

    assert(cpu_head->e_shoff != 0);
    rela = elf64_find_section_header_type(symhead, cpu_head->e_shnum, SHT_RELA);
    assert(rela != NULL);
    symtab = elf64_find_section_header_type(symhead, cpu_head->e_shnum, SHT_SYMTAB /* SHT_DYNSYM */);
    assert(symtab != NULL);
    elf64_relocate(cpu_mem.frameid.base + arch_page_size, state.elfbase,
                   (struct Elf64_Rela *)(uintptr_t)(cpu_binary.vaddr + rela->sh_offset),
                   rela->sh_size,
                   (struct Elf64_Sym *)(uintptr_t)(cpu_binary.vaddr + symtab->sh_offset),
                   symtab->sh_size,
                   state.elfbase, state.vbase);

    genvaddr_t cpu_reloc_entry = cpu_entry - state.elfbase
                                 + cpu_mem.frameid.base + arch_page_size;

    struct armv8_core_data *core_data = (struct armv8_core_data *)cpu_mem.buf;

    /* set the stack */
    core_data->kernel_stack = stack_mem.frameid.base + stack_mem.frameid.bytes - 16;
    core_data->boot_magic = ARMV8_BOOTMAGIC_PSCI;

    core_data->elf.size = sizeof(struct Elf64_Shdr);
    core_data->elf.addr = cpu_binary.paddr + (uintptr_t)cpu_head->e_shoff;
    core_data->elf.num  = cpu_head->e_shnum;

    core_data->module_start = cpu_binary.paddr;
    core_data->module_end   = cpu_binary.paddr + cpu_binary.size;
    core_data->urpc_frame_base = urpc_frame_id.base;
    assert((1UL << log2ceil(urpc_frame_id.bytes)) == urpc_frame_id.bytes);
    core_data->urpc_frame_bits = log2ceil(urpc_frame_id.bytes);
    core_data->monitor_binary   = monitor_binary.paddr;
    core_data->monitor_binary_size = monitor_binary.size;
    core_data->memory_base_start = monitor_mem.frameid.base;
    assert((1UL << log2ceil(monitor_mem.frameid.bytes)) == monitor_mem.frameid.bytes);
    core_data->memory_bits       = log2ceil(monitor_mem.frameid.bytes);
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

    /* start */

    debug_printf("invoking PSCI_START hwid=%lx entry=%lx context=%lx\n",
                 hwid, cpu_reloc_entry, cpu_mem.frameid.base);
    err = sys_debug_invoke_psci(hwid, cpu_reloc_entry, cpu_mem.frameid.base);
    DEBUG_ERR(err, "sys_debug_invoke_psci");


    err = mem_free(&stack_mem);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }

    err = mem_free(&cpu_mem);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }
    err = mem_free(&monitor_mem);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }
    return SYS_ERR_OK;
}

errval_t get_core_info(coreid_t core_id, hwid_t* hw_id, enum cpu_type* cpu_type)
{
    char* record = NULL;
    errval_t err = oct_get(&record, "hw.processor.%"PRIuCOREID"", core_id);
    if (err_is_fail(err)) {
        goto out;
    }


    uint64_t enabled, type, barrelfish_id;
    err = oct_read(record, "_ { " HW_PROCESSOR_GENERIC_FIELDS " }",
                   &enabled, &barrelfish_id, hw_id, &type);
    if (err_is_fail(err)) {
        goto out;
    }

    debug_printf("Get Core Info: %" PRIuCOREID ", hwid=%" PRIxHWID "\n", core_id, *hw_id);

    if (!enabled) {
        /* XXX: better error code */
        err = SYS_ERR_CORE_NOT_FOUND;
    }


    *cpu_type = (enum cpu_type) type;
out:
    if (record) {
        free(record);
    }
    return err;
}
